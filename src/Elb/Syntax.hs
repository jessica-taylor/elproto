module Elb.Syntax (distr) where

import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Language.Haskell.Exts.Parser (
  parseExp, ParseResult(ParseOk, ParseFailed))
import Language.Haskell.TH

import Elb.InvFun

-- Gets variables in a pattern (but not in sub-patterns).
patternDirectVariables :: Pat -> Set Name
patternDirectVariables (VarP n) = Set.fromList [n]
patternDirectVariables (AsP n _) = Set.fromList [n]
patternDirectVariables _ = Set.empty

-- Gets subpatterns of a pattern.
subpatterns :: Pat -> [Pat]
subpatterns (TupP pats) = pats
subpatterns (ConP _ pats) = pats
subpatterns (InfixP a _ b) = [a, b]
subpatterns (TildeP pat) = [pat]
subpatterns (BangP pat) = [pat]
subpatterns (AsP _ pat) = [pat]
subpatterns (RecP _ fields) = map snd fields
subpatterns (ListP pats) = pats
subpatterns (SigP pat _) = [pat]
subpatterns (ViewP _ pat) = [pat] 
subpatterns _ = []

-- Gets a set of variables in a pattern.
patternVariables :: Pat -> Set Name
patternVariables p =
  Set.unions (patternDirectVariables p : map (patternVariables . subpatterns) p)

-- Gets a pattern for a tuple of the given variables in sorted order.
variablesPattern :: Set Name -> Pat
variablesPattern names = 
  case Set.toAscList names of
    [n] -> VarP n
    ns -> TupP ns

-- Gets an expression for a tuple of the given variables in sorted order.
variablesExpr :: Set Name -> Exp
variablesExpr names =
  case Set.toAscList names of
    [n] -> VarE n
    ns -> TupE ns

-- Converts a pattern to an expression.
patternExpr :: Pat -> Exp
patternExpr (LitP lit) = LitE lit
patternExpr (VarP name) VarE name
patternExpr (TupP pats) = TupE (map patternExpr pats)
patternExpr (ConP name pats) = foldl AppE (ConE name) (map patternExpr pats)
patternExpr (InfixP a name b) = 
  InfixE (Just (patternExpr a)) (ConE name) (Just (patternExpr b))
patternExpr (TildeP pat) = patternExpr pat
patternExpr (BangP pat) = patternExpr pat
patternExpr (AsP name _) = VarE name
patternExpr (RecP name fields) = 
  RecConE name [(n, patternExpr p) | (n, p) <- fields]
patternExpr (ListP pats) = ListE (map patternExpr pats)
patternExpr (SigP pat typ) = SigE (patternExpr pat) typ
patternExpr other = error ("Pattern not allowed: " ++ show other)

-- Converts an expression to a pattern.
exprPattern :: Exp -> Pat
exprPattern (LitE lit) = LitP lit
exprPattern (VarE name) = VarP name
exprPattern (TupE exprs) = TupP (map exprPattern exprs)
exprPattern (ConE name) = ConP name []
exprPattern (AppE fun arg) = case exprPattern fun of
  ConP name [args] -> ConP name ([args] ++ [exprPattern arg])
  _ -> error "Cannot have application other than constructor"
exprPattern (RecConE name fields) =
  RecConP name [(n, exprPattern e) | (n, e) <- fields]
exprPattern (ListE exprs) = ListP (map exprPattern exprs)
exprPattern (SigE expr typ) = SigP (exprPattern expr) typ
exprPattern other = error ("Expr is not a pattern: " ++ show other)

-- Produces a function
subcallFunction :: Set Name -> Exp -> Q Exp
subcallFunction vars exp =
  [| \$(return $ variablesPattern vars) -> $(return exp) |]

scopePlusPattern :: Set Name -> Pat -> Q Exp
scopePlusPattern vars pat =
  [| \($(return $ variablesPattern vars), $(return pat)) ->
       $(return $ variablesExpr (Set.union vars $ patternVariables pat)) |]

scopeMinusPattern :: Set Name -> Pat -> Q Exp
scopeMinusPattern vars pat =
  [| \$(return $ variablesPattern (Set.union vars $ patternVariables pat)) ->
       ($(return $ variablesExpr vars), $(return $ patternExpr pat)) |]

invScopePlusPattern :: Set Name -> Pat -> Q Exp
invScopePlusPattern vars pat =
  [| Pure (errorless $(scopePlusPattern vars pat) 
                     $(scopeMinusPattern vars pat)) |]

invScopeMinusPattern :: Set Name -> Pat -> Q Exp
invScopeMinusPattern vars pat =
  [| Pure (errorless $(scopeMinusPattern vars pat) 
                     $(scopePlusPattern vars pat)) |]

translateStatement :: Set Name -> Stmt -> (Q Exp, Set Name)
translateStatement vars (BindS pat (InfixE (Just fun) (VarE op) (Just arg)))
  | show op == "-<" =
    ([| $(invScopeMinusPattern commonVars argPattern) `Compose`
        Subcall $(subcallFunction commonVars fun) `Compose`
        $(invScopePlusPattern commonVars pat) |],
     commonVars ++ patternVariables pat)
    where argPattern = exprPattern arg
          commonVars = vars \\ patternVariables argPattern
translateStatement vars (BindS pat expr) = do
  rhs <- [| $(return expr) -< () |]
  translateStatement vars (BindS pat rhs)
translateStatement vars (NoBindS inf) = 
  translateStatement vars (BindS (TupP []) inf)

returnPattern :: Set Name -> Pat -> Q Exp
returnPattern vars pat =
  if patternVariables pat != vars
    then error "Not all variables were returned"
    else [| Compose $(invScopeMinusPattern vars retPat)
            (Pure (errorless (\((), res) -> res) (\res -> ((), res)))) |]

translateStatements :: Set Name -> [Stmt] -> Q Exp
translateStatements vars [NoBindS (InfixE (Just fun) (VarE op) (Just arg))]
  | show op == "-<" = [| Compose $(returnPattern vars (exprPattern arg))
                         $(return fun) |]
translateStatements vars [NoBindS ret] = returnPattern vars (exprPattern ret)
translateStatements vars (stmt:rest) = do
  (exp, newVars) <- translateStatement vars stmt
  [| Compose $(return exp) $(translateStatements newVars rest) |]

translateDo :: Exp -> Q Exp
translateDo (LamE [argPat] (DoE stmts)) =
  [| Pure (\a -> ((), a)) (\((), a) -> a) `Compose`
     $(invScopePlusPattern Set.empty argPat) `Compose`
     $(translateStatements (patternVariables argPat) stmts) |]
translateDo (DoE stmts) = translateDo (LamE (TupP []) (DoE stmts))

parseDistr :: String -> Q Exp
parseDistr str = case parseExp str of
  ParseOk exp -> translateDo exp
  ParseFailed _ msg -> fail msg

distr :: QuasiQuoter
distr = QuasiQuoter { quoteExp = parseDistr }
