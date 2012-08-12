module Elb.Syntax where

import Data.Set (Set, \\)
import qualified Data.Set as Set
import Language.Haskell.TH

import Elb.InvFun

patternDirectVariables :: Pat -> Set Name
patternDirectVariables (VarP n) = Set.fromList [n]
patternDirectVariables (AsP n _) = Set.fromList [n]
patternDirectVariables _ = Set.empty

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

patternVariables :: Pat -> Set Name
patternVariables p =
  Set.unions (patternDirectVariables p : map (patternVariables . subpatterns) p)

variablesPattern :: Set Name -> Pat
variablesPattern names = 
  case Set.toAscList names of
    [n] -> VarP n
    ns -> TupP ns

variablesExpr :: Set Name -> Exp
variablesExpr names =
  case Set.toAscList names of
    [n] -> VarE n
    ns -> TupE ns

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

distributionFunction :: Set Name -> Exp -> Q Exp
distributionFunction vars exp =
  [| \$(return $ variablesPattern vars) -> $(return exp) |]

scopePlusPattern :: Set Name -> Pat -> Q Exp
scopePlusPattern vars pat =
  [| \($(return $ variablesPattern vars), $(return pat)) ->
       $(return $ variablesExpr (Set.union vars $ patternVariables pat)) |]

scopeMinusPattern :: Set Name -> Pat -> Q Exp
scopeMinusPattern vars pat =
  [| \$(return $ variablesPattern (Set.union vars $ patternVariables pat)) ->
       ($(return $ variablesExpr vars), $(return $ patternExpr pat)) |]

translateStatement :: Set Name -> Stmt -> (Q Exp, Set Name)
translateStatement vars (BindS pat (InfixE (Just fun) (VarE op) (Just arg)))
  | show op == "-<" =
    ([| $(scopeMinusPattern commonVars argPattern) `Compose`
        Subcall $(distributionFunction commonVars fun) `Compose`
        $(scopePlusPattern commonVars pat) |],
     commonVars ++ patternVariables pat)
    where argPattern = exprPattern arg
          commonVars = vars \\ patternVariables argPattern

translateStatement vars (BindS pat expr) = do
  rhs <- [| $(return expr) -< () |]
  translateStatement vars (BindS pat rhs)
translateStatement vars (NoBindS inf) = 
  translateStatement vars (BindS (TupP []) inf)

translateStatements :: Set Name -> [Stmt] -> Q Exp
translateStatements vars [NoBindS ret] 
  | vars != retVars = error "Not all variables were returned"
  | otherwise = [| Compose $(scopeMinusPattern vars retPat) 
                   (Pure (\((), res) -> res) (\res -> ((), res))) |]
  where retPat = exprPattern ret
        retVars = patternVariables retPat
translateStatements vars (stmt:rest) = do
  (exp, newVars) <- translateStatement vars stmt
  [| Compose $(return exp) $(translateStatements newVars rest) |]

translateDo :: Exp -> Q Exp
translateDo (LamE [argPat] (DoE stmts)) =
  [| Pure (\a -> ((), a)) (\((), a) -> a) `Compose`
     $(scopePlusPattern Set.empty argPat) `Compose`
     $(translateStatements (patternVariables argPat) stmts) |]
translateDo (DoE stmts) = translateDo (LamE (TupP []) (DoE stmts))
