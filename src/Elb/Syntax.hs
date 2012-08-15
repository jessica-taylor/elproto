{-# LANGUAGE TemplateHaskell #-}
module Elb.Syntax (distr, (-<)) where

import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Language.Haskell.TH

import Elb.InvFun
import Elb.PureInvFun (errorless)

infixr 0 -<
(-<) :: InvFun a b -> a -> InvFun () b
(-<) = error "Use of -< outside distr code"

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
  Set.unions (patternDirectVariables p : map patternVariables (subpatterns p))

-- Gets a pattern for a tuple of the given variables in sorted order.
variablesPattern :: Set Name -> Pat
variablesPattern names = 
  case Set.toAscList names of
    [n] -> VarP n
    ns -> TupP (map VarP ns)

-- Gets an expression for a tuple of the given variables in sorted order.
variablesExpr :: Set Name -> Exp
variablesExpr names =
  case Set.toAscList names of
    [n] -> VarE n
    ns -> TupE (map VarE ns)

-- Converts a pattern to an expression.
patternExpr :: Pat -> Exp
patternExpr (LitP lit) = LitE lit
patternExpr (VarP name) = VarE name
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
  RecP name [(n, exprPattern e) | (n, e) <- fields]
exprPattern (ListE exprs) = ListP (map exprPattern exprs)
exprPattern (SigE expr typ) = SigP (exprPattern expr) typ
exprPattern other = error ("Expr is not a pattern: " ++ show other)

-- Produces a function
subcallFunction :: Set Name -> Exp -> Q Exp
subcallFunction vars exp = return $ LamE [variablesPattern vars] exp

scopePlusPattern :: Set Name -> Pat -> Q Exp
scopePlusPattern vars pat = return $
  LamE [TupP [variablesPattern vars, pat]] $
    variablesExpr (Set.union vars $ patternVariables pat)

scopeMinusPattern :: Set Name -> Pat -> Q Exp
scopeMinusPattern vars pat = return $
  LamE [variablesPattern (Set.union vars $ patternVariables pat)] $
    TupE [variablesExpr vars, patternExpr pat]

invScopePlusPattern :: Set Name -> Pat -> Q Exp
invScopePlusPattern vars pat =
  [| Pure (errorless $(scopePlusPattern vars pat) 
                     $(scopeMinusPattern vars pat)) |]

invScopeMinusPattern :: Set Name -> Pat -> Q Exp
invScopeMinusPattern vars pat =
  [| Pure (errorless $(scopeMinusPattern vars pat) 
                     $(scopePlusPattern vars pat)) |]

translateAssignmentCall :: Set Name -> Pat -> Exp -> Pat -> Q (Exp, Set Name)
translateAssignmentCall vars pat fun arg = do
  let commonVars = vars \\ patternVariables arg
  exp <- [| $(invScopeMinusPattern commonVars arg) `Compose`
            Subcall $(subcallFunction commonVars fun) `Compose`
            $(invScopePlusPattern commonVars pat) |]
  return (exp, Set.union commonVars (patternVariables pat))

translateStatement :: Set Name -> Stmt -> Q (Exp, Set Name)
translateStatement vars (BindS pat (InfixE (Just fun) (VarE op) (Just arg)))
  | nameBase op == "-<" = translateAssignmentCall vars pat fun (exprPattern arg)
translateStatement vars (BindS pat fun) =
  translateAssignmentCall vars pat fun (TupP [])
translateStatement vars (NoBindS (InfixE (Just fun) (VarE op) (Just arg)))
  | nameBase op == "-<" = 
    translateAssignmentCall vars (TupP []) fun (exprPattern arg)

returnPattern :: Set Name -> Pat -> Q Exp
returnPattern vars pat =
  if patternVariables pat /= vars
    then error "Not all variables were returned"
    else [| Compose $(invScopeMinusPattern Set.empty pat)
            (Pure (errorless (\((), res) -> res) (\res -> ((), res)))) |]

translateStatements :: Set Name -> [Stmt] -> Q Exp
translateStatements vars [NoBindS (InfixE (Just fun) (VarE op) (Just arg))]
  | nameBase op == "-<" = [| Compose $(returnPattern vars (exprPattern arg))
                             $(return fun) |]
translateStatements vars [NoBindS (AppE (VarE return') ret)]
  | nameBase return' == "return" = returnPattern vars (exprPattern ret)
translateStatements vars [x] = error ("Bad final statement: " ++ show x)
translateStatements vars (stmt:rest) = do
  (exp, newVars) <- translateStatement vars stmt
  [| Compose $(return exp) $(translateStatements newVars rest) |]

translateDo :: Exp -> Q Exp
translateDo (LamE [argPat] (DoE stmts)) =
  [| Pure (errorless (\a -> ((), a)) (\((), a) -> a)) `Compose`
     $(invScopePlusPattern Set.empty argPat) `Compose`
     $(translateStatements (patternVariables argPat) stmts) |]
translateDo (DoE stmts) = translateDo (LamE [TupP []] (DoE stmts))

distr :: Q Exp -> Q Exp
distr = (>>= translateDo)
