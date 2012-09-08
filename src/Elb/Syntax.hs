{-# LANGUAGE TemplateHaskell #-}
module Elb.Syntax (distr, (-<)) where

import Control.Monad (liftM)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Language.Haskell.TH

import Elb.InvFun
import Elb.PureInvFun (errorless)

{-

TODO(jacobt):
make this report a helpful error message:

dirichletHelper :: Int -> InvFun [Int] [Int]
dirichletHelper scale = $(distr [|\elems -> do
  first <- constI 1
  dirichletHelper first -< (first:elems)
  |])

problem: first is not allowed to be used in the last line
-}

-- TODO(mario) Figure out if this is normal tempate Haskell.
-- TODO(mario) Document the smash operator.
-- |'-<' is the smash operator.
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
  ConP name args -> ConP name (args ++ [exprPattern arg])
  other -> error ("Cannot have application other than constructor: " ++
                  pprint (AppE fun arg))
exprPattern (InfixE (Just lhs) (ConE name) (Just rhs)) =
  ConP name $ map exprPattern [lhs, rhs]
exprPattern (RecConE name fields) =
  RecP name [(n, exprPattern e) | (n, e) <- fields]
exprPattern (ListE exprs) = ListP (map exprPattern exprs)
exprPattern (SigE expr typ) = SigP (exprPattern expr) typ
exprPattern other = error ("Expr is not a pattern: " ++ show other)

-- Produces a function
subcallFunction :: Set Name -> Exp -> Q Exp
subcallFunction vars exp = return $ LamE [variablesPattern vars] exp

patternInv :: Pat -> Pat -> Q Exp
patternInv lhs rhs =
  [| Pure (errorless $(return $ LamE [lhs] (patternExpr rhs))
                     $(return $ LamE [rhs] (patternExpr lhs))) |]

lambdaInv :: Q Exp -> Q Exp
lambdaInv lam = do
  lamExpr@(LamE [arg] res) <- lam
  [| Pure (errorless $(return lamExpr)
                     $(return (LamE [exprPattern res] (patternExpr arg)))) |]

invScopePlusPattern :: Set Name -> Pat -> Q Exp
invScopePlusPattern vars pat =
  patternInv (TupP [variablesPattern vars, pat])
             (variablesPattern (Set.union vars $ patternVariables pat))

translateInvertibleMatch :: [(Pat, Pat)] -> Q Exp
translateInvertibleMatch pairs = 
  [| Pure $ errorless $(makeCase doPairs) $(makeCase undoPairs) |]
  where makeMatches = map $ \(pat, exp) -> Match pat (NormalB exp) []
        doPairs = map (\(pat, expPat) -> (pat, patternExpr expPat)) pairs
        undoPairs = map (\(expPat, pat) -> (pat, patternExpr expPat)) pairs
        makeCase pairs = [|\value -> $(do
          valueExp <- [| value |]
          return $ CaseE valueExp (makeMatches pairs)
          )|]


translateTryPattern :: Set Name -> Pat -> Q Exp
translateTryPattern commonVars pat = do
  let patternVarsExpr = variablesExpr (patternVariables pat) 
  successExpr <- [| (True, Right $(return patternVarsExpr)) |]
  valueName <- newName "value"
  failExpr <- [| (False, Left $(varE valueName)) |]
  [| Subcall $(lamE [return $ variablesPattern commonVars] [|
      $(translateInvertibleMatch [(pat, exprPattern successExpr),
                                  (VarP valueName, exprPattern failExpr)])|])
     `Compose` $(lambdaInv [| (\(a, (b, c)) -> (b, (a, c))) |]) |]

combineVariables :: Set Name -> Set Name -> Q Exp
combineVariables left right = 
  patternInv (TupP [variablesPattern left, variablesPattern right])
             (variablesPattern (Set.union left right))

getLeft :: Q Exp
getLeft = lambdaInv [| \(a, Left x) -> (a, x) |] 

getRight :: Q Exp
getRight = lambdaInv [| \(a, Right x) -> (a, x) |] 

translateSplitCase :: Set Name -> [(Pat, Exp, Pat)] -> Q Exp
translateSplitCase _ [] = [| error "pattern match not exhaustive" |]
translateSplitCase commonVars ((pat, body, endpat):rest) =
  [| $(translateTryPattern commonVars pat) `Compose`
     Subcall (\success ->
       if success 
       then $getRight `Compose`
            $(combineVariables commonVars (patternVariables pat)) `Compose`
            $(return body) `Compose`
            Undo $(combineVariables commonVars (patternVariables endpat))
            `Compose` Undo $getRight
       else $getLeft `Compose` $(translateSplitCase commonVars rest) `Compose`
            Undo $getLeft)
    `Compose` Undo $(translateTryPattern commonVars endpat) |]

splitMatch :: Set Name -> Match -> Q (Pat, Exp, Pat, Set Name)
splitMatch commonVars (Match pat (NormalB exp) []) = do
  let vars = Set.union commonVars (patternVariables pat)
  (exp, retExp, newVars) <- 
    case exp of
      AppE (VarE returnI') ret | nameBase returnI' == "returnI" -> do
        idExp <- [| Pure $ errorless id id |]
        return (idExp, ret, vars)
      DoE stmts -> do
        (exp, vars') <- translateMiddleStatements vars (init stmts)
        case last stmts of
          NoBindS (AppE (VarE returnI') ret) | nameBase returnI' == "returnI" ->
            return (exp, ret, vars')
  let ret = exprPattern retExp
  return (pat, exp, ret, newVars \\ patternVariables ret)

unify :: Eq a => [a] -> a
unify = foldl1 (\x y -> if x == y then x else error "Unify failed")

translateCase :: Set Name -> Pat -> [Match] -> Q (Exp, Set Name)
translateCase vars pat matches = do
  splits <- mapM (splitMatch vars) matches
  let commonVars = vars \\ patternVariables pat
      finalVars = unify (map (\(_, _, _, vs) -> vs) splits)
      splits' = map (\(pat, exp, endpat, _) -> (pat, exp, endpat)) splits
  matchExp <- [| Undo $(invScopePlusPattern commonVars pat)
                `Compose` $(translateSplitCase vars splits') |]
  return (matchExp, finalVars)

translateAssignmentCall :: Set Name -> Pat -> Exp -> Pat -> Q (Exp, Set Name)
translateAssignmentCall vars pat fun arg = do
  let commonVars = vars \\ patternVariables arg
  exp <- [| Undo $(invScopePlusPattern commonVars arg) `Compose`
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
-- translateStatement vars (BindS pat (CaseE exp matches)) = do
--   (matchExp, vars') <- translateCase vars (exprPattern exp) matches
--   let commonVars = vars' \\ patternVariables pat
--   exp <- [| $(return matchExp) `Compose` $(invScopePlusPattern vars' pat) |]
--   return (exp, Set.union vars' (patternVariables pat))

translateMiddleStatements :: Set Name -> [Stmt] -> Q (Exp, Set Name)
translateMiddleStatements vars [] = do
  idExp <- [| Pure $ errorless id id |]
  return (idExp, vars)
translateMiddleStatements vars (stmt:rest) = do
  (firstExp, vars') <- translateStatement vars stmt
  (restExp, vars'') <- translateMiddleStatements vars' rest
  exp <- [| $(return firstExp) `Compose` $(return restExp) |]
  return (exp, vars'')

returnPattern :: Set Name -> Pat -> Q Exp
returnPattern vars pat =
  if patternVariables pat /= vars
    then error "Not all variables were returned"
    else [| Compose (Undo $(invScopePlusPattern Set.empty pat))
            (Pure (errorless (\((), res) -> res) (\res -> ((), res)))) |]

translateStatements :: Set Name -> [Stmt] -> Q Exp
translateStatements vars stmts = do
  (middleExp, vars') <- translateMiddleStatements vars (init stmts)
  finalExp <- case last stmts of
    NoBindS (InfixE (Just fun) (VarE op) (Just arg)) | nameBase op == "-<" -> 
      [| Compose $(returnPattern vars' (exprPattern arg)) $(return fun) |]
    NoBindS (AppE (VarE returnI') ret) | nameBase returnI' == "returnI" -> 
      returnPattern vars' (exprPattern ret)
    NoBindS (CaseE exp matches) -> do
      let pat = exprPattern exp
      (matchExp, vars'') <- translateCase vars' pat matches
      [| $(return matchExp)
         `Compose` Pure (errorless (\((), x) -> x) (\x -> ((), x))) |]
    other -> error $ "Bad final statement: " ++ pprint other
  [| $(return middleExp) `Compose` $(return finalExp) |]

-- translateStatements vars [NoBindS (InfixE (Just fun) (VarE op) (Just arg))]
--   | nameBase op == "-<" = [| Compose $(returnPattern vars (exprPattern arg))
--                              $(return fun) |]
-- translateStatements vars [NoBindS (AppE (VarE returnI') ret)]
--   | nameBase returnI' == "returnI" = returnPattern vars (exprPattern ret)
-- translateStatements vars [x] = error ("Bad final statement: " ++ show x)
-- translateStatements vars (stmt:rest) = do
--   (exp, newVars) <- translateStatement vars stmt
--   [| Compose $(return exp) $(translateStatements newVars rest) |]

translateDo :: Exp -> Q Exp
translateDo (LamE [argPat] (DoE stmts)) =
  [| Pure (errorless (\a -> ((), a)) (\((), a) -> a)) `Compose`
     $(invScopePlusPattern Set.empty argPat) `Compose`
     $(translateStatements (patternVariables argPat) stmts) |]
translateDo (DoE stmts) = translateDo (LamE [TupP []] (DoE stmts))

distr :: Q Exp -> Q Exp
distr = (>>= translateDo)
