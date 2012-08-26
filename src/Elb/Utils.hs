{-# LANGUAGE TemplateHaskell #-}
module Elb.Utils (
  pureI, composeI, subcallI, undoI, flipI,
  appliedI, returnI, idI, replicateI, reverseI, categorical
) where

import Elb.InvFun
import Elb.PureInvFun
import Elb.Syntax

pureI :: (Eq a, Eq b) => PureInvFun a b -> InvFun a b
pureI = Pure

composeI :: (Eq a, Eq b, Eq c) => InvFun a b -> InvFun b c -> InvFun a c
composeI = Compose

subcallI :: (Eq a, Eq b, Eq c) => (a -> InvFun b c) -> InvFun (a, b) (a, c)
subcallI = Subcall

undoI :: (Eq a, Eq b) => InvFun a b -> InvFun b a
undoI = Undo

flipI :: Double -> InvFun () Bool
flipI = Flip

idI :: Eq a => InvFun a a
idI = Pure (errorless id id)

returnI :: Eq a => a -> InvFun () a
returnI x = Pure (errorless (const x) (const ()))

appliedI :: (Eq a, Eq b) => InvFun a b -> a -> InvFun () b
appliedI f x = composeI (returnI x) f

reverseI :: Eq a => InvFun [a] [a]
reverseI = Pure $ errorless reverse reverse

replicateI' :: Eq a => Int -> InvFun () a -> InvFun [a] [a]
replicateI' 0 samp = reverseI
replicateI' n samp = $(distr [|\elems -> do
  first <- samp
  replicateI' (n-1) samp -< (first:elems)
  |])

replicateI :: Eq a => Int -> InvFun () a -> InvFun () [a]
replicateI n samp = appliedI (replicateI' n samp) []

data CatTree = Leaf Int | Branch Int Double CatTree CatTree

split :: [a] -> ([a], [a])
split lst = let mid = div (length lst) 2 in (take mid lst, drop mid lst)

makeCatTree :: Int -> [Double] -> CatTree
makeCatTree start [_] = Leaf start
makeCatTree start lst =
  Branch mid (weight left / weight lst) (makeCatTree startleft) 
             (makeCatTree mid right)
  where (left, right) = split lst
        mid = start + length left
        weight lst = sum (map snd lst)

sampleCatTree :: CatTree -> InvFun () Int
sampleCatTree (Leaf x) = returnI x
sampleCatTree (Branch mid p left right) = $(distr [|do
  isLeft <- flipI p
  res <- sampleCatTree (if isLeft then left else right)
  undoI (returnI (res < mid)) -< isLeft
  returnI res


categorical :: [Int] -> InvFun () Int
categorical weights = sampleCatTree (makeCatTree 0 weights)
