{-# LANGUAGE TemplateHaskell #-}
module Elb.Utils (
  pureI, composeI, subcallI, undoI, flipI,
  appliedI, returnI, idI, replicateI, reverseI
) where

import Elb.InvFun
import Elb.PureInvFun
import Elb.Syntax

-- |'PureI' transforms a pure invertible function into an invertible function.
-- TODO(mario/jacob) Mario may need help isolating the exact difference between
-- PureInvFun and InvFun, although he'll read more source to try to understand.
pureI :: (Eq a, Eq b) => PureInvFun a b -> InvFun a b
pureI = Pure

-- |'composeI' makes invertible functions transitive by composing them --
-- pretty simple.
composeI :: (Eq a, Eq b, Eq c) => InvFun a b -> InvFun b c -> InvFun a c
composeI = Compose

subcallI :: (Eq a, Eq b, Eq c) => (a -> InvFun b c) -> InvFun (a, b) (a, c)
subcallI = Subcall

-- 'undoI' reverses an invertible function.
undoI :: (Eq a, Eq b) => InvFun a b -> InvFun b a
undoI = Undo

-- 'flipI' emulates flipping a coin, taking a 'Double' representing the
-- probability of True or False.
flipI :: Double -> InvFun () Bool
flipI = Flip

-- | 'idI' gives the identity function.
idI :: Eq a => InvFun a a
idI = Pure (errorless id id)

-- | 'returnI' takes an x and returns an InvFun from () to that x.
returnI :: Eq a => a -> InvFun () a
returnI x = Pure (errorless (const x) (const ()))

-- | 'appliedI' applies an invertible function given the first parameter and
-- returns an invertible function from nothing () to the result.
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

