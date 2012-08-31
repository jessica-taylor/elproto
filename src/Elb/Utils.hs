{-# LANGUAGE TemplateHaskell #-}
module Elb.Utils (
  pureI, composeI, subcallI, undoI, flipI,
  appliedI, returnI, idI, replicateI, reverseI
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

-- | 'returnI' takes an x and returns an InvFun from () to that x.
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

