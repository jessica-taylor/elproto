module Elb.Utils (
  pureI, composeI, subcallI, undoI, flipI,
  applied, constI, idI, reverseinv
) where

import Elb.InvFun
import Elb.PureInvFun
import Elb.Syntax

pureI :: PureInvFun a b -> InvFun a b
pureI = Pure

composeI :: Eq b => InvFun a b -> InvFun b c -> InvFun a c
composeI = Compose

subcallI :: (Eq b, Eq c) => (a -> InvFun b c) -> InvFun (a, b) (a, c)
subcallI = Subcall

undoI :: InvFun a b -> InvFun b a
undoI = Undo

flipI :: Double -> InvFun () Bool
flipI = Flip

appliedI :: InvFun a b -> a -> InvFun () b
appliedI f x = composeI f (constI x)

idI :: InvFun a a
idI = Pure (errorless id id)

constI :: a -> InvFun () a
constI x = Pure (errorless (const x) (const ()))

reverseI :: InvFun [a] [a]
reverseI = Pure $ errorless reverse reverse

replicateI' :: Eq a => Int -> InvFun () a -> InvFun [a] [a]
replicateI' 0 samp = reverseI
replicateI' n samp = $(distr [|\elems -> do
  first <- samp
  replicateI' (n-1) samp -< (first:elems)
  |])

replicateI :: Eq a => Int -> InvFun () a -> InvFun () [a]
replicateI n samp = appliedI (replicateI' n samp) []
