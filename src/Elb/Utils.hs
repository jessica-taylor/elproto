module Elb.Utils (
  invPure, invCompose, invSubcall, invUndo, invFlip,
  applied, invConst, invId, reverseinv
) where

import Elb.InvFun
import Elb.invPureFun
import Elb.Syntax

invPure :: invPureFun a b -> InvFun a b
invPure = Pure

invCompose :: Eq b => InvFun a b -> InvFun b c -> InvFun a c
invCompose = Compose

invSubcall :: (Eq b, Eq c) => (a -> InvFun b c) -> InvFun (a, b) (a, c)
invSubcall = Subcall

invUndo :: InvFun a b -> InvFun b a
invUndo = Undo

invFlip :: Double -> InvFun () Bool
invFlip = Flip

invApplied :: InvFun a b -> a -> InvFun () b
invApplied f x = invCompose f (invConst x)

invId :: InvFun a a
invId = Pure (errorless id id)

invConst :: a -> InvFun () a
invConst x = Pure (errorless (const x) (const ()))

invReverse :: InvFun [a] [a]
invReverse = Pure $ errorless reverse reverse

invReplicate' :: Eq a => Int -> InvFun () a -> InvFun [a] [a]
invReplicate' 0 samp = invReverse
invReplicate' n samp = $(distr [|\elems -> do
  first <- samp
  invReplicate' (n-1) samp -< (first:elems)
  |])

invReplicate :: Eq a => Int -> InvFun () a -> InvFun () [a]
invReplicate n samp = invApplied (invreplicate' n samp) []
