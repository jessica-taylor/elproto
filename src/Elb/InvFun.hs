module Elb.InvFun (
  Invfun(Pure, Compose, Subcall, Undo, Flip),
  sample
) where

import Control.Monad.Error ()

import Elb.PureInvFun (PureInvFun)
import qualified Elb.PureInvFun as Pure
import Elb.Sampler (Sampler, flip, unflip)

data InvFun :: * -> * -> * where
  Pure :: PureInvFun a b -> InvFun a b
  Compose :: Eq b => InvFun a b -> InvFun b c -> InvFun a c
  Subcall :: (a -> InvFun b c) -> InvFun (a, b) (a, c)
  Undo :: InvFun a b -> InvFun b a
  Flip :: Double -> InvFun () Bool

sample :: (Eq a, Eq b) => InvFun a b -> a -> Sampler b
sample (Pure f) x = case Pure.call f x of
  Left err -> fail err
  Right res -> return res
sample (Compose f g) x = sample f x >>= sample g
sample (Subcall f) (a, b) = do
  c <- sample (f a) b
  return (a, c)
sample (Undo f) x = unsample f x
sample (Flip prob) () = flip prob

unsample :: (Eq a, Eq b) => InvFun a b -> b -> Sampler a
unsample (Pure f) x = sample (Pure.invert f) x
unsample (Compose f g) x = unsample g x >>= unsample f
unsample (Subcall f) x = sample (Subcall (Undo . f)) x
unsample (Undo f) x = sample f x
unsample (Flip prob) flip = unflip prob flip

