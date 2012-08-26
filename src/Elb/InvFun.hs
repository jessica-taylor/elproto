{-# LANGUAGE GADTs #-}
module Elb.InvFun (
  InvFun(Pure, Compose, Subcall, Undo, Flip),
  sample
) where

import Control.Monad.Error ()

import Elb.PureInvFun (PureInvFun)
import qualified Elb.PureInvFun as Pure
import Elb.Sampler (Sampler, flipCoin, unflipCoin)
import System.Random (RandomGen)

data InvFun a b where
  Pure :: (Eq a, Eq b) => PureInvFun a b -> InvFun a b
  Compose :: (Eq a, Eq b, Eq c) => InvFun a b -> InvFun b c -> InvFun a c
  Subcall :: (Eq a, Eq b, Eq c) => (a -> InvFun b c) -> InvFun (a, b) (a, c)
  Undo :: (Eq a, Eq b) => InvFun a b -> InvFun b a
  Flip :: Double -> InvFun () Bool
  --Importance :: InvFun () (a, b) -> ([b -> InvFun () a], Double) -> b -> InvFun () a


sample :: (Eq a, Eq b, RandomGen g) => InvFun a b -> a -> Sampler g b
sample (Pure f) x = case Pure.call f x of
  Left err -> fail err
  Right res -> return res
sample (Compose f g) x = sample f x >>= sample g
sample (Subcall f) (a, b) = do
  c <- sample (f a) b
  return (a, c)
sample (Undo f) x = unsample f x
sample (Flip prob) () = flipCoin prob
-- sample (Importance prob joint posts obs) () = do
--   hypLps <- mapM (\(post, _) -> subSample (sample (post obs) ())) posts
--   ((hyp, hypLp), postLp) <- selectByLp $ zip (zip hypLps (map snd posts))
--                                     (map snd hypLps)
--   addLp (hypLp - log (postLp / sum (map snd posts))))
--   return hyp


unsample :: (Eq a, Eq b, RandomGen g) => InvFun a b -> b -> Sampler g a
unsample (Pure f) x = sample (Pure (Pure.invert f)) x
unsample (Compose f g) x = unsample g x >>= unsample f
unsample (Subcall f) x = sample (Subcall (Undo . f)) x
unsample (Undo f) x = sample f x
unsample (Flip prob) heads = unflipCoin prob heads
