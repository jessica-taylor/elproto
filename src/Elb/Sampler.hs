module Elb.Sampler (
  Sampler, flip, unflip
) where

import Data.Random (StdGen, random)


data Sampler a = Sampler { 
  runSampler :: StdGen -> Double -> Either String (a, StdGen, Double) 
}

instance Monad Sampler where
  return x = Sampler (\g lp -> Right (x, g, lp))
  Sampler run >>= f = Sampler $ \g lp -> do
    (x, g', lp') <- run g lp
    runSampler (f x) g' lp'
  fail msg = Sampler (const (Left msg))

flip :: Double -> Sampler Bool
flip prob = Sampler $ \g lp -> 
  return (flip, g', lp + log (if flip then prob else 1-prob))
  where (x, g') = random g
        flip = x < prob)

unflip :: Double -> Bool -> Sampler ()
unflip prob flip = Sampler $ \g lp -> 
  return ((), g, lp - log (if flip then prob else 1-prob)))
