module Elb.Sampler (
  Sampler, flipCoin, unflipCoin, 
  runSamplerRand, runSamplerIO
) where

import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad.Random (Rand, evalRandIO, getRandom)
import System.Random (RandomGen)

import Elb.LogProb (LogProb, toLogProb)


newtype Sampler g a = Sampler {
  runSampler :: LogProb -> ErrorT String (Rand g) (a, LogProb)
}


instance Monad (Sampler g) where
  return x = Sampler (\prob -> return (x, prob))
  Sampler run >>= f = Sampler $ \prob -> do
    (first, prob') <- run prob
    runSampler (f first) prob'
  fail msg = Sampler (const (fail msg))

flipCoin :: RandomGen g => Double -> Sampler g Bool
flipCoin prob 
  | prob < 0 || prob > 1 = error "prob must be between 0 and 1"
  | otherwise = Sampler $ \prob -> do
    x <- lift getRandom
    let heads = x < prob
    return (heads, prob * toLogProb (if heads then prob else 1-prob))

unflipCoin :: Double -> Bool -> Sampler ()
unflipCoin prob heads
  | prob < 0 || prob > 1 = error "prob must be between 0 and 1"
  | otherwise = Sampler $ \prob ->
    return ((), prob / toLogProb (if heads then prob else 1-prob))

runSamplerRand :: Sampler g a -> ErrorT String (Rand g) (a, LogProb)
runSamplerRand (Sampler f) = f 1

runSamplerIO :: Sampler StdGen a -> IO (a, LogProb)
runSamplerIO samp = do
  run <- evalRandIO (runErrorT $ runSamplerRand samp)
  case run of
    Left msg -> fail msg
    Right (res, prob) -> return (res, prob)

