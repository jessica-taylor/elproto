module Elb.Sampler (
  Sampler, flipCoin, unflipCoin, 
  runSamplerRand, runSamplerIO
) where

import Control.Monad.Error ()
import Control.Monad.Random (Rand, evalRandIO, getRandom)
import System.Random (RandomGen)

import Elb.LogProb (LogProb, toLogProb)


newtype Sampler g a = Sampler {
  runSampler :: LogProb -> Rand g (Either String (a, LogProb))
}


instance Monad (Sampler g) where
  return x = Sampler (\prob -> return (x, prob))
  Sampler run >>= f = Sampler $ \prob -> do
    first <- run prob
    case first of
      Left msg -> return (Left msg)
      Right (res, prob') -> runSampler (f res) prob'
  fail msg = Sampler (\prob -> return (Left msg))

flipCoin :: RandomGen g => Double -> Sampler g Bool
flipCoin prob 
  | prob < 0 || prob > 1 = error "prob must be between 0 and 1"
  | otherwise = Sampler $ \prob -> do
    x <- getRandom
    let heads = x < prob
    return (Right (heads, prob * toLogProb (if heads then prob else 1-prob)))

unflipCoin :: Double -> Bool -> Sampler ()
unflipCoin prob heads
  | prob < 0 || prob > 1 = error "prob must be between 0 and 1"
  | otherwise = Sampler $ \prob -> 
    return (Right ((), prob / toLogProb (if heads then prob else 1-prob)))

runSamplerRand :: Sampler g a -> Rand g (Either String (a, LogProb))
runSamplerRand (Sampler f) = f 0

runSamplerIO :: Sampler StdGen a -> IO (a, LogProb)
runSamplerIO samp = do
  run <- evalRandIO (runSamplerRand samp)
  case run of
    Left msg -> fail msg
    Right (res, prob) -> return (res, prob)

