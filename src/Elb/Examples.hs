{-# LANGUAGE TemplateHaskell #-}
-- |'Examples' has examples for elproto.
module Examples where

import Control.Monad (liftM, replicateM)

import Elb.Inference
import Elb.InvFun
import Elb.LogProb
import Elb.Numeric
import Elb.PureInvFun
import Elb.Sampler
import Elb.Syntax
import Elb.Utils

priorWeights :: [Double]
priorWeights = [1, 2, 3]

numSamples :: Int
numSamples = 20

prior :: InvFun () [Int]
prior = dirichlet priorWeights 1000

obsFun :: [Int] -> InvFun () [Int]
obsFun weights = replicateI numSamples (categorical $ map fromIntegral weights)

truePosterior :: [Int] -> InvFun () [Int]
truePosterior samples = dirichlet weights 1000
  where weights = zipWith (+) priorWeights (map indexCount [0..])
        indexCount n = fromIntegral $ length (filter (==n) samples)

badPosterior :: [Int] -> InvFun () [Int]
badPosterior = const prior

main :: IO ()
main = do
  let score post = runSamplerIO (scoredPosterior prior obsFun post)
      printScores lst = print (map (logFromLogProb . snd) lst)
  replicateM 20 (score truePosterior) >>= printScores
  replicateM 20 (score badPosterior) >>= printScores
-- TODO(mario) Write more (LogProb) examples.
