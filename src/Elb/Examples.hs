{-# LANGUAGE TemplateHaskell #-}
-- |'Examples' has examples for elproto.
module Examples where

import Elb.Inference
import Elb.InvFun
import Elb.LogProb
import Elb.Numeric
import Elb.PureInvFun
import Elb.Sampler
import Elb.Syntax
import Elb.Utils

priorWeights :: [Int]
priorWeights = [1, 2, 3]

numSamples :: Int
numSamples = 20

prior :: InvFun () [Int]
prior = dirichlet priorWeights 1000

obsFun :: [Int] -> InvFun () [Int]
obsFun weights = replicateI numSamples (categorical weights)

truePosterior :: [Int] -> InvFun () [Int]
truePosterior samples = 
  zipWith (+) priorWeights (map (\n -> length (filter (==n) samples)) [0..])

badPosterior :: [Int] -> InvFun () [Int]
badPosterior = const prior

main :: IO ()
main = do
  print $ replicateM 20 (runSamplerIO (scoredHypothesis prior obsFun truePosterior)) 
  print $ replicateM 20 (runSamplerIO (scoredHypothesis prior obsFun badPosterior)) 
  
  
-- TODO(mario) Write more (LogProb) examples.
