module Elb.Inference where

import System.Random (RandomGen)

import Elb.InvFun (InvFun, sample)
import Elb.LogProb (LogProb)
import Elb.Sampler (Sampler, runSamplerRand)
import Elb.Utils (undoI)



scoredHypothesis :: (RandomGen g, Eq a, Eq b) => InvFun () a
                 -> (a -> InvFun () b) -> (b -> InvFun () a) -> b 
                 -> Sampler g a
scoredHypothesis prior obsFun posterior obs = do
    hyp <- sample (posterior obs) ()
    sample (undoI (obsFun hyp)) obs
    sample (undoI prior) hyp
    return hyp

-- TODO(mario) Look over notes from Jacob while documenting this. This is an
-- example very illustrative of the utility of reversible programming in
-- Bayesian probability.
scoredPosterior :: (RandomGen g, Eq a, Eq b) => InvFun () a 
                -> (a -> InvFun () b) -> (b -> InvFun () a) -> Sampler g ()
scoredPosterior prior obsFun posterior = do
  -- P(F)
  fact <- sample prior ()
  -- P(O|F)
  obs <- sample (obsFun fact) ()
  -- 1 / P(F|O)
  sample (undoI (posterior obs)) fact
  -- P(H|O)
  hyp <- sample (posterior obs) ()
  -- 1 / P(O|H)
  sample (undoI (obsFun hyp)) obs
  -- 1 / P(H)
  sample (undoI prior) hyp
  -- P(H) P(O|H) P(F|O) / (P(F) P(O|F) P(H|O))
  -- = (P(H) P(O|H) / P(H|O)) / (P(F) P(O|F) / P(F|O))
  -- = P(O) / P(O)
  -- Yeah I'm not sure what this means but it seems like it should work.
