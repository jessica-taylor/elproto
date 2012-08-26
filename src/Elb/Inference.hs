module Elb.Inference where

import Elb.InvFun (InvFun, sampler)
import Elb.LogProb (LogProb)
import Elb.Sampler (runSamplerRand)
import Elb.Utils (undoI)

scoredHypothesis :: (RandomGen g) => InvFun () a -> (a -> InvFun () b) 
                 -> (b -> InvFun () a) -> b -> Rand (a, LogProb)
scoredHypothesis prior obsFun posterior obs = do
  -- P(H|O)
  (hyp, posteriorProb) <- runSamplerRand $ sample (posterior obs) ()
  -- 1 / P(O|H)
  ((), invObsProb) <- runSamplerRand $ sample (undoI (obsFun hyp)) obs
  -- 1 / P(H)
  ((), invPriorProb) <- runSamplerRand $ sample (undoI prior) hyp
  -- P(O) = P(H) P(O|H) / P(H|O)
  -- better P(O) means better posterior
  return (hyp, 1 / (invPriorProb * invObsProb * posteriorProb))

scoredPosterior :: (RandomGen g) => InvFun () a -> (a -> InvFun () b)
                -> (b -> InvFun () a) -> Rand LogProb
scoredPosterior prior obsFun posterior = do
  -- P(F)
  (fact, factProb) <- runSamplerRand $ sample prior ()
  -- P(O|F)
  (obs, obsFactProb) <- runSamplerRand $ sample (obsFun fact) ()
  -- 1 / P(F|O)
  ((), invPosteriorFactProb) <- runSamplerRand $ 
                                  sample (undoI (posterior obs)) fact
  -- P(H|O)
  (hyp, posteriorHypProb) <- runSamplerRand $ sample (posterior obs) ()
  -- 1 / P(O|H)
  ((), invObsHypProb) <- runSamplerRand $ sample (undoI (obsFun hyp)) obs
  -- 1 / P(H)
  ((), invHypProb) <- runSamplerRand $ sample (undoI prior) hyp
  -- P(H) P(O|H) P(F|O) / (P(F) P(O|F) P(H|O))
  -- = (P(H) P(O|H) / P(H|O)) / (P(F) P(O|F) / P(F|O))
  -- = P(O) / P(O)
  -- Yeah I'm not sure what this means but it seems like it should work.
  return (1 / (factProb * obsFactProb * invPosteriorFactProb *
               posteriorHypProb * invObsHypProb * invHypProb))
