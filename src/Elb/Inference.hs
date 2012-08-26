module Elb.Inference where

scoredHypothesis :: (RandomGen g) => InvFun () a -> (a -> InvFun () b) 
                 -> (b -> InvFun () a) -> b -> Rand (a, LogProb)
scoredHypothesis prior obsFun posterior obs = do
  -- P(H|O)
  (hyp, posteriorProb) <- sample (posterior obs) ()
  -- P(O|H)
  ((), obsProb) <- sample (undoI (obsFun hyp)) obs
  -- P(H)
  ((), priorProb) <- sample (undoI prior) hyp
  -- P(O) = P(H) P(O|H) / P(H|O)
  -- better P(O) means better posterior
  return (hyp, obsProb * priorProb / posteriorProb)


