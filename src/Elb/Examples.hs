{-# LANGUAGE TemplateHaskell #-}
-- |'Examples' has examples for elproto.
module Examples where

import Elb.InvFun
import Elb.LogProb
import Elb.Numeric
import Elb.PureInvFun
import Elb.Sampler
import Elb.Syntax
import Elb.Utils

twoFlips :: Double -> InvFun () (Bool, Bool)
twoFlips prob = $(distr [|do
  flip1 <- Flip prob
  flip2 <- Flip prob
  returnI (flip1, flip2)
  |])


dirichletSamples :: [Int] -> Int -> Int -> InvFun () Int
dirichletSamples weights scale nsamps = $(distr [|do
  alphas <- dirichlet weights scale
  samples <- replicateI nsamps (


main :: IO ()
main = do
  let samp = replicateI 20 (dirichlet [1,2,3] 1000)
  (res, lp) <- runSamplerIO (sample samp ())
  ((), lp') <- runSamplerIO (sample (undoI samp) res)
  print (res, lp, lp')
  
-- TODO(mario) Write more (LogProb) examples.
