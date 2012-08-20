{-# LANGUAGE TemplateHaskell #-}
module Examples where

import Elb.InvFun
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


main :: IO ()
main = do
  let samp = replicateI 20 (dirichlet [1,2,3] 1000)
  (res, lp) <- runSamplerIO (sample samp ())
  print (res, lp)
  
-- TODO(mario) Write more examples.
