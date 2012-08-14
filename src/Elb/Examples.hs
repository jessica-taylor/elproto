module Examples where

import Elb.InvFun
import Elb.Syntax

twoFlips :: Double -> InvFun () (Bool, Bool)
twoFlips prob = [distr|do
  flip1 <- Flip prob
  flip2 <- Flip prob
  (flip1, flip2)
|]
