{-# LANGUAGE TemplateHaskell #-}
module Examples where

import Elb.InvFun
import Elb.Sampler
import Elb.Syntax
import Elb.Utils

twoFlips :: Double -> InvFun () (Bool, Bool)
twoFlips prob = $(distr [|do
  flip1 <- Flip prob
  flip2 <- Flip prob
  return (flip1, flip2)
  |])

main :: IO ()
main = do
  (res, lp) <- runSamplerIO (sample (twoFlips 0.5) ())
  print (res, lp)

-- TODO(mario) Write more examples.
