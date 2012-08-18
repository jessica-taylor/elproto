{-# LANGUAGE TemplateHaskell #-}
module Examples where

import Elb.InvFun
import Elb.PureInvFun
import Elb.Sampler
import Elb.Syntax
import Elb.Utils

twoFlips :: Double -> InvFun () (Bool, Bool)
twoFlips prob = $(distr [|do
  flip1 <- Flip prob
  flip2 <- Flip prob
  return (flip1, flip2)
  |])

replicateInv' :: Eq a => Int -> InvFun () a -> InvFun [a] [a]
replicateInv' 0 samp = Pure (errorless reverse reverse)
-- TODO(mario) Understand why |\elems comes from [a] better. Lambda calc.
-- distr is a keyword Jacob created.
replicateInv' n samp = $(distr [|\elems -> do
  first <- samp
  replicateInv' (n-1) samp -< (first:elems)
  |])

replicateInv :: Eq a => Int -> InvFun () a -> InvFun () [a]
replicateInv n samp = $(distr [|do
  replicateInv' n samp -< []
  |])


main :: IO ()
main = do
  (res, lp) <- runSamplerIO (sample (twoFlips 0.5) ())
  print (res, lp)

  let samp = replicateInv 20 (Flip 0.2)
  (flips, lp) <- runSamplerIO (sample samp ())
  print (flips, lp)
  
-- TODO(mario) Write more examples.
