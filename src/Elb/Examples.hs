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
  let distr = replicateInv 20 (Flip 0.2)
  (flips, lp) <- runSamplerIO (sample distr ())
  print (flips, lp)
