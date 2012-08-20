{-# LANGUAGE TemplateHaskell #-}
module Elb.Numeric where

import Numeric.SpecFunctions (incompleteBeta)

import Elb.InvFun
import Elb.PureInvFun
import Elb.Syntax
import Elb.Utils (appliedI, returnI, flipI, reverseI, undoI)

binarySearchInt :: (Int -> Int -> Int -> Double) -> Int -> Int -> InvFun () Int
binarySearchInt probLess low high
  | high <= low = error "high must exceed low"
  | high == low + 1 = returnI low
  | otherwise = $(distr [|do
    isLess <- flipI (probLess low mid high)
    result <- uncurry randInRange (if isLess then (low, mid) else (mid, high))
    undoI (returnI (result < mid)) -< isLess
    returnI result
    |]) where mid = low + div (high + 1 - low) 2

portion :: Double -> Double -> Double -> Double
portion low med high = (med - low) / (high - low)

randInRange :: Int -> Int -> InvFun () Int
randInRange = binarySearchInt getProb
  where getProb l m h = 
          portion (fromIntegral l) (fromIntegral m) (fromIntegral h)

fromCdfHelper :: (Double -> Double) -> Int -> Int -> Int -> InvFun () Int
fromCdfHelper cdf scale = binarySearchInt getProb
  where getProb l m h = fallback ((cdf' m - cdf' l) / (cdf' h - cdf' l)) $
                                 portion (fromIntegral l) (fromIntegral m) 
                                         (fromIntegral h)
        cdf' x = cdf (fromIntegral x / fromIntegral scale)
        fallback a b = if 0 <= a && a <= 1 then a else b

fromCdf :: (Double -> Double) -> Int -> InvFun () Int
fromCdf cdf scale = fromCdfHelper cdf scale 0 scale

beta :: Double -> Double -> Int -> InvFun () Int
beta a b = fromCdf (incompleteBeta a b)

dirichletHelper :: [(Double, Double)] -> Int -> InvFun () [Int]
dirichletHelper [] _ = returnI []
dirichletHelper [_] scale = returnI [scale]
dirichletHelper ((weight, sumRestWeights) : restWeights) scale = $(distr [|do
  first <- beta weight sumRestWeights scale
  rest <- dirichletHelper restWeights (scale - first)
  returnI (first:rest)
  |])

dirichlet :: [Double] -> Int -> InvFun () [Int]
dirichlet weights scale = dirichletHelper (zip weights restSums) scale
  where restSums = tail $ scanl (-) (sum weights) weights
