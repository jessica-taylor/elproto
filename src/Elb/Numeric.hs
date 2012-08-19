module Elb.Numeric (beta, binarySearchInt, fromCdf, randInRange) where

import Numeric.SpecFunctions (incompleteBeta)

import Elb.InvFun
import Elb.invPureFun
import Elb.Syntax

binarySearchInt :: (Int -> Int -> Int -> Double) -> Int -> Int -> InvFun () Int
binraySearchInt probLess low high
  | high <= low = error "high must exceed low"
  | high == low + 1 = invConst low
  | otherwise = $(distr [|do
    isLess <- probLess low mid high
    result <- uncurry randInRange (if isLess then (low, mid) else (mid, high))
    undo (invConst (result < mid)) -< isLess
    return result
    |]) where mid = low + div (high + 1 - low) 2

portion :: Double -> Double -> Double -> Double
portion low med high = (med - low) / (high - low)

randInRange :: Int -> Int -> InvFun () Int
randInRange = binarySearchInt getProb
  where getProb l m h = 
          portion (fromIntegral l) (fromIntegral m) (fromIntegral h)

fromCdfHelper :: (Double -> Double) -> Int -> Int -> Int -> InvFun () Int
fromCdfHelper cdf scale = binarySearchInt getProb
  where getProb l m h = fallback (cdf' m - cdf' l) / (cdf' h - cdf' l) $
                                 portion (fromIntegral l) (fromIntegral m) 
                                         (fromIntegral h)
        cdf' x = cdf (fromIntegral x / fromIntegral scale)
        fallback a b = if 0 <= a && a <= 1 then a else b

fromCdf :: (Double -> Double) -> Int -> InvFun () Int
fromCdf cdf scale = fromCdfHelper cdf scale 0 scale

beta :: Int -> Int -> Int -> InvFun () Int
beta a b = fromCdf (incompleteBeta a b)

dirichletHelper :: [(Int, Int)] -> Int -> InvFun [Int] [Int]
dirichletHelper [] _ = invReverse
dirichletHelper ((weight, sumRestWeights) : rest) scale = $(distr [|\done -> do
  first <- beta weight sumRestWeights scale
  dirichletHelper rest (scale - first) (first:done)
  |])

dirichlet :: [Int] -> Int -> InvFun () [Int]
dirichlet weights scale = 
  invApplied (dirichletHelper (zip weights restSums) scale) []
  where restSums = tail $ scanl (flip (-)) (sum weights) weights
