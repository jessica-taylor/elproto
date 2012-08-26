module Elb.LogProb (LogProb, toLogProb, fromLogProb) where

newtype LogProb = LogProb { fromLogProb :: Double } deriving (Eq, Ord, Show)

toLogProb :: Double -> LogProb
toLogProb = LogProb

instance Num LogProb where
  LogProb a + LogProb b 
    | a >= b = LogProb (a + log (1 + exp (b - a)))
    | otherwise = LogProb (b + log (1 + exp (a - b)))
  LogProb a - LogProb b
    | a >= b = LogProb (a + log (1 - exp (b - a)))
    | otherwise = error "Result of LogProb subtraction is negative"
  LogProb a * LogProb b = LogProb (a + b)
  LogProb a / LogProb b = LogProb (a - b)

    
