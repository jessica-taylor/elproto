module Elb.LogProb (
  LogProb, toLogProb, fromLogProb, 
  logToLogProb, logFromLogProb
) where

newtype LogProb = LogProb { logFromLogProb :: Double } deriving (Eq, Ord, Show)

logToLogProb :: Double -> LogProb
logToLogProb = LogProb

toLogProb :: Double -> LogProb
toLogProb = logToLogProb . log

fromLogProb :: LogProb -> Double
fromLogProb = exp . logFromLogProb

instance Num LogProb where
  LogProb a + LogProb b 
    | a >= b = LogProb (a + log (1 + exp (b - a)))
    | otherwise = LogProb (b + log (1 + exp (a - b)))
  LogProb a - LogProb b
    | a >= b = LogProb (a + log (1 - exp (b - a)))
    | otherwise = error "Result of LogProb subtraction is negative"
  LogProb a * LogProb b = LogProb (a + b)
  LogProb a / LogProb b = LogProb (a - b)

    
