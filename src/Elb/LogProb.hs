-- |'Elb.LogProb' exports the basic type for log probability and several
-- functions for basic operations on log probability.
module Elb.LogProb (
  LogProb, toLogProb, fromLogProb, 
  logToLogProb, logFromLogProb
) where

-- |'LogProb' is a type for log probabilities.
newtype LogProb = LogProb { logFromLogProb :: Double } deriving (Eq, Ord, Show)

-- |'logToLogProb' casts a Double representing a log probability as a LogProb.
logToLogProb :: Double -> LogProb
logToLogProb = LogProb

-- |'toLogProb' takes a Double representing a probability and returns the log
-- probability.
toLogProb :: Double -> LogProb
toLogProb = logToLogProb . log

-- |'fromLogProb' takes a LogProb and returns the non-logarithmic probability
-- as a Double.
fromLogProb :: LogProb -> Double
fromLogProb = exp . logFromLogProb

-- |'Num LogProb' defines basic arithmetic operations (addition, subtraction,
-- multiplication) for working with log probabilities.
instance Num LogProb where
  LogProb a + LogProb b 
    | a >= b = LogProb (a + log (1 + exp (b - a)))
    | otherwise = LogProb (b + log (1 + exp (a - b)))
  LogProb a - LogProb b
    | a >= b = LogProb (a + log (1 - exp (b - a)))
    | otherwise = error "Result of LogProb subtraction is negative"
  LogProb a * LogProb b = LogProb (a + b)
  fromInteger = toLogProb . fromInteger
  -- TODO(mario) Put in more fns from Num. Make sure this works as users would
  -- expect it to work.
  -- TODO(mario) Minimize runtime errors since people may expect behavior of
  -- Num.
  -- TODO(mario) Try to use this for basic examples (LogReg, Naive Bayes). 

-- |'Fractional LogProb' defines more basic arithmetic operations (division)
-- for working with log probabilities.
instance Fractional LogProb where
  LogProb a / LogProb b = LogProb (a - b)
