module Elb.Sampler (
  Sampler, runSampler, flipCoin, unflipCoin, runSamplerIO
) where

import Control.Monad.Error ()
import System.Random (StdGen, random, newStdGen)


data Sampler a = Sampler {
  runSampler :: StdGen -> Double -> Either String (a, StdGen, Double) 
}
-- flipCoin 2 coins
-- get result: prob 1/4
-- could pass probability, but number small
-- log xy = log x + log y
-- mario did log probability in 109 ^^
-- wtf did they call it?
-- you can store the log of a small number in a double, but storing a really
-- fucking small double is bad. FLOATING POINT AHHAHH

-- definition of how this is a monad
-- Monad is a typeclass. This is a typeclass definition. Defines instance of
-- typeclass Monad which someone already defined.
instance Monad Sampler where
  -- constant sampler, always ret x
  -- Right is Correct
  return x = Sampler (\g lp -> Right (x, g, lp))
  -- >>= defines new infix operator
  Sampler run >>= f = Sampler $ \g lp -> do
    -- whole thing is Bool -> Sampler Bool
    -- line below runs flipCoin 0.5
    (x, g', lp') <- run g lp
    -- anything that has return, bind is monad
    -- g' is new seed, lp' is new lp. lawl tautology
    runSampler (f x) g' lp'
  -- Left is wrong. Fucking ideological Haskellers.
  fail msg = Sampler (\g lp -> (Left msg))
-- <- do syntax
-- monads are wizard things, types are your spells
-- example of new infix (bind) operator:
-- flipCoin 0.5 >>= \f -> if f then flip 0.9 else flip 0.1
-- return a Sampler

-- takes p of coming true, returns sampler (== random generator)
flipCoin :: Double -> Sampler Bool
-- $ means apply Sampler to \g and lp
flipCoin prob 
  | prob < 0 || prob > 1 = error "prob must be between 0 and 1"
  | otherwise = Sampler $ \g lp -> 
  let (x, g') = random g
      heads = x < prob
    in return (heads, g', lp + log (if heads then prob else 1-prob))
-- TODO(mario) figure out why this flipCoins a coin, grok the syntax

unflipCoin :: Double -> Bool -> Sampler ()
unflipCoin prob heads =
  | prob < 0 || prob > 1 = error "prob must be between 0 and 1"
  | otherwise = Sampler $ \g lp -> 
  return ((), g, lp - log (if heads then prob else 1-prob))
-- increases log probability (flipCoin 2 coins! back to the future! now 1 coin!)

runSamplerIO :: Sampler a -> IO (a, Double)
runSamplerIO samp = do
  gen <- newStdGen
  case runSampler samp gen 0 of
    Left msg -> fail msg
    Right (res, _, lp) -> return (res, lp)

