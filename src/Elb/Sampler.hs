module Elb.Sampler (
  Sampler, flip, unflip
) where

import Data.Random (StdGen, random)

-- record syntax {} - define type, get function. like a struct member
data Sampler a = Sampler { 
  runSampler :: StdGen -> Double -> Either String (a, StdGen, Double) 
}
-- flip 2 coins
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
    -- line below runs flip 0.5
    (x, g', lp') <- run g lp
    -- anything that has return, bind is monad
    -- g' is new seed, lp' is new lp. lawl tautology
    runSampler (f x) g' lp'
  -- Left is wrong. Fucking ideological Haskellers.
  fail msg = Sampler (\g lp -> (Left msg))
-- <- do syntax
-- monads are wizard things, types are your spells
-- example of new infix (bind) operator:
-- flip 0.5 >>= \f -> if f then flip 0.9 else flip 0.1
-- return a Sampler

-- takes p of coming true, returns sampler (== random generator)
flip :: Double -> Sampler Bool
-- $ means apply Sampler to \g and lp
flip prob = Sampler $ \g lp -> 
-- this may be fucked up
  return (flip, g', lp + log (if flip then prob else 1-prob))
  -- where is let backwards
  where (x, g') = random g
        flip = x < prob)
-- TODO(mario) figure out why this flips a coin, grok the syntax

unflip :: Double -> Bool -> Sampler ()
unflip prob flip = Sampler $ \g lp -> 
  return ((), g, lp - log (if flip then prob else 1-prob)))
-- increases log probability (flip 2 coins! back to the future! now 1 coin!)
