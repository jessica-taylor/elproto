




data Distr :: * -> * where
  Unit :: Distr ()
  Transform :: (Eq a, Eq b) => Distr a -> (a -> b) -> (b -> a) -> Distr b
  Product :: Distr a -> (a -> Distr b) -> Distr (a, b)
  Contract :: Distr (a, b) -> (a -> Distr b) -> Distr a
  Flip :: Double -> Distr Bool
