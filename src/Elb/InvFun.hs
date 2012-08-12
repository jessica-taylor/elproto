


data InvFun :: * -> * -> * where
  Pure :: (a -> b) -> (b -> a) -> InvFun a b
  Compose :: InvFun a b -> InvFun b c -> InvFun a c
  Subcall :: (a -> InvFun b c) -> InvFun (a, b) (a, c)
  Undo :: InvFun a b -> InvFun b a
  Flip :: Double -> InvFun () Bool
