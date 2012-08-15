module Elb.PureInvFun (
  PureInvFun(PureInvFun), errorless, call, invert
) where

data PureInvFun a b = 
  PureInvFun (a -> Either String b) (b -> Either String a)
    (a -> b -> a -> String) (b -> a -> b -> String)
  -- mario should learn how these error messages work
  -- arguments: g(f(x)) -> f(x) -> x -> error! if x is not really x
errorless :: (a -> b) -> (b -> a) -> PureInvFun a b
errorless f invF = PureInvFun (return . f) (return . invF) undefined undefined

call :: Eq a => PureInvFun a b -> a -> Either String b
call (PureInvFun f invF err _) x = do
  -- f x is an Either String b
  -- res is just a b
  res <- f x
  -- x' == f^-1(x)
  x' <- invF res
  if x == x' then return res else fail (err x res x')

invert :: PureInvFun a b -> PureInvFun b a
invert (PureInvFun f invF err invErr) = PureInvFun invF f invErr err
