module Elb.PureInvFun (
  PureInvFun(PureInvfun), errorless, call, invert
) where

data PureInvFun a b = 
  PureInvFun (a -> Either String b) (b -> Either String a)
    (a -> b -> a -> String) (b -> a -> b -> String)

errorless :: (a -> b) -> (b -> a) -> PureInvFun a b
errorless f invF = Pure (return . f) (return . invF) undefined undefined

call :: Eq a => PureInvFun a b -> a -> Either String b
call (PureInvFun f invF err _) x = do
  res <- f x
  x' <- invF res
  if x == x' then return res else fail (err x res x')

invert :: PureInvFun a b -> PureInvFun b a
invert (PureInvFun f invF err invErr) = PureInvFun invF f invErr err
