module Elb.Utils (constant, idInv) where

import Elb.InvFun
import Elb.PureInvFun


idInv :: InvFun a a
idInv = Pure (errorless id id)

constant :: a -> InvFun () a
constant x = Pure (errorless (const x) (const ()))
