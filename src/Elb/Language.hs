{-# LANGUAGE TemplateHaskell #-}
module Elb.Language where

import Elb.InvFun
import Elb.PureInvFun
import Elb.Syntax
import Elb.Utils (appliedI, returnI, flipI, reverseI, undoI, incI)

natToWord :: InvFun Int (Maybe Int)
natToWord = $(distr [|\num -> do
  case num of
    0 -> returnI Nothing
    n -> do
      n <- incI (-1) -< n
      returnI (Just n)
  |])
    

bagOfWordsObsFun :: InvFun () (Maybe Int) -> InvFun () [Int]
bagOfWordsObsFun getWord = $(distr [|do
  first <- getWord
  case first of
    Nothing -> returnI []
    Just word -> do
      rest <- bagOfWordsObsFun getWord
      returnI (word : rest)
  |])

