{-# LANGUAGE GADTs, DataKinds, KindSignatures, ScopedTypeVariables #-}
module Lib
    (
    ) where

import GHC.Records
import GHC.TypeLits
import Data.Proxy

data Predicate recordType where
  MkPred  :: (HasField (field :: Symbol) recordType fieldType)
          => Proxy field
          -> (fieldType -> Bool)
          -> Predicate recordType
  (:&&:)  :: Predicate recordType
          -> Predicate recordType
          -> Predicate recordType
  (:||:)  :: Predicate recordType
          -> Predicate recordType
          -> Predicate recordType

execPred :: Predicate recordType -> recordType -> Bool
execPred (MkPred (Proxy :: Proxy field) predFn) = \record ->
  predFn $ getField @field record
execPred (pred1 :&&: pred2) = \record ->
  execPred pred1 record && execPred pred2 record
execPred (pred1 :||: pred2) = \record ->
  execPred pred1 record || execPred pred2 record

data EndMileType = MkEndMileType { id :: Int
                                 , response :: String
                                 , done :: Bool }

x :: Predicate EndMileType
x = MkPred (Proxy @"id") (\n -> n == 5)

y :: Predicate EndMileType
y = MkPred (Proxy @"response") (\n -> n == "x")

z :: Predicate EndMileType
z = MkPred (Proxy @"done") Prelude.id
