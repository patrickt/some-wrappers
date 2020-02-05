{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | This module provides 'Some' and 'Some1', which are GADT wrappers
-- providing a convenient way to create a polymorphic type constrained by some
-- provided typeclass reified in the signature.
--
-- It is intended to replace data types of the form
--
-- @
--   data SomeShow = forall a . Show a => SomeShow
-- @
--
-- with the easier type @Some Show@.
module Data.Some
  ( Some (..)
  , Some1 (..)
  ) where

import Data.Aeson
import Data.Kind
import Data.Proxy

-- | A polymorphic wrapper around a type of kind @*.
data Some (c :: Type -> Constraint) where
  Some :: c a => a -> Some c

instance ToJSON (Some ToJSON) where
  toJSON (Some a) = toJSON a
  toEncoding (Some a) = toEncoding a


-- | A polymorphic wrapper around a kind of parameterized type @* -> *@.
-- The first constraint represents those rendered on the container @f@,
-- and the second represents those on the constituent type @a@. If you
-- don't need any constraints on @a@, use 'Forall'.
--
-- The extra @k@ parameter to the constructor is to allow the kind of
-- the parameterized type to be poly-kinded in the input type. This
-- allows you to pass @(~) Proxy@ as the first argument and have some
-- type-level function as the second.
data Some1 c d where
  Some1 ::
    forall k
      (c :: (k -> Type) -> Constraint)
      (d :: k -> Constraint)
      (f :: k -> Type)
      (a :: k) .
    (c f, d a)
    => f a
    -> Some1 c d

instance ToJSON (Some1 ToJSON1 ToJSON) where
  toJSON (Some1 a) = toJSON1 a
  toEncoding (Some1 a) = toEncoding1 a
