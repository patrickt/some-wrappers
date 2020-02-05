{-# LANGUAGE ConstraintKinds, FlexibleInstances, GADTs, KindSignatures, PolyKinds, RankNTypes, TypeFamilies #-}
-- | This module provides 'Some', 'Some', and 'SSome', which are GADT wrappers
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
  , SSome (..)
  , SomeJSON
  , Forall
  ) where

import Data.Aeson
import Data.Kind
import Data.Proxy

-- | A polymorphic wrapper around a type of kind @*.
data Some (c :: Type -> Constraint) where
  Some :: c a => a -> Some c

instance ToJSON (Some JSON) where
  toJSON (Some a) = toJSON a
  toEncoding (Some a) = toEncoding a


-- | A polymorphic wrapper around a kind of parameterized type @* -> *@.
-- The first constraint represents those rendered on the container @f@,
-- and the second represents those on the constituent type @a@. If you
-- don't need any constraints on @a@, use 'Forall'.
data Some1 (c :: (Type -> Type) -> Constraint) (d :: Type -> Constraint) where
  Some1 :: forall c d f a . (c f, d a) => f a -> Some1 c d

instance ToJSON (Some1 ToJSON1 ToJSON) where
  toJSON (Some1 a) = toJSON1 a
  toEncoding (Some1 a) = toEncoding1 a


-- | A polykinded wrapper around a 'Proxy'. Useful when operating on singleton types.
data SSome (c :: k -> Constraint) where
  SSome :: c a => Proxy a -> SSome c

class Forall a
instance Forall a
