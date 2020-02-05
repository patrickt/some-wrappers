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
-- with the easier type @Some Show@. You can also wrap GADTs:
--
-- @
--   data Number a where
--     I :: Int -> Number Int
--     F :: Float -> Number Int
--
--   type SomeNumber = Show1 ((~) Number) Forall
-- @

module Data.Some
  ( Some (..)
  , Some1 (..)
  , Forall
  ) where

import Data.Aeson
import Data.Kind

-- | A polymorphic wrapper around a type of kind @*@.
data Some (c :: Type -> Constraint) where
  Some :: c a => a -> Some c

instance Show (Some Show) where
  showsPrec n (Some a) = showsPrec n a

instance ToJSON (Some ToJSON) where
  toJSON (Some a) = toJSON a
  toEncoding (Some a) = toEncoding a


-- | A polymorphic wrapper around a type taking a single parameter.
-- The first constraint represents those rendered on the container @f@,
-- and the second represents those on the constituent type @a@. If you
-- don't need any constraints on @a@, use 'Forall'.
--
-- The extra @k@ parameter to the constructor is to allow the kind of
-- the parameterized type to be poly-kinded in the input type. This
-- allows you to pass @(~) Proxy@ as the first argument and have some
-- type-level function as the second. In certain cases, you may need
-- to specify it as 'Data.Kind.Type' with a type application.
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

-- | A zero-methods typeclass used when you don't have any constraints
-- you want to levy on some datum. Useful as the second parameter to
-- Some1.
class Forall (x :: k)
instance Forall a
