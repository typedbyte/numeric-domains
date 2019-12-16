-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Distance
-- Copyright   :  (c) Michael Szvetits, 2019
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports the types and functions needed to handle distances
-- between two domain values and to determine if two domain values are
-- adjacent.
-----------------------------------------------------------------------------
module Numeric.Distance
  ( Dist(..)
  , Distance(..)
  , adjacent
  ) where

-- base
import Data.Int (Int8, Int16, Int32, Int64)

-- | The distance between two elements of a domain is either finite (e.g., the
-- distance between the integers @3@ and @5@ is @2@), negative infinite (e.g.,
-- the distance between the double values @3.0@ and @1.0@) or positive infinite
-- (e.g., the distance between the double values @1.0@ and @3.0@).
--
-- The distance is used to determine if two interval bounds are adjacent, which
-- further allows to merge intervals and convert open to closed intervals based
-- on the used domain value type.
data Distance
  = NegativeInfinite
  | Finite Int
  | PositiveInfinite
  deriving (Eq, Ord)

-- | The class of domain value types for which a distance can be determined.
class Ord a => Dist a where
  distance :: a -> a -> Distance
  -- ^ Calculates the distance between two values.
  shift    :: Int -> a -> Maybe a
  -- ^ Determines a new value by trying to move a finite distance from a given
  -- value.
  --
  -- Returns 'Nothing' if the 'distance' to the new value is infinite.

instance Dist Int where
  distance m n = Finite (n - m)
  shift    m n = Just (m + n)

instance Dist Int8 where
  distance = integralDistance
  shift    = integralShift

instance Dist Int16 where
  distance = integralDistance
  shift    = integralShift

instance Dist Int32 where
  distance = integralDistance
  shift    = integralShift

instance Dist Int64 where
  distance = integralDistance
  shift    = integralShift

instance Dist Integer where
  distance m n = Finite (fromInteger (n - m))
  shift    m n = Just (toInteger m + n)

instance Dist Float where
  distance = uncountableDistance
  shift    = uncountableShift

instance Dist Double where
  distance = uncountableDistance
  shift    = uncountableShift

integralDistance :: Integral a => a -> a -> Distance
integralDistance m n = Finite (fromIntegral n - fromIntegral m)

integralShift :: Num a => Int -> a -> Maybe a
integralShift m n = Just (fromIntegral m + n)

uncountableDistance :: Ord a => a -> a -> Distance
uncountableDistance m n
  | m == n    = Finite 0
  | m < n     = PositiveInfinite
  | otherwise = NegativeInfinite

uncountableShift :: Int -> a -> Maybe a
uncountableShift 0 m = Just m
uncountableShift _ _ = Nothing

-- | Checks if two domain values are adjacent, i.e. their distance is finite @1@.
adjacent :: Dist a => a -> a -> Bool
adjacent a b = distance a b == Finite 1