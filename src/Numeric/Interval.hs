-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Interval
-- Copyright   :  (c) Michael Szvetits, 2019
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports the types and functions needed to construct, combine
-- and process non-empty numeric intervals.
-- 
-- Package users might want to use the module "Numeric.Domain" instead, since
-- it provides a convenient abstraction for empty and multiple intervals.
-----------------------------------------------------------------------------
module Numeric.Interval
  (
  -- * Core Types
    Interval
  , lowerBound
  , upperBound
  -- * Interval Construction
  , singleton
  , interval
  , lowerBounded
  , upperBounded
  , maxInterval
  -- * Interval Combination
  , difference
  , intersect
  , merge
  -- * Interval Predicates
  , member
  , isSingleton
  , isInfinite
  , hasNegatives
  , hasPositives
  , contains
  -- * Interval Values
  , elems
  , minValue
  , maxValue
  -- * Interval Arithmetic
  , plus
  , minus
  , times
  , div
  , abs
  , negate
  -- * Interval Presentation
  , pretty
  ) where

-- base
import Data.Maybe (isJust)
import Prelude hiding (abs, div, isInfinite, negate)

import qualified Numeric.Bound as B
import Numeric.Distance (Dist)

-- | A numeric interval is a non-empty, possibly infinite range of values.
-- 
-- Note that rounding errors due to floating point arithmetic are not handled
-- at the lower and upper bounds of intervals.
data Interval a = Interval
  { lowerBound :: B.LowerBound a -- ^ Returns the lower bound of an interval.
  , upperBound :: B.UpperBound a -- ^ Returns the upper bound of an interval.
  }
  deriving (Eq, Show)

-- | Creates an interval with a single element.
singleton :: a -> Interval a
singleton value =
  Interval (B.closedLower value) (B.closedUpper value)

-- | Creates an interval with the given lower and upper bounds.
--
-- Returns 'Nothing' if the lower bound is greater than the upper bound.
interval :: Ord a => B.LowerBound a -> B.UpperBound a -> Maybe (Interval a)
interval lower upper
  | lower `B.isAbove` upper = Nothing
  | otherwise               = Just (Interval lower upper)

-- | Creates an interval with the given lower bound and a positive infinite
-- upper bound.
lowerBounded :: B.LowerBound a -> Interval a
lowerBounded lb = Interval lb B.infiniteUpper

-- | Creates an interval with the given upper bound and a negative infinite
-- lower bound.
upperBounded :: B.UpperBound a -> Interval a
upperBounded ub = Interval B.infiniteLower ub

-- | Creates an interval with values ranging from negative infinity to positive
-- infinity.
maxInterval :: Interval a
maxInterval = Interval B.infiniteLower B.infiniteUpper

-- | @contains whole sub@ checks if the interval @sub@ is contained in the
-- interval @whole@.
--
-- Returns 'True' if the two intervals are the same.
contains :: Ord a => Interval a -> Interval a -> Bool
contains (Interval xl xh) (Interval yl yh) =
  xl <= yl && yh <= xh

-- | Checks if an interval has a lower bound of negative infinity or an upper
-- bound of positive infinity.
isInfinite :: Interval a -> Bool
isInfinite iv =
  B.isInfiniteLower (lowerBound iv) ||
  B.isInfiniteUpper (upperBound iv)

-- | Checks if an interval contains exactly one element.
isSingleton :: Eq a => Interval a -> Bool
isSingleton iv =
  B.isSingleton (lowerBound iv) (upperBound iv)

-- | Checks if an interval contains negative values.
hasNegatives :: (Num a, Ord a) => Interval a -> Bool
hasNegatives = B.isNegativeLower . lowerBound

-- | Checks if an interval contains positive values.
hasPositives :: (Num a, Ord a) => Interval a -> Bool
hasPositives = B.isPositiveUpper . upperBound

-- | Checks if an interval contains a specific value.
member :: Ord a => a -> Interval a -> Bool
member value iv =
  B.containsLower value (lowerBound iv) &&
  B.containsUpper value (upperBound iv)

-- | Enumerates all elements of an interval in ascending order according to the
-- 'Enum' implementation of the interval value type.
--
-- Returns 'Nothing' if any of the two interval bounds is unbounded (see
-- 'isInfinite').
elems :: Enum a => Interval a -> Maybe [a]
elems iv = B.elems (lowerBound iv) (upperBound iv)

-- | Calculates the intersection of two intervals, i.e. @intersect ix iy@
-- contains all elements that are in both intervals @ix@ and @iy@.
--
-- Returns 'Nothing' if the intervals do not overlap.
intersect :: Ord a => Interval a -> Interval a -> Maybe (Interval a)
intersect (Interval xl xh) (Interval yl yh)
  | yl `B.isAbove` xh || yh `B.isBelow` xl = Nothing
  | otherwise =
      Just $ Interval (max xl yl) (min xh yh)

-- | Calculates the difference between two intervals, i.e. @difference whole
-- diff@ returns the intervals which contain all elements of @whole@ that are
-- not in @diff@.
--
-- Returns the empty list if @diff@ contains @whole@.
--
-- Returns a singleton list if @whole@ and @diff@ overlap on one of their
-- bounds.
--
-- Returns a list with two intervals if @whole@ contains @diff@.
difference :: Dist a => Interval a -> Interval a -> [Interval a]
difference whole@(Interval low high) diff =
  case intersect whole diff of
    Nothing -> [whole]
    Just di ->
      let lowInterval = do
            adjHigh <- B.adjacentUpper (lowerBound di)
            interval low adjHigh
          upInterval = do
            adjLow <- B.adjacentLower (upperBound di)
            interval adjLow high
      in
      case (lowInterval, upInterval) of
        (Just li, Just ui) -> [li, ui]
        (Just li, Nothing) -> [li]
        (Nothing, Just ui) -> [ui]
        (Nothing, Nothing) -> []

-- | Merges two intervals into one if they overlap or if they have adjacent
-- bounds.
--
-- Note that adjacency depends on the type of the interval values, e.g. the
-- intervals @[1,3]@ and @[4,6]@ can be merged to @[1,6]@ for @Interval Int@,
-- but not for @Interval Double@.
--
-- Returns 'Nothing' if the intervals do not overlap or if their bounds are not
-- adjacent.
merge :: Dist a => Interval a -> Interval a -> Maybe (Interval a)
merge l@(Interval xl xh) r@(Interval yl yh)
  | isJust (intersect l r) = Just $ Interval (min xl yl) (max xh yh)
  | B.mergeable xh yl      = Just $ Interval xl yh
  | B.mergeable yh xl      = Just $ Interval yl xh
  | otherwise              = Nothing

-- | Returns the greatest value of an interval.
--
-- Only succeeds if the upper bound is closed.
maxValue :: Interval a -> Maybe a
maxValue = B.maxValue . upperBound

-- | Returns the smallest value of an interval.
--
-- Only succeeds if the lower bound is closed.
minValue :: Interval a -> Maybe a
minValue = B.minValue . lowerBound

-- | Adds the value ranges of two intervals.
plus :: Num a => Interval a -> Interval a -> Interval a
plus (Interval xl xh) (Interval yl yh) =
  Interval
    (xl `B.plusLower` yl)
    (xh `B.plusUpper` yh)

-- | Subtracts the value ranges of two intervals.
minus :: Num a => Interval a -> Interval a -> Interval a
minus (Interval xl xh) (Interval yl yh) =
  Interval
    (xl `B.minusUpper` yh)
    (xh `B.minusLower` yl)

-- | Multiplies the value ranges of two intervals.
times :: (Num a, Ord a) => Interval a -> Interval a -> Interval a
times (Interval xl xh) (Interval yl yh) =
  case result of
    (Just low, Just up) -> Interval low up
    (Just low, Nothing) -> lowerBounded low
    (Nothing , Just up) -> upperBounded up
    (Nothing , Nothing) -> maxInterval
  where
    ll     = xl `B.timesLower` yl
    lh     = xl `B.timesMixed` yh
    hl     = yl `B.timesMixed` xh
    hh     = xh `B.timesUpper` yh
    temp1  = B.applyResult (Nothing, Nothing) ll
    temp2  = B.applyResult temp1 lh
    temp3  = B.applyResult temp2 hl
    result = B.applyResult temp3 hh

-- | Divides the value ranges of two intervals using integer division.
--
-- Returns 'maxInterval' if @0@ is a member of the divisor interval.
div :: Integral a => Interval a -> Interval a -> Interval a
div (Interval xl xh) r@(Interval yl yh)
  | member 0 r = maxInterval
  | otherwise  =
      case result of
        (Just low, Just up) -> Interval low up
        (Just low, Nothing) -> lowerBounded low
        (Nothing , Just up) -> upperBounded up
        (Nothing , Nothing) -> maxInterval
      where
        ll     = xl `B.divLower` yl
        lh     = xl `B.divLowerUpper` yh
        hl     = xh `B.divUpperLower` yl
        hh     = xh `B.divUpper` yh
        temp1  = B.applyResult (Nothing, Nothing) ll
        temp2  = B.applyResult temp1 lh
        temp3  = B.applyResult temp2 hl
        result = B.applyResult temp3 hh

-- | Negates the value range of an interval.
negate :: Num a => Interval a -> Interval a
negate (Interval low high) =
  Interval
    (B.negateUpper high)
    (B.negateLower low)

-- | Calculates the abs function of an interval and their corresponding values.
abs :: (Num a, Ord a) => Interval a -> Interval a
abs r@(Interval low high)
  | B.isNegativeLower low =
      Interval
        (max (B.closedLower 0) (B.negateUpper high))
        (max (B.negateLower low) high)
  | otherwise = r

-- | Returns a pretty string for an interval by using Unicode symbols for
-- infinity.
pretty :: Show a => Interval a -> String
pretty (Interval low high) =
  B.prettyLower low ++ "," ++ B.prettyUpper high