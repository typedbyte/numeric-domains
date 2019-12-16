-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Bound
-- Copyright   :  (c) Michael Szvetits, 2019
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports the types and functions needed to construct, combine
-- and process lower and upper bounds of numeric intervals.
-- 
-- Package users might want to use the module "Numeric.Domain" instead, since
-- it provides a convenient abstraction for intervals and their bounds.
-----------------------------------------------------------------------------
module Numeric.Bound
  (
  -- * Core Types
    LowerBound
  , UpperBound
  -- * Bound Construction
  , closedLower
  , closedUpper
  , openLower
  , openUpper
  , infiniteLower
  , infiniteUpper
  -- * Bound Combination
  , adjacentLower
  , adjacentUpper
  -- * Bound Predicates
  , mergeable
  , isAbove
  , isBelow
  , isSingleton
  , isInfiniteLower
  , isInfiniteUpper
  , isPositiveLower
  , isPositiveUpper
  , isNegativeLower
  , isNegativeUpper
  , isZeroLower
  , isZeroUpper
  , containsLower
  , containsUpper
  -- * Bound Values
  , elems
  , minValue
  , maxValue
  -- * Bound Arithmetic
  , Result(..)
  , applyResult
  , plusLower
  , plusUpper
  , minusLower
  , minusUpper
  , timesLower
  , timesUpper
  , timesMixed
  , divLower
  , divUpper
  , divLowerUpper
  , divUpperLower
  , recipLower
  , recipUpper
  , negateLower
  , negateUpper
  -- * Bound Presentation
  , prettyLower
  , prettyUpper
  ) where

import Numeric.Distance (Dist, adjacent, shift)

data Bound a
  = Open a
  | Closed a
  | Infinite
  deriving (Eq, Show)

-- | A special type for representing the result of bound multiplications and
-- divisions. These operations can either yield a lower bound, an upper bound
-- or both.
data Result a
  = Lower (LowerBound a)
  | Upper (UpperBound a)
  | Both (LowerBound a) (UpperBound a)
  deriving (Eq, Show)

-- | The lower bound of an interval. Can be open, closed or negative infinity.
newtype LowerBound a = LowerBound { unLowerBound :: Bound a }
  deriving (Eq, Show)

instance Ord a => Ord (LowerBound a) where
  compare (LowerBound x) (LowerBound y) =
    case (x,y) of
      (Open a, Closed b)   | a >= b       -> GT
                           | otherwise    -> LT
      (Closed a, Open b)   | a <= b       -> LT
                           | otherwise    -> GT
      (Open a, Open b)                    -> compare a b
      (Closed a, Closed b)                -> compare a b
      (Infinite, Infinite)                -> EQ
      (_, Infinite)                       -> GT
      (Infinite, _)                       -> LT

-- | The upper bound of an interval. Can be open, closed or positive infinity.
newtype UpperBound a = UpperBound { unUpperBound :: Bound a }
  deriving (Eq, Show)

instance Ord a => Ord (UpperBound a) where
  compare (UpperBound x) (UpperBound y) =
    case (x,y) of
      (Open a, Closed b)   | a <= b    -> LT
                           | otherwise -> GT
      (Closed a, Open b)   | a >= b    -> GT
                           | otherwise -> LT
      (Open a, Open b)                 -> compare a b
      (Closed a, Closed b)             -> compare a b
      (Infinite, Infinite)             -> EQ
      (_, Infinite)                    -> LT
      (Infinite, _)                    -> GT

-- | Creates an open lower bound. Internally, the bound is converted to a
-- closed one, if possible.
openLower :: Dist a => a -> LowerBound a
openLower = closeLower . LowerBound . Open

-- | Creates a closed lower bound.
closedLower :: a -> LowerBound a
closedLower = LowerBound . Closed

-- | Creates a negative infinite lower bound.
infiniteLower :: LowerBound a
infiniteLower = LowerBound Infinite

-- | Creates an open upper bound. Internally, the bound is converted to a
-- closed one, if possible.
openUpper :: Dist a => a -> UpperBound a
openUpper = closeUpper . UpperBound . Open

-- | Creates a closed upper bound.
closedUpper :: a -> UpperBound a
closedUpper = UpperBound . Closed

-- | Creates a positive infinite upper bound.
infiniteUpper :: UpperBound a
infiniteUpper = UpperBound Infinite

closeLower :: Dist a => LowerBound a -> LowerBound a
closeLower b@(LowerBound (Open a)) =
  case shift 1 a of
    Just sa -> closedLower sa
    Nothing -> b
closeLower b = b

closeUpper :: Dist a => UpperBound a -> UpperBound a
closeUpper b@(UpperBound (Open a)) =
  case shift (-1) a of
    Just sa -> closedUpper sa
    Nothing -> b
closeUpper b = b

-- | Checks if a lower bound contains a specific value, i.e. if the value is
-- equal to (if closed) or greater than (if open or closed) the lower bound.
containsLower :: Ord a => a -> LowerBound a -> Bool
containsLower value bound =
  case unLowerBound bound of
    Open a   -> value > a
    Closed a -> value >= a
    Infinite -> True

-- | Checks if an upper bound contains a specific value, i.e. if the value is
-- equal to (if closed) or less than (if open or closed) the upper bound.
containsUpper :: Ord a => a -> UpperBound a -> Bool
containsUpper value bound =
  case unUpperBound bound of
    Open a   -> value < a
    Closed a -> value <= a
    Infinite -> True

-- | Checks if a lower bound is negative infinity.
isInfiniteLower :: LowerBound a -> Bool
isInfiniteLower (LowerBound Infinite) = True
isInfiniteLower _                     = False

-- | Checks if an upper bound is positive infinity.
isInfiniteUpper :: UpperBound a -> Bool
isInfiniteUpper (UpperBound Infinite) = True
isInfiniteUpper _                     = False

-- | Checks if a lower bound is negative.
isNegativeLower :: (Num a, Ord a) => LowerBound a -> Bool
isNegativeLower bound =
  case unLowerBound bound of
    Open a   -> a < 0
    Closed a -> a < 0
    Infinite -> True

-- | Checks if an upper bound is negative.
isNegativeUpper :: (Num a, Ord a) => UpperBound a -> Bool
isNegativeUpper bound =
  case unUpperBound bound of
    Open a   -> a <= 0
    Closed a -> a < 0
    Infinite -> False

-- | Checks if a lower bound is positive.
isPositiveLower :: (Num a, Ord a) => LowerBound a -> Bool
isPositiveLower bound =
  case unLowerBound bound of
    Open a   -> a >= 0
    Closed a -> a > 0
    Infinite -> False

-- | Checks if an upper bound is positive.
isPositiveUpper :: (Num a, Ord a) => UpperBound a -> Bool
isPositiveUpper bound =
 case unUpperBound bound of
    Open a   -> a > 0
    Closed a -> a > 0
    Infinite -> True

isClosedZero :: (Eq a, Num a) => Bound a -> Bool
isClosedZero (Closed 0) = True
isClosedZero _          = False

isClosedZeroLower :: (Eq a, Num a) => LowerBound a -> Bool
isClosedZeroLower = isClosedZero . unLowerBound

isClosedZeroUpper :: (Eq a, Num a) => UpperBound a -> Bool
isClosedZeroUpper = isClosedZero . unUpperBound

isOpenZero :: (Eq a, Num a) => Bound a -> Bool
isOpenZero (Open 0) = True
isOpenZero _        = False

isZero :: (Eq a, Num a) => Bound a -> Bool
isZero bound = isClosedZero bound || isOpenZero bound

-- | Checks if a lower bound is zero. It is ignored if the bound is open or
-- closed.
isZeroLower :: (Eq a, Num a) => LowerBound a -> Bool
isZeroLower = isZero . unLowerBound

-- | Checks if an upper bound is zero. It is ignored if the bound is open or
-- closed.
isZeroUpper :: (Eq a, Num a) => UpperBound a -> Bool
isZeroUpper = isZero . unUpperBound

-- | Checks if a lower bound is above an upper bound.
isAbove :: Ord a => LowerBound a -> UpperBound a -> Bool
isAbove (LowerBound x) (UpperBound y) =
  case (x,y) of
    (Open a, Open b)     -> a >= b
    (Open a, Closed b)   -> a >= b
    (Closed a, Open b)   -> a >= b
    (Closed a, Closed b) -> a > b
    _                    -> False

-- | Checks if an upper bound is below a lower bound.
isBelow :: Ord a => UpperBound a -> LowerBound a -> Bool
isBelow = flip isAbove

-- | Checks if a lower bound and an upper bound contain exactly one element.
isSingleton :: Eq a => LowerBound a -> UpperBound a -> Bool
isSingleton (LowerBound (Closed a)) (UpperBound (Closed b)) = a == b
isSingleton _ _ = False

-- | Enumerates all elements between a lower bound and an upper bound in
-- ascending order according to the 'Enum' implementation of their value type.
--
-- Returns 'Nothing' if any of the two bounds is infinite.
elems :: Enum a => LowerBound a -> UpperBound a -> Maybe [a]
elems (LowerBound x) (UpperBound y) =
  case (x,y) of
    (Open a, Open b)     -> Just [succ a .. pred b]
    (Open a, Closed b)   -> Just [succ a .. b]
    (Closed a, Open b)   -> Just [a .. pred b]
    (Closed a, Closed b) -> Just [a .. b]
    _                    -> Nothing

-- | Creates a lower bound that is adjacent to (i.e., directly above) a given
-- upper bound.
--
-- Returns 'Nothing' if the upper bound is positive infinity.
adjacentLower :: Dist a => UpperBound a -> Maybe (LowerBound a)
adjacentLower bound =
  case unUpperBound bound of
    Open a   -> Just $ LowerBound (Closed a)
    Closed a -> Just . closeLower $ LowerBound (Open a)
    Infinite -> Nothing

-- | Creates an upper bound that is adjacent to (i.e., directly below) a given
-- lower bound.
--
-- Returns 'Nothing' if the lower bound is negative infinity.
adjacentUpper :: Dist a => LowerBound a -> Maybe (UpperBound a)
adjacentUpper bound =
  case unLowerBound bound of
    Open a   -> Just (closedUpper a)
    Closed a -> Just (openUpper a)
    Infinite -> Nothing

-- | Adds the values of two lower bounds.
plusLower :: Num a => LowerBound a -> LowerBound a -> LowerBound a
plusLower (LowerBound x) (LowerBound y) =
  LowerBound (apply (+) x y)

-- | Adds the values of two upper bounds.
plusUpper :: Num a => UpperBound a -> UpperBound a -> UpperBound a
plusUpper (UpperBound x) (UpperBound y) =
  UpperBound (apply (+) x y)

-- | Subtracts the value of a lower bound from the value of an upper bound.
minusLower :: Num a => UpperBound a -> LowerBound a -> UpperBound a
minusLower (UpperBound x) (LowerBound y) =
  UpperBound (apply (-) x y)

-- | Subtracts the value of an upper bound from the value of a lower bound.
minusUpper :: Num a => LowerBound a -> UpperBound a -> LowerBound a
minusUpper (LowerBound x) (UpperBound y) =
  LowerBound (apply (-) x y)

apply :: (a -> a -> a) -> Bound a -> Bound a -> Bound a
apply f x y =
  case (x,y) of
    (Open a, Open b)     -> Open (f a b)
    (Open a, Closed b)   -> Open (f a b)
    (Closed a, Open b)   -> Open (f a b)
    (Closed a, Closed b) -> Closed (f a b)
    _                    -> Infinite

-- | Multiplies the values of two lower bounds.
timesLower :: (Num a, Ord a) => LowerBound a -> LowerBound a -> Result a
timesLower l@(LowerBound x) r@(LowerBound y) =
  case (x,y) of
    (Open a, Open b)     -> bothOpen (a * b)
    (Open a, Closed b)   -> bothOpen (a * b)
    (Closed a, Open b)   -> bothOpen (a * b)
    (Closed a, Closed b) -> bothClosed (a * b)
    (Infinite, _)        -> inf r
    (_, Infinite)        -> inf l
  where
    inf b | isNegativeLower b   = Upper infiniteUpper
          | isClosedZeroLower b = Both (closedLower 0) (closedUpper 0)
          | otherwise           = Lower infiniteLower

-- | Multiplies the values of two upper bounds.
timesUpper :: (Num a, Ord a) => UpperBound a -> UpperBound a -> Result a
timesUpper l@(UpperBound x) r@(UpperBound y) =
  case (x,y) of
    (Open a, Open b)     -> bothOpen (a * b)
    (Open a, Closed b)   -> bothOpen (a * b)
    (Closed a, Open b)   -> bothOpen (a * b)
    (Closed a, Closed b) -> bothClosed (a * b)
    (Infinite, _)        -> inf r
    (_, Infinite)        -> inf l
  where
    inf b | isPositiveUpper b   = Upper infiniteUpper
          | isClosedZeroUpper b = Both (closedLower 0) (closedUpper 0)
          | otherwise           = Lower infiniteLower

-- | Multiplies the value of a lower bound and the value of an upper bound.
timesMixed :: (Num a, Ord a) => LowerBound a -> UpperBound a -> Result a
timesMixed l@(LowerBound x) r@(UpperBound y) =
  case (x,y) of
    (Open a, Open b)     -> bothOpen (a * b)
    (Open a, Closed b)   -> bothOpen (a * b)
    (Closed a, Open b)   -> bothOpen (a * b)
    (Closed a, Closed b) -> bothClosed (a * b)
    (Infinite, _)        -> infLower r
    (_, Infinite)        -> infUpper l
  where
    infLower b | isPositiveUpper b   = Lower infiniteLower
               | isClosedZeroUpper b = Both (closedLower 0) (closedUpper 0)
               | otherwise           = Upper infiniteUpper
    infUpper b | isNegativeLower b   = Lower infiniteLower
               | isClosedZeroLower b = Both (closedLower 0) (closedUpper 0)
               | otherwise           = Upper infiniteUpper

-- | Divides the values of two lower bounds using integer division.
--
-- Note that it is not checked if the divisor bound is zero, which yields an
-- error. Such a check must happen before calling this function.
divLower :: Integral a => LowerBound a -> LowerBound a -> Result a
divLower (LowerBound x) r@(LowerBound y) =
  case (x,y) of
    (Open a, Open b)     -> bothOpen   (a `div` b)
    (Open a, Closed b)   -> bothOpen   (a `div` b)
    (Closed a, Open b)   -> bothOpen   (a `div` b)
    (Closed a, Closed b) -> bothClosed (a `div` b)
    (Infinite, _)        -> inf r
    (_, Infinite)        -> bothClosed 0
  where
    inf b | isNegativeLower b = Upper infiniteUpper
          | otherwise         = Lower infiniteLower

-- | Divides the values of two upper bounds using integer division.
--
-- Note that it is not checked if the divisor bound is zero, which yields an
-- error. Such a check must happen before calling this function.
divUpper :: Integral a => UpperBound a -> UpperBound a -> Result a
divUpper (UpperBound x) r@(UpperBound y) =
  case (x,y) of
    (Open a, Open b)     -> bothOpen   (a `div` b)
    (Open a, Closed b)   -> bothOpen   (a `div` b)
    (Closed a, Open b)   -> bothOpen   (a `div` b)
    (Closed a, Closed b) -> bothClosed (a `div` b)
    (Infinite, _)        -> inf r
    (_, Infinite)        -> bothClosed 0
  where
    inf b | isNegativeUpper b = Lower infiniteLower
          | otherwise         = Upper infiniteUpper

-- | Divides the values of a lower bound and an upper bound using integer
-- division.
--
-- Note that it is not checked if the divisor bound is zero, which yields an
-- error. Such a check must happen before calling this function.
divLowerUpper :: Integral a => LowerBound a -> UpperBound a -> Result a
divLowerUpper (LowerBound x) r@(UpperBound y) =
  case (x,y) of
    (Open a, Open b)     -> bothOpen   (a `div` b)
    (Open a, Closed b)   -> bothOpen   (a `div` b)
    (Closed a, Open b)   -> bothOpen   (a `div` b)
    (Closed a, Closed b) -> bothClosed (a `div` b)
    (Infinite, _)        -> inf r
    (_, Infinite)        -> bothClosed 0
  where
    inf b | isNegativeUpper b = Upper infiniteUpper
          | otherwise         = Lower infiniteLower

-- | Divides the values of an upper bound and a lower bound using integer
-- division.
--
-- Note that it is not checked if the divisor bound is zero, which yields an
-- error. Such a check must happen before calling this function.
divUpperLower :: Integral a => UpperBound a -> LowerBound a -> Result a
divUpperLower (UpperBound x) r@(LowerBound y) =
  case (x,y) of
    (Open a, Open b)     -> bothOpen   (a `div` b)
    (Open a, Closed b)   -> bothOpen   (a `div` b)
    (Closed a, Open b)   -> bothOpen   (a `div` b)
    (Closed a, Closed b) -> bothClosed (a `div` b)
    (Infinite, _)        -> inf r
    (_, Infinite)        -> bothClosed 0
  where
    inf b | isNegativeLower b = Lower infiniteLower
          | otherwise         = Upper infiniteUpper

bothClosed :: a -> Result a
bothClosed value =
  Both (closedLower value) (closedUpper value)

bothOpen :: a -> Result a
bothOpen value =
  Both (LowerBound (Open value)) (UpperBound (Open value))

-- | Given a possible lower bound and upper bound as well as a result from a
-- bound multiplication or division, this function updates the bounds depending
-- on whether the result was a lower bound, upper bound or both.
--
-- The lower bound is updated by favouring the smaller one, while the upper
-- bound is updated by favouring the greater one.
applyResult
  :: Ord a
  => (Maybe (LowerBound a), Maybe (UpperBound a))
  -> Result a
  -> (Maybe (LowerBound a), Maybe (UpperBound a))
applyResult (nowMin,nowMax) result =
  case result of
    Both low up -> (update min nowMin low, update max nowMax up)
    Lower low   -> (update min nowMin low, nowMax)
    Upper up    -> (nowMin, update max nowMax up)
  where
    update f current new =
      Just $
        case current of
          Just m  -> f m new
          Nothing -> new

negateBound :: Num a => Bound a -> Bound a
negateBound (Open a)   = Open (negate a)
negateBound (Closed a) = Closed (negate a)
negateBound Infinite   = Infinite

-- | Negates a lower bound and transforms it into an upper bound.
negateLower :: Num a => LowerBound a -> UpperBound a
negateLower = UpperBound . negateBound . unLowerBound

-- | Negates an upper bound and transforms it into a lower bound.
negateUpper :: Num a => UpperBound a -> LowerBound a
negateUpper = LowerBound . negateBound . unUpperBound

recipBound :: Fractional a => Bound a -> Bound a
recipBound (Open a)   = Open (recip a)
recipBound (Closed a) = Closed (recip a)
recipBound Infinite   = Closed 0

-- | Calculates the reciprocal of an upper bound and transforms it into a lower
-- bound.
recipUpper :: Fractional a => UpperBound a -> LowerBound a
recipUpper = LowerBound . recipBound . unUpperBound

-- | Calculates the reciprocal of a lower bound and transforms it into an upper
-- bound.
recipLower :: Fractional a => LowerBound a -> UpperBound a
recipLower = UpperBound . recipBound . unLowerBound

-- | Checks if an upper bound and a lower bound are mergeable, i.e. if they
-- overlap or if they are adjacent to each other.
--
-- Note that adjacency depends on the type of the bound values, e.g. the
-- upper bound @[...,3]@ and lower bound @[4,...]@ can be merged for
-- @Interval Int@, but not for @Interval Double@.
--
-- Infinite bounds (negative infinity, positive infinity) are never mergeable.
mergeable :: Dist a => UpperBound a -> LowerBound a -> Bool
mergeable (UpperBound x) (LowerBound y) =
  case (x,y) of
    (Open a, Closed b)   -> a == b
    (Closed a, Open b)   -> a == b
    (Closed a, Closed b) -> a == b || adjacent a b
    _                    -> False

-- | Returns the greatest value of an upper bound.
--
-- Only succeeds if the upper bound is closed.
maxValue :: UpperBound a -> Maybe a
maxValue bound =
  case unUpperBound bound of
    Closed a -> Just a
    _        -> Nothing

-- | Returns the smallest value of a lower bound.
--
-- Only succeeds if the lower bound is closed.
minValue :: LowerBound a -> Maybe a
minValue bound =
  case unLowerBound bound of
    Closed a -> Just a
    _        -> Nothing

-- | Returns a pretty string for a lower bound by using the Unicode symbol for
-- negative infinity.
prettyLower :: Show a => LowerBound a -> String
prettyLower bound =
  case unLowerBound bound of
    Open a -> "(" ++ show a
    Closed a -> "[" ++ show a
    Infinite -> "(-\8734"

-- | Returns a pretty string for an upper bound by using the Unicode symbol for
-- positive infinity.
prettyUpper :: Show a => UpperBound a -> String
prettyUpper bound =
  case unUpperBound bound of
    Open a -> show a ++ ")"
    Closed a -> show a ++ "]"
    Infinite -> "\8734)"