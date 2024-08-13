-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Domain
-- Copyright   :  (c) Michael Szvetits, 2019
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports the types and functions needed to construct, combine
-- and process numeric domains.
-----------------------------------------------------------------------------
module Numeric.Domain
  (
  -- * Core Types
    Domain
  , intervals
  -- * Domain Construction
  , empty
  , singleton
  , interval
  , greaterThan
  , greaterOrEqual
  , lessThan
  , lessOrEqual
  , maxDomain
  , (<..<)
  , (<=..<)
  , (<..<=)
  , (<=..<=)
  -- * Domain Combination
  , difference
  , intersect
  , union
  , greaterThanDomain
  , greaterOrEqualDomain
  , lessThanDomain
  , lessOrEqualDomain
  , notEqual
  -- * Domain Predicates
  , null
  , isSingleton
  , isInfinite
  , isSubsetOf
  , sameElements
  -- * Domain Values
  , elems
  , member
  , maxValue
  , minValue
  -- * Domain Arithmetic
  -- | Note that the type 'Domain' implements the 'Num' and 'Fractional' type
  -- classes for additional arithmetic operations.
  , div
  , inverseAbs
  , inverseSignum
  -- * Domain Presentation
  , pretty
  , putPrettyLn
  ) where

-- base
import Data.List  (find, intercalate, sortBy)
import Data.Maybe (maybeToList)
import Data.Ord   (comparing)
import Prelude hiding (div, isInfinite, null)

import qualified Numeric.Bound as B
import qualified Numeric.Interval as I
import Numeric.Distance (Dist)

-- | A numeric domain is a union of zero or more non-empty, non-overlapping,
-- open or closed numeric 'I.Interval's.
-- 
-- Note that rounding errors due to floating point arithmetic are not handled
-- at the lower and upper bounds of intervals.
newtype Domain a = Domain
  { intervals :: [I.Interval a]
    -- ^ Returns the non-empty, non-overlapping intervals of a numeric domain
    -- in no specific order.
    --
    -- Package users will most likely not need this, except for custom logic
    -- regarding intervals that is not offered here (e.g., pretty printing).
  }
  deriving (Eq, Show)

instance (Dist a, Fractional a) => Fractional (Domain a) where
  fromRational = singleton . fromRational
  recip        = Numeric.Domain.recip

instance (Dist a, Num a) => Num (Domain a) where
  (+)         = join I.plus
  (-)         = join I.minus
  (*)         = join I.times
  negate      = Domain . fmap I.negate . intervals
  abs         = Numeric.Domain.abs
  signum      = Numeric.Domain.signum
  fromInteger = singleton . fromInteger

-- | Creates an empty domain which contains no elements.
empty :: Domain a
empty = Domain []

-- | Creates a domain with a single element.
singleton :: a -> Domain a
singleton value =
  Domain [I.singleton value]

-- | Creates a domain with a single interval. If the lower bound is greater
-- than the upper bound, 'empty' is created.
interval :: Ord a => B.LowerBound a -> B.UpperBound a -> Domain a
interval low high =
  case I.interval low high of
    Nothing -> empty
    Just di -> Domain [di]

-- | Creates a domain with values ranging from negative infinity to an
-- exclusive upper bound.
lessThan :: Dist a => a -> Domain a
lessThan value =
  Domain [I.upperBounded (B.openUpper value)]

-- | Creates a domain with values ranging from negative infinity to an
-- inclusive upper bound.
lessOrEqual :: a -> Domain a
lessOrEqual value =
  Domain [I.upperBounded (B.closedUpper value)]

-- | Creates a domain with values ranging from an exclusive lower bound to
-- positive infinity.
greaterThan :: Dist a => a -> Domain a
greaterThan value =
  Domain [I.lowerBounded (B.openLower value)]

-- | Creates a domain with values ranging from an inclusive lower bound to
-- positive infinity.
greaterOrEqual :: a -> Domain a
greaterOrEqual value =
  Domain [I.lowerBounded (B.closedLower value)]

-- | Creates a domain with values ranging from negative infinity to positive
-- infinity.
maxDomain :: Domain a
maxDomain = Domain [I.maxInterval]

-- | Creates a domain with values between an exclusive lower bound and an
-- exclusive upper bound.
--
-- If the lower bound is greater than the upper bound, 'empty' is returned.
(<..<) :: Dist a => a -> a -> Domain a
(<..<) low high = interval (B.openLower low) (B.openUpper high)

-- | Creates a domain with values between an inclusive lower bound and an
-- exclusive upper bound.
--
-- If the lower bound is greater than the upper bound, 'empty' is returned.
(<=..<) :: Dist a => a -> a -> Domain a
(<=..<) low high = interval (B.closedLower low) (B.openUpper high)

-- | Creates a domain with values between an exclusive lower bound and an
-- inclusive upper bound.
--
-- If the lower bound is greater than the upper bound, 'empty' is returned.
(<..<=) :: Dist a => a -> a -> Domain a
(<..<=) low high = interval (B.openLower low) (B.closedUpper high)

-- | Creates a domain with values between an inclusive lower bound and an
-- inclusive upper bound.
--
-- If the lower bound is greater than the upper bound, 'empty' is returned.
(<=..<=) :: Ord a => a -> a -> Domain a
(<=..<=) low high = interval (B.closedLower low) (B.closedUpper high)

-- | Checks if any domain interval has a lower bound of negative infinity or an
-- upper bound of positive infinity.
isInfinite :: Domain a -> Bool
isInfinite (Domain is) = any I.isInfinite is

-- | Checks if a domain contains exactly one element.
isSingleton :: Eq a => Domain a -> Bool
isSingleton (Domain [i]) = I.isSingleton i
isSingleton _            = False

-- | Checks if a domain is a subset of another domain, i.e. @isSubsetOf sub
-- whole@ returns 'True' if the domain @whole@ contains the domain @sub@.
--
-- Equal domains are not considered as subsets.
isSubsetOf :: Ord a => Domain a -> Domain a -> Bool
isSubsetOf (Domain sub) (Domain whole) = go False 0 sub whole
  where
    go smaller len []     domain = smaller || len < length domain
    go smaller len (i:is) domain =
      case find (`I.contains` i) domain of
        Nothing -> False
        Just wi -> go (smaller || wi /= i) (len + 1) is domain

-- | Checks if a domain contains a specific value.
member :: Ord a => a -> Domain a -> Bool
member value (Domain is) =
  any (I.member value) is

-- | Checks if a domain is empty, i.e. contains no elements.
null :: Domain a -> Bool
null (Domain []) = True
null _           = False

-- | Checks if two domains contain exactly the same elements.
sameElements :: Ord a => Domain a -> Domain a -> Bool
sameElements (Domain xs) (Domain ys) =
  sortBy (comparing I.lowerBound) xs ==
  sortBy (comparing I.lowerBound) ys

-- | Enumerates all elements of a domain in no specific order according to the
-- 'Enum' implementation of the domain value type.
--
-- Returns 'Nothing' if any interval of the domain is unbounded (see
-- 'isInfinite').
elems :: Enum a => Domain a -> Maybe [a]
elems (Domain is) = go is
  where
    go []     = Just []
    go (i:rs) = do
      xs <- I.elems i
      ts <- go rs
      pure (xs ++ ts)

-- | Calculates the difference between two domains, i.e. @difference whole
-- diff@ contains all elements of the domain @whole@ that are not in the domain
-- @diff@.
difference :: Dist a => Domain a -> Domain a -> Domain a
difference (Domain whole) (Domain diff) = Domain (foldl go whole diff)
  where
    go []     _ = []
    go (r:rs) i =
      I.difference r i ++ go rs i

-- | Calculates the intersection of two domains, i.e. @intersect dx dy@
-- contains all elements that are in both domains @dx@ and @dy@.
intersect :: Ord a => Domain a -> Domain a -> Domain a
intersect dx dy = Domain $ do
  x <- intervals dx
  y <- intervals dy
  maybeToList (I.intersect x y)

-- | Calculates the union of two domains, i.e. @union dx dy@ contains all
-- elements that are either in domain @dx@ or domain @dy@.
union :: Dist a => Domain a -> Domain a -> Domain a
union (Domain ls) (Domain rs) = Domain (listUnion ls rs)
  where
    listUnion domain []     = domain
    listUnion domain (i:is) = listUnion (go domain i [] []) is
    
    go [] r remains merges =
      case merges of
        [] -> r : remains
        ms -> listUnion remains ms
    go (i:is) r remains merges =
      case I.merge i r of
        Just mi -> go is r remains (mi : merges)
        Nothing -> go is r (i : remains) merges

-- | Creates a domain whose values are less than the ones from another domain.
-- If the passed domain is empty, the result is empty.
lessThanDomain :: Dist a => Domain a -> Domain a
lessThanDomain domain =
  case maxValue domain of
    Just mv -> lessThan mv
    Nothing | null domain -> domain
            | otherwise   -> maxDomain

-- | Creates a domain whose values are less than or equal to the ones from
-- another domain. If the passed domain is empty, the result is empty.
lessOrEqualDomain :: Ord a => Domain a -> Domain a
lessOrEqualDomain domain =
  case maxValue domain of
    Just mv -> lessOrEqual mv
    Nothing | null domain -> domain
            | otherwise   -> maxDomain

-- | Creates a domain whose values are greater than the ones from another
-- domain. If the passed domain is empty, the result is empty.
greaterThanDomain :: Dist a => Domain a -> Domain a
greaterThanDomain domain =
  case minValue domain of
    Just mv -> greaterThan mv
    Nothing | null domain -> domain
            | otherwise   -> maxDomain

-- | Creates a domain whose values are greater than or equal to the ones from
-- another domain. If the passed domain is empty, the result is empty.
greaterOrEqualDomain :: Ord a => Domain a -> Domain a
greaterOrEqualDomain domain =
  case minValue domain of
    Just mv -> greaterOrEqual mv
    Nothing | null domain -> domain
            | otherwise   -> maxDomain

-- | Creates a domain whose values differ from the ones of another domain.
--
-- This can only happen if the passed domain is a singleton domain (see
-- 'isSingleton'). In all other cases, 'maxDomain' is returned.
notEqual :: Dist a => Domain a -> Domain a
notEqual domain
  | isSingleton domain = maxDomain `difference` domain
  | otherwise          = maxDomain

-- | Returns the greatest value of a domain if all of its intervals have a
-- non-infinite upper bound (see 'isInfinite').
maxValue :: Ord a => Domain a -> Maybe a
maxValue = aggregate I.maxValue max

-- | Returns the smallest value of a domain if all of its intervals have a
-- non-infinite lower bound (see 'isInfinite').
minValue :: Ord a => Domain a -> Maybe a
minValue = aggregate I.minValue min

aggregate :: (I.Interval a -> Maybe a) -> (a -> a -> a) -> Domain a -> Maybe a
aggregate mapping combine (Domain is) =
  case is of
    []   -> Nothing
    r:rs ->
      case mapping r of
        Nothing -> Nothing
        Just m  -> go m rs
      where
        go current []     = Just current
        go current (x:xs) =
          case mapping x of
            Nothing -> Nothing
            Just n  -> go (combine n current) xs

join
  :: Dist a
  => (I.Interval a -> I.Interval a -> I.Interval a)
  -> Domain a
  -> Domain a
  -> Domain a
join f (Domain xs) (Domain ys) =
  union empty (Domain is)
    where
      is = do
        x <- xs
        y <- ys
        pure (f x y)

abs :: (Dist a, Num a) => Domain a -> Domain a
abs (Domain is) =
  union empty (Domain ns)
    where
      ns = fmap I.abs is

signum :: (Dist a, Num a) => Domain a -> Domain a
signum (Domain is) =
  foldl union empty (fmap intervalSignum is)
  where
    intervalSignum r =
      let
        neg  | I.hasNegatives r = singleton (-1)
             | otherwise        = empty
        zero | I.member 0 r     = singleton 0
             | otherwise        = empty
        pos  | I.hasPositives r = singleton 1
             | otherwise        = empty
      in
        union neg (union zero pos)

recip :: (Dist a, Fractional a) => Domain a -> Domain a
recip (Domain is) =
  foldl union empty (fmap recipInterval is)
  where
    recipInterval r =
      if not hasZero
      then interval recipHigh recipLow
      else
        case (I.hasNegatives r, I.hasPositives r) of
          (True, True) ->
            let
              Domain li = interval B.infiniteLower recipLow
              Domain hi = interval recipHigh B.infiniteUpper
            in
              Domain (li ++ hi)
          (True , False) -> interval B.infiniteLower recipLow
          (False, True ) -> interval recipHigh B.infiniteUpper
          (False, False) -> empty
      where
        low       = I.lowerBound r
        high      = I.upperBound r
        recipLow  = B.recipLower low
        recipHigh = B.recipUpper high
        hasZero   =
          I.member 0 r      ||
          B.isZeroLower low || 
          B.isZeroUpper high

-- | Calculates the inverse abs function of a domain, i.e. positive values are
-- also mirrored to their negative counterparts.
inverseAbs :: (Dist a, Num a) => Domain a -> Domain a
inverseAbs domain = union positives negatives
  where
    positives = intersect domain (greaterOrEqual 0)
    negatives = negate positives

-- | Calculates the inverse signum function of a domain, i.e. if the domain
-- contains the values @(-1)@, @0@ and\/or @1@, the result is the union of
-- negative infinity, zero and\/or positive infinity, respectively.
inverseSignum :: (Dist a, Num a) => Domain a -> Domain a
inverseSignum (Domain is) =
  let
    neg  | any (I.member (-1)) is = lessThan 0
         | otherwise              = empty
    zero | any (I.member 0) is    = singleton 0
         | otherwise              = empty
    pos  | any (I.member 1) is    = greaterThan 0
         | otherwise              = empty
  in
    union neg (union zero pos)

-- | Calculates the integer division of two domains.
div :: (Dist a, Integral a) => Domain a -> Domain a -> Domain a
div = join I.div

-- | Returns a pretty string for a domain by sorting its intervals in ascending
-- order and using Unicode symbols for infinity and unions.
pretty :: (Ord a, Show a) => Domain a -> String
pretty (Domain is) =
  case sortBy (comparing I.lowerBound) is of
    [] -> "\8709"
    xs -> intercalate " \8746 " (fmap I.pretty xs)

-- | Same as 'pretty', but with an additional output to standard output.
putPrettyLn :: (Ord a, Show a) => Domain a -> IO ()
putPrettyLn = putStrLn . pretty