{-# LANGUAGE Trustworthy #-}

{- |
 Module: PlutusTx.List.Ord
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental
 Functions for checking the sortedness of lists.
-}
module PlutusTx.List.Ord (
  isSorted,
  isSortedOn,
  isSortedMonotonic,
  isSortedMonotonicOn,
  isSortedBy,
) where

import PlutusTx.Prelude
import Prelude qualified ()

{-# INLINEABLE isSortedBy #-}

{- | Checks if the 'Foldable' is sorted with respect to a total order
represented by a binary relation.

@since 1.0
-}
isSortedBy :: Foldable f => (a -> a -> Bool) -> f a -> Bool
isSortedBy f = snd . foldr go (Nothing, True)
  where
    go x (Nothing, _) = (Just x, True)
    go x (Just y, b) = (Just x, b /\ f x y)

{-# INLINEABLE isSortedOn #-}

{- | Checks if the 'Foldable' is sorted in ascending order with respect to the
result of a key function.

@since 1.0
-}
isSortedOn :: (Foldable f, Ord b) => (a -> b) -> f a -> Bool
isSortedOn f = isSortedBy (\x y -> f x <= f y)

{-# INLINEABLE isSorted #-}

{- | Checks if the 'Foldable' is sorted in ascending order with respect to its
elements' 'Ord' instance.

@since 1.0
-}
isSorted :: (Foldable f, Ord a) => f a -> Bool
isSorted = isSortedBy (<=)

{-# INLINEABLE isSortedMonotonicOn #-}

{- | Checks if the 'Foldable' is sorted strictly in ascending order with respect
to the result of a key function.. This means any pair of adjacent elements
cannot be equal.

@since 1.0
-}
isSortedMonotonicOn :: (Foldable f, Ord b) => (a -> b) -> f a -> Bool
isSortedMonotonicOn f = isSortedBy (\x y -> f x < f y)

{-# INLINEABLE isSortedMonotonic #-}

{- | Checks if the 'Foldable' is sorted strictly in ascending order with respect
to its elements' 'Ord' instance. This means any pair of adjacent elements cannot
be equal.

@since 1.0
-}
isSortedMonotonic :: (Foldable f, Ord a) => f a -> Bool
isSortedMonotonic = isSortedBy (<)
