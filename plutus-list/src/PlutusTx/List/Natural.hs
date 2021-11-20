{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Trustworthy #-}

{- |
 Module: PlutusTx.List.Natural
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental
 List functions that works with the 'Natural' type.
-}
module PlutusTx.List.Natural (
  length,
  replicate,
  take,
  drop,
  splitAt,
) where

import Data.Monoid (Sum (Sum, getSum))
import PlutusTx.Natural (Natural, nat)
import PlutusTx.Prelude hiding (length, take)
import Prelude qualified ()

{-# INLINEABLE length #-}

{- | Returns the size/length of a finite structure as a 'Natural'.

@since 1.0
-}
length :: Foldable f => f a -> Natural
length = getSum . foldMap (Sum . const [nat| 1 |])

{-# INLINEABLE replicate #-}

{- | @'replicate' n x@ is a list of length @n@ with @x@ the value of every
element.

@since 1.0
-}
replicate :: Natural -> a -> [a]
replicate [nat| 0 |] _ = []
replicate n x = x : replicate (pred n) x

-- [daylily]: these two are not morally "total" enough (uses 'pred'). How to
-- improve?
{-# INLINEABLE take #-}

{- | @'take' n xs@ returns the prefix of @xs@ of length @n@, or @xs@ itself if
@n '>=' 'length' xs@.

@since 1.0
-}
take :: Natural -> [a] -> [a]
take _ [] = []
take [nat| 0 |] _ = []
take n (x : xs) = x : take (pred n) xs

{-# INLINEABLE drop #-}

{- | @'drop' n xs@ returns the suffix of @xs@ after the first @n@ elements, or
@[]@ if @n '>=' 'length' xs@.

@since 1.0
-}
drop :: Natural -> [a] -> [a]
drop _ [] = []
drop [nat| 0 |] xs = xs
drop n (_ : xs) = drop (pred n) xs

{-# INLINEABLE splitAt #-}

{- | @'splitAt' n xs@ returns a tuple where first element is @xs@ prefix of
 length @n@ and second element is the remainder of the list.

 @since 1.0
-}
splitAt :: Natural -> [a] -> ([a], [a])
splitAt _ [] = ([], [])
splitAt [nat| 0 |] xs = ([], xs)
splitAt n (x : xs) =
  let (xs', xs'') = splitAt (pred n) xs in (x : xs', xs'')
