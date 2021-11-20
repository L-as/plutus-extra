{-# LANGUAGE ViewPatterns #-}

import Data.Function (on)
import Data.Kind (Type)
import Data.List (nub, nubBy, sort, sortOn)
import PlutusTx.List.Natural qualified as Nat
import PlutusTx.List.Ord (
  isSorted,
  isSortedMonotonic,
  isSortedMonotonicOn,
  isSortedOn,
 )
import PlutusTx.Natural (Natural)
import PlutusTx.Prelude qualified as PTx
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Fun (Fun),
  Gen,
  Property,
  QuickCheckTests,
  Testable (property),
  checkCoverage,
  cover,
  forAllShrink,
  oneof,
  testProperty,
 )

main :: IO ()
main = defaultMain $ testGroup "Tests" tests

tests :: [TestTree]
tests =
  [ localOption go
      . testGroup "List functions with Natural"
      $ [ testProperty "length == PlutusTx.length" propLengthAgree
        , testProperty "take == PlutusTx.take" propTakeAgree
        , testProperty "take n xs ++ drop n xs == xs" propTakeDrop
        , testProperty "splitAt n xs == (take n xs, drop n xs)" propSplitAt
        , testProperty "length (replicate n x) == n" propReplicateLength
        , testProperty "forall y in (replicate n x), x == y" propReplicateElem
        ]
  , testGroup
      "List sortedness"
      [ testProperty "isSorted xs == (sort xs == xs)" propIsSorted
      , testProperty "isSortedMonotonic xs == (nub (sort xs) == xs)" propIsSortedMonotonic
      , testProperty "isSortedOn f xs == isSorted (f <$> xs)" propIsSortedOn
      , testProperty "isSortedMonotonicOn f xs == isSortedMonotonic (f <$> xs)" propIsSortedMonotonicOn
      ]
  ]
  where
    go :: QuickCheckTests
    go = 100000

-- This ensures at least 50% sorted lists.
genSomeSortedListOn :: (Arbitrary a, Ord b) => (a -> b) -> Gen (Either [a] [a])
genSomeSortedListOn f =
  oneof
    [ Left <$> arbitrary
    , Right . sortOn f <$> arbitrary
    ]

genSomeSortedList :: (Arbitrary a, Ord a) => Gen (Either [a] [a])
genSomeSortedList = genSomeSortedListOn id

shrinkSomeSortedListOn :: (Arbitrary a, Ord b) => (a -> b) -> Either [a] [a] -> [Either [a] [a]]
shrinkSomeSortedListOn f = \case
  Left xs -> Left <$> shrink xs
  Right xs -> Right . sortOn f <$> shrink xs

shrinkSomeSortedList :: (Arbitrary a, Ord a) => Either [a] [a] -> [Either [a] [a]]
shrinkSomeSortedList = shrinkSomeSortedListOn id

-- This ensures at least 50% lists in strict ascending order.
genSomeMonotonicListOn :: (Arbitrary a, Ord b) => (a -> b) -> Gen (Either [a] [a])
genSomeMonotonicListOn f =
  oneof
    [ Left <$> arbitrary
    , Right . nubBy ((==) `on` f) . sortOn f <$> arbitrary
    ]

genSomeMonotonicList :: (Arbitrary a, Ord a) => Gen (Either [a] [a])
genSomeMonotonicList = genSomeMonotonicListOn id

shrinkSomeMonotonicListOn :: (Arbitrary a, Ord b) => (a -> b) -> Either [a] [a] -> [Either [a] [a]]
shrinkSomeMonotonicListOn f = \case
  Left xs -> Left <$> shrink xs
  Right xs -> Right . nubBy ((==) `on` f) . sortOn f <$> shrink xs

shrinkSomeMonotonicList :: (Arbitrary a, Ord a) => Either [a] [a] -> [Either [a] [a]]
shrinkSomeMonotonicList = shrinkSomeMonotonicListOn id

undiag :: forall (a :: Type). Either a a -> a
undiag (Left x) = x
undiag (Right x) = x

propLengthAgree :: Property
propLengthAgree = property $ \(xs :: [Int]) ->
  PTx.fromEnum (Nat.length xs) == PTx.fromEnum (PTx.length xs)

propTakeAgree :: Property
propTakeAgree = property $ \(n :: Natural) (xs :: [Int]) ->
  Nat.take n xs == PTx.take (PTx.toEnum $ PTx.fromEnum n) xs

propTakeDrop :: Property
propTakeDrop = property $ \(n :: Natural) (xs :: [Int]) ->
  Nat.take n xs <> Nat.drop n xs == xs

propSplitAt :: Property
propSplitAt = property $ \(n :: Natural) (xs :: [Int]) ->
  Nat.splitAt n xs == (Nat.take n xs, Nat.drop n xs)

propReplicateLength :: Property
propReplicateLength = property $ \(n :: Natural) (x :: Int) ->
  Nat.length (Nat.replicate n x) == n

propReplicateElem :: Property
propReplicateElem = property $ \(n :: Natural) (x :: Int) ->
  all (== x) $ Nat.replicate n x

propIsSorted :: Property
propIsSorted = forAllShrink genSomeSortedList shrinkSomeSortedList $
  \(undiag -> xs :: [PTx.Integer]) ->
    checkCoverage
      . cover 50.0 (isSorted xs) "precondition known satisfied"
      $ isSorted xs == (sort xs == xs)

propIsSortedMonotonic :: Property
propIsSortedMonotonic = forAllShrink genSomeMonotonicList shrinkSomeMonotonicList $
  \(undiag -> xs :: [PTx.Integer]) ->
    checkCoverage
      . cover 50.0 (isSortedMonotonic xs) "precondition known satisfied"
      $ isSortedMonotonic xs == (nub (sort xs) == xs)

propIsSortedOn :: Property
propIsSortedOn = property $
  \(Fun _ f :: Fun PTx.Integer PTx.Integer) ->
    forAllShrink (genSomeSortedListOn f) (shrinkSomeSortedListOn f) $
      \(undiag -> xs :: [PTx.Integer]) ->
        checkCoverage
          . cover 50.0 (isSortedOn f xs) "precondition known satisfied"
          $ isSortedOn f xs == isSorted (f <$> xs)

-- [daylily] This is VERY slow for now. Revisit after 'ordNub'.
propIsSortedMonotonicOn :: Property
propIsSortedMonotonicOn = property $
  \(Fun _ f :: Fun PTx.Integer PTx.Integer) ->
    forAllShrink (genSomeMonotonicListOn f) (shrinkSomeMonotonicListOn f) $
      \(undiag -> xs :: [PTx.Integer]) ->
        checkCoverage
          . cover 50.0 (isSortedMonotonicOn f xs) "precondition known satisfied"
          $ isSortedMonotonicOn f xs == isSortedMonotonic (f <$> xs)
