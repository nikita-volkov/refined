{-# language
    DataKinds
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
  , ViewPatterns
  #-}

module Main (main) where

import Test.QuickCheck
import Refined

main :: IO ()
main = mapM_ quickCheck
  [ prop_lt
  , prop_gt
  , prop_from
  , prop_to
  , prop_bad
  , prop_shrink_lt
  , prop_shrink_gt
  ]

iddy :: (Eq a) => Refined p a -> Bool
iddy (unrefine -> r) = r == r

prop_lt :: Property
prop_lt = property $ \(r :: Refined (LessThan 5) MyInt) -> iddy r

prop_gt :: Property
prop_gt = property $ \(r :: Refined (GreaterThan 5) MyInt) -> iddy r

prop_from :: Property
prop_from = property $ \(r :: Refined (From 9) MyInt) -> iddy r

prop_to :: Property
prop_to = property $ \(r :: Refined (To 9) MyInt) -> iddy r

prop_bad :: Property
prop_bad = expectFailure $ ioProperty $ do
  -- expectFailure expects the error inside the property itself, not while
  -- generating the argument to the property. So we need to hide generation from
  -- it.
  r :: Refined (EqualTo 2 && EqualTo 3) MyInt <- generate arbitrary
  return $ iddy r

prop_shrink_lt :: Property
prop_shrink_lt = property $ \(r :: Refined (LessThan 5) MyInt) ->
  (unrefine <$> shrink r) == shrink (unrefine r)

prop_shrink_gt :: Property
prop_shrink_gt = property $ \(r :: Refined (GreaterThan 5) MyInt) ->
  (unrefine <$> shrink r) == (filter (> 5) $ shrink (unrefine r))

newtype MyInt = MyInt Int
  deriving (Eq, Ord, Show, Num)

instance Arbitrary MyInt where
  arbitrary = MyInt <$> choose (0,100)
  shrink (MyInt 0) = []
  shrink (MyInt n) = MyInt <$> [0..n-1]
