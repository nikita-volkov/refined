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
import Refined.Orphan

main :: IO ()
main = mapM_ quickCheck
  [ prop_lt
  , prop_gt
  , prop_from
  , prop_to
  , prop_bad
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
prop_bad = expectFailure $ \(r :: Refined (EqualTo 2 && EqualTo 3) MyInt) -> iddy r

newtype MyInt = MyInt Int
  deriving (Eq, Ord, Show, Num)

instance Arbitrary MyInt where
  arbitrary = MyInt <$> choose (0,100)
