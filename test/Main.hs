{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}

module Main (main) where

import Refined
import Control.Monad (guard)
import Data.List (replicate)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

main :: IO ()
main = pure ()

--boogle :: Refined NonZero Int
--boogle = $$(refineTH 0)

bamble :: Refined NonZero Int
bamble = $$(refineTH 12)

--foogle :: Refined EmptyList ByteString
--foogle = $$(refineTH "")

-- this will fail to compile
--badoogle :: Refined (Or Descending Ascending) [Int]
--badoogle = $$(refineTH [1,2,3,2])

boggle :: Refined (Or Descending Ascending) (Set Int)
boggle = $$(refineTH (Set.fromList [1,2,3,4]))

-- this will fail to compile
--hoggle :: Refined Descending (Set Int)
--hoggle = $$(refineTH (Set.fromList [1,2,3,4]))

baz :: Refined NonEmpty (Set Int)
baz = $$(refineTH (Set.singleton 0))

-- this will fail to compile
--baz' :: Refined NonEmpty (Set Int)
--baz' = $$(refineTH (Set.empty))

bar :: Refined NonEmpty (Map Int Int)
bar = $$(refineTH (Map.singleton 0 1))

-- this will fail to compile
--bar' :: Refined NonEmpty (Map Int Int)
--bar' = $$(refineTH (Map.empty))

foo :: [Bool]
foo = foldl1 (\x acc -> x >>= guard >> acc)
  ($$(refineTH (replicate 3 True)) :: Refined NonEmpty [Bool])

-- this will fail to compile
--foo' :: [Bool]
--foo' = foldl1 (\x acc -> x >>= guard >> acc)
--  ($$(refineTH (replicate 0 True)) :: Refined NonEmpty [Bool])
