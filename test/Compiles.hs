{-# language
    AllowAmbiguousTypes
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , TemplateHaskell
  , TypeApplications
  #-}

module Main (main) where

import Refined
import Prelude (IO,putStrLn,Int)
import Data.Void (Void)

main :: IO ()
main = do
  putStrLn "refined/test/Compiles.hs: it compiles!"

id   = $$(refineTH @IdPred     @Int 3)
even = $$(refineTH @(Not Even) @Int 3)
odd  = $$(refineTH @Odd        @Int 3)

id_   = $$(refineTH_ @IdPred     @Int 3)
even_ = $$(refineTH_ @(Not Even) @Int 3)
odd_  = $$(refineTH_ @Odd        @Int 3)
