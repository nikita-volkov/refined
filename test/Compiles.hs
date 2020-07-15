{-# language
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , TemplateHaskell
  , TypeApplications
  #-}

module Main (main) where

import Refined
import Data.Void (Void)

id_   = $$(refineTH_ @IdPred     @Int 3)
even_ = $$(refineTH_ @(Not Even) @Int 3)
odd_  = $$(refineTH_ @Odd        @Int 3)

--foo = $$(refineTH
--        @( And
--             Even
--             ( Xor
--                 ( And
--                     (Not (DivisibleBy 3))
--                     IdPred
--                 )
--                 (DivisibleBy 2)
--             )
--         )
--        @Int
--        3)
--bar = case foo of
--  Left e -> e
--  Right _ -> error "bad!"

main :: IO ()
main = do
  putStrLn "refined/test/Compiles.hs: it compiles!"
  foldMap print
    [ id_
    , even_
    , odd_
    ]
--  print bar
