module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest $ srcFiles ++ compFlags

srcFiles :: [String]
srcFiles =
  [ "src/Refined.hs"
  , "src/Refined/Internal.hs"
  , "src/Refined/Orphan/Aeson.hs"
  , "src/Refined/Orphan/QuickCheck.hs"
  , "src/Refined/Orphan.hs"
  , "src/Refined/These.hs"
  , "src/Refined/Unsafe/Type.hs"
  , "src/Refined/Unsafe.hs"
  ]

compFlags :: [String]
compFlags =
  [ "-XScopedTypeVariables"
  ]
