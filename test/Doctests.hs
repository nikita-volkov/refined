module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest $ srcFiles ++ compFlags

srcFiles :: [String]
srcFiles =
  [ "src/Refined.hs"
  , "src/Refined/Unsafe/Type.hs"
  , "src/Refined/Unsafe.hs"
  ]

compFlags :: [String]
compFlags =
  [ "-XScopedTypeVariables"
  ]
