{-# language
    AllowAmbiguousTypes
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  #-}

{-# options_ghc -fdefer-type-errors #-}

module Main (main) where

import Data.IORef
import Data.Void (Void)
import Predicates
import Prelude (IO,putStrLn,Int,undefined)
import Refined
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception as E

main :: IO ()
main = do
  let none = $$(refineTH @None @Int 3)
  let none_ = $$(refineTH @None @Int undefined)
  let noneE = $$(refineTH @NoneE @Int 3)
  let noneE_ = $$(refineTH @NoneE @Int undefined)

  foldMap catchTypeError
    [ none
    , none_
    , noneE
    , noneE_
    ]

numShouldFailToCompile :: Int
numShouldFailToCompile = 2

failedToCompile :: IORef Int
failedToCompile = unsafePerformIO $ newIORef 0

catchTypeError :: a -> IO ()
catchTypeError x = E.catch
  (E.evaluate x)
  (\(e :: E.TypeError) -> do
    putStrLn $ E.displayException e
    modifyIORef' (+1) failedToCompile
  )
