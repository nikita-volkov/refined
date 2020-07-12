{-# language
    AllowAmbiguousTypes
  , ExistentialQuantification
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  #-}

-- unfortunately this doesn't work because
-- it uses TH, not type errors
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
  let shouldntCompiles =
        [ Something ($$(refineTH @None @Int 3))
        , Something ($$(refineTH @None @Int undefined))
        , Something ($$(refineTH @NoneE @Int 3))
        , Something ($$(refineTH @NoneE @Int undefined))
        ]

  foldMap (\(Something x) -> catchTypeError x) shouldntCompiles

  let numShouldntCompiles = length shouldntCompiles

  actualDidntCompiles <- readIORef failedToCompile

  assert (numShouldntCompiles == actualDidntCompiles) (pure ())

failedToCompile :: IORef Int
failedToCompile = unsafePerformIO $ newIORef 0
{-# NOINLINE failedToCompile #-}

data Something = forall a. Something a

catchTypeError :: a -> IO ()
catchTypeError x = E.catch
  (E.evaluate x)
  (\(e :: E.TypeError) -> do
    putStrLn $ E.displayException e
    modifyIORef' (+1) failedToCompile
  )
