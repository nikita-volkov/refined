{-# language
    FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  #-}

module Predicates
  ( None(..), NoneE(..)
  ) where

import Refined
import Data.Typeable (typeOf)
import Control.Exception (Exception)

data None = None

instance Predicate None x where
  validate p _ = throwRefineOtherException (typeOf p) "None shall pass!"

data NoneE = NoneE
data NoneException = NoneException deriving Show
instance Exception NoneException

instance Predicate NoneE x where
  validate p _ = throwRefineSomeException (typeOf p) NoneException
