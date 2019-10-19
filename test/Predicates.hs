{-# language
    FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  #-}

module Predicates
  ( None(..)
  ) where

import Refined
import Data.Typeable (typeOf)

data None = None

instance Predicate None x where
  validate p _ = throwRefineOtherException (typeOf p) "None shall pass!"
