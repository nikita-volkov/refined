{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-| This module contains orphan 'Lift' instances for more available compile-time checking of predicates.
-}

module Refined.Orphan () where

import Control.Monad
import Data.Map.Internal (Map(..))
import Data.Set.Internal (Set(..))

import qualified Language.Haskell.TH.Syntax as TH

deriving instance (TH.Lift k, TH.Lift v) => TH.Lift (Map k v)
deriving instance (TH.Lift v) => TH.Lift (Set v)
