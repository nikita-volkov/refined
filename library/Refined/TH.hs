--------------------------------------------------------------------------------

{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

--------------------------------------------------------------------------------

{-| This module contains orphan 'Lift' instances of types in common libraries
    such as 'containers', for more available compile-time checking of predicates.

-}

module Refined.TH () where

--------------------------------------------------------------------------------

import Data.IntMap.Internal (IntMap(..))
import Data.Map.Internal (Map(..))
import Data.Sequence.Internal (Digit(..), Elem(..), FingerTree(..), Node(..), Seq(..), ViewL(..), ViewR(..))
import Data.Set.Internal (Set(..))
import Data.Tree (Tree(..))

import Language.Haskell.TH.Syntax (Lift)

--------------------------------------------------------------------------------

-- [containers]
deriving instance (Lift a) => Lift (IntMap a)
deriving instance (Lift k, Lift v) => Lift (Map k v)
deriving instance (Lift v) => Lift (Set v)
deriving instance (Lift a) => Lift (Elem a)
deriving instance (Lift a) => Lift (Node a)
deriving instance (Lift a) => Lift (Digit a)
deriving instance (Lift a) => Lift (FingerTree a)
deriving instance (Lift a) => Lift (Seq a)
deriving instance (Lift a) => Lift (ViewL a)
deriving instance (Lift a) => Lift (ViewR a)
deriving instance (Lift a) => Lift (Tree a)

--------------------------------------------------------------------------------
