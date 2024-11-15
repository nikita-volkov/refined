--------------------------------------------------------------------------------

-- Copyright © 2015 Nikita Volkov
-- Copyright © 2018 Remy Goldschmidt
-- Copyright © 2020 chessai
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use,
-- copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following
-- conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
-- OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
-- HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
-- WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
-- OTHER DEALINGS IN THE SOFTWARE.

--------------------------------------------------------------------------------

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE TemplateHaskell            #-}

--------------------------------------------------------------------------------

-- | This module exports the 'Refined' type with its
--   constructor. This is very risky! In particular, the 'Data.Coerce.Coercible'
--   instances will be visible throughout the importing module.
--   It is usually better to build the necessary coercions locally
--   using the utilities in "Refined.Unsafe", but in some cases
--   it may be more convenient to write a separate module that
--   imports this one and exports some large coercion.
module Refined.Unsafe.Type
  ( Refined(Refined)
  , Refined1(Refined1)
  ) where

import           Control.DeepSeq              (NFData)
import           Data.Hashable (Hashable)
import qualified Language.Haskell.TH.Syntax   as TH

-- | A refinement type, which wraps a value of type @x@.
--
--   @since 0.1.0.0
newtype Refined (p :: k) x
  = Refined x -- ^ @since 0.1.0.0
  deriving newtype
    ( Eq -- ^ @since 0.1.0.0
    , Ord -- ^ @since 0.1.0.0
    , Hashable -- ^ @since 0.6.3
    , NFData -- ^ @since 0.5
    )
  deriving stock
    ( Show -- ^ @since 0.1.0.0
    )
  deriving stock
    ( Foldable -- ^ @since 0.2
    )

-- | @since 0.3.0.0
type role Refined nominal nominal

-- | @since 0.1.0.0
instance (TH.Lift x) => TH.Lift (Refined p x) where
  lift (Refined a) = [|Refined a|]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (Refined a) = [||Refined a||]
#endif

-- | A refinement type, which wraps a value of type @f x@.
--
-- The predicate is applied over the functor @f@. Thus, we may safely recover
-- various 'Functor'y instances, because no matter what you do to the
-- values inside the functor, the predicate may not be invalidated.
newtype Refined1 (p :: k) f x
  = Refined1 (f x)
  deriving newtype
    ( Eq
    , Ord
    , Hashable
    , NFData
    , Functor
    , Foldable
    )
  deriving stock
    ( Show
    , Traversable
    )

type role Refined1 nominal nominal nominal

instance TH.Lift (f a) => TH.Lift (Refined1 p f a) where
  lift (Refined1 fa) = [|Refined1 fa|]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (Refined1 fa) = [||Refined1 fa||]
#endif
