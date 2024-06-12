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

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
#endif
{-# OPTIONS_GHC -Wall #-}

--------------------------------------------------------------------------------

-- | This module exposes /unsafe/ refinements. An /unsafe/ refinement
--   is one which either does not make the guarantee of totality in construction
--   of the 'Refined' value or does not perform a check of the refinement
--   predicate. It is recommended only to use this when you can manually prove
--   that the refinement predicate holds.
module Refined.Unsafe
  ( -- * 'Refined'
    Refined
  , Refined1

    -- ** Creation
  , reallyUnsafeRefine
  , reallyUnsafeRefine1
  , unsafeRefine

    -- ** Coercion
  , reallyUnsafeUnderlyingRefined
#if __GLASGOW_HASKELL__ >= 805
  , reallyUnsafeAllUnderlyingRefined
#endif
  , reallyUnsafePredEquiv
  ) where

--------------------------------------------------------------------------------

import           Control.Exception            (displayException)
import           Data.Coerce                  (coerce)

import           Refined                      (Predicate, refine)
import           Refined.Unsafe.Type          (Refined(Refined), Refined1(Refined1))
import           Data.Type.Coercion           (Coercion (..))
#if __GLASGOW_HASKELL__ >= 805
import           Data.Coerce                  (Coercible)
#endif

--------------------------------------------------------------------------------

-- | Constructs a 'Refined' value at run-time,
--   calling 'Prelude.error' if the value
--   does not satisfy the predicate.
--
--   WARNING: this function is not total!
--
--   @since 0.2.0.0
unsafeRefine :: (Predicate p x) => x -> Refined p x
unsafeRefine = either (error . displayException) id . refine
{-# INLINABLE unsafeRefine #-}

-- | Constructs a 'Refined' value, completely
--   ignoring any refinements! Use this only
--   when you can manually prove that the refinement
--   holds.
--
--   @since 0.3.0.0
reallyUnsafeRefine :: x -> Refined p x
reallyUnsafeRefine = coerce
{-# INLINE reallyUnsafeRefine #-}

reallyUnsafeRefine1 :: f x -> Refined1 p f x
reallyUnsafeRefine1 = coerce
{-# INLINE reallyUnsafeRefine1 #-}

-- | A coercion between a type and any refinement of that type.
--   See "Data.Type.Coercion" for functions manipulating
--   coercions.
--
--   @since 0.3.0.0
reallyUnsafeUnderlyingRefined :: Coercion x (Refined p x)
reallyUnsafeUnderlyingRefined = Coercion

-- | A coercion between two 'Refined' types, magicking up the
--   claim that one predicate is entirely equivalent to another.
--
--   @since 0.3.0.0
reallyUnsafePredEquiv :: Coercion (Refined p x) (Refined q x)
reallyUnsafePredEquiv = Coercion
-- Note: reallyUnsafePredEquiv =
-- sym 'reallyUnsafeUnderlyingRefined' `trans` 'reallyUnsafeUnderlyingRefined'

#if __GLASGOW_HASKELL__ >= 805
-- | Reveal that @x@ and @'Refined' p x@ are 'Coercible' for
-- /all/ @x@ and @p@ simultaneously.
--
-- === Example
--
-- @
-- reallyUnsafePredEquiv :: Coercion (Refined p x) (Refined q x)
-- reallyUnsafePredEquiv = reallyUnsafeAllUnderlyingRefined Coercion
-- @
--
--   @since 0.3.0.0
reallyUnsafeAllUnderlyingRefined
  :: ((forall x y p. (Coercible x y => Coercible y (Refined p x))) => r) -> r
-- Why is this constraint so convoluted? Because otherwise the constraint
-- solver doesn't handle transitivity properly. See "Safe Zero-cost Coercions
-- for Haskell" by Breitner et al.
reallyUnsafeAllUnderlyingRefined r = r
#endif
