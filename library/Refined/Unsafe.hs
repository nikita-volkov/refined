--------------------------------------------------------------------------------

-- Copyright © 2015 Nikita Volkov
-- Copyright © 2018 Remy Goldschmidt
-- Copyright © 2018 Daniel Cartwright
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

    -- ** Creation
  , reallyUnsafeRefine 
  , unsafeRefine

    -- ** Coercion
  , reallyUnsafeUnderlyingRefined
  , reallyUnsafeRefinedRefined
  ) where

--------------------------------------------------------------------------------

import           Control.Exception            (Exception(displayException))
import           Data.Coerce                  (Coercible, coerce)
import           Data.Either                  (either)
import           Data.Function                (id)

import           GHC.Err                      (error)

import           Refined.Internal             (Refined(Refined), Predicate, refine, (.>))
import           Data.Type.Coercion           (Coercion (..))

--------------------------------------------------------------------------------

-- | Constructs a 'Refined' value at run-time,
--   calling 'Prelude.error' if the value
--   does not satisfy the predicate.
--
--   WARNING: this function is not total!
unsafeRefine :: (Predicate p x) => x -> Refined p x
unsafeRefine = refine .> either (displayException .> error) id
{-# INLINABLE unsafeRefine #-}

-- | Constructs a 'Refined' value, completely
--   ignoring any refinements! Use this only
--   when you can manually prove that the refinement
--   holds.
reallyUnsafeRefine :: x -> Refined p x
reallyUnsafeRefine = coerce
{-# INLINE reallyUnsafeRefine #-}

-- | A coercion between a 'Refined' type and a type coercible with
-- its underlying type.
reallyUnsafeUnderlyingRefined :: Coercible x y => Coercion x (Refined p y)
reallyUnsafeUnderlyingRefined = Coercion

-- | A coercion between two 'Refined' types. This may be more convenient
-- than 'reallyUnsafeUnderlyingRefined' in some cases.
reallyUnsafeRefinedRefined :: Coercible x y => Coercion (Refined p x) (Refined q y)
reallyUnsafeRefinedRefined = Coercion
