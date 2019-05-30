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

{-# OPTIONS_GHC -Wall           #-}

--------------------------------------------------------------------------------

{-# language ExplicitNamespaces #-}

--------------------------------------------------------------------------------

-- | In type theory, a refinement type is a type endowed
--   with a predicate which is assumed to hold for any element
--   of the refined type.
--
--   This library allows one to capture the idea of a refinement type
--   using the 'Refined' type. A 'Refined' @p@ @x@ wraps a value
--   of type @x@, ensuring that it satisfies a type-level predicate @p@.
--
--   A simple introduction to this library can be found here: http://nikita-volkov.github.io/refined/
--
--   This module only provides /safe/ constructions of 'Refined'
--   values, /safe/ meaning that the refinement predicate holds,
--   and the construction of the 'Refined' value is total.
--
--   If you can manually prove that the refinement predicate holds,
--   or you do not necessarily care about this definition of safety,
--   use the module /Refined.Unsafe/.
module Refined
  ( -- * 'Refined'
    Refined

    -- ** Creation
  , refine
  , refineThrow
  , refineFail
  , refineError
  , refineTH

    -- ** Consumption
  , unrefine

    -- * 'Predicate'
  , Predicate (validate)

    -- * Logical predicates
  , Not
  , And
  , type (&&)
  , Or
  , type (||)

    -- * Identity predicate
  , IdPred

    -- * Numeric predicates
  , LessThan
  , GreaterThan
  , From
  , To
  , FromTo
  , EqualTo
  , NotEqualTo 
  , Positive
  , NonPositive
  , Negative
  , NonNegative
  , ZeroToOne
  , NonZero

    -- * Foldable predicates
  , SizeLessThan
  , SizeGreaterThan
  , SizeEqualTo
  , NonEmpty

    -- * IsList predicates
  , Ascending
  , Descending

    -- * Weakening
  , Weaken (weaken)
  , andLeft
  , andRight
  , leftOr
  , rightOr

    -- * Error handling

    -- ** 'RefineException'
  , RefineException
    ( RefineNotException
    , RefineAndException
    , RefineOrException
    , RefineOtherException
    )
  , displayRefineException

    -- ** 'RefineT' and 'RefineM'
  , RefineT, runRefineT, mapRefineT
  , RefineM, refineM, runRefineM
  , throwRefine, catchRefine
  , throwRefineOtherException

    -- * Re-Exports
  , pretty
  ) where

--------------------------------------------------------------------------------

import Refined.Internal

-------------------------------------------------------------------------------
