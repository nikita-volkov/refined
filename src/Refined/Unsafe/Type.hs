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

-- | This module exports the 'Refined' type with its
--   constructor. This is very risky! In particular, the 'Coercible'
--   instances will be visible throughout the importing module.
--   It is usually better to build the necessary coercions locally
--   using the utilities in "Refined.Unsafe", but in some cases
--   it may be more convenient to write a separate module that
--   imports this one and exports some large coercion.
module Refined.Unsafe.Type
  ( Refined(Refined)
  ) where

import Refined.Internal (Refined(Refined))
