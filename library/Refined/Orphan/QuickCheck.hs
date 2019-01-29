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

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

-- | This module exposes orphan instances for the 'Refined' type.
--   This is unavoidable given the current module structure.
module Refined.Orphan.QuickCheck () where

--------------------------------------------------------------------------------

#if HAVE_QUICKCHECK

import           Data.Either      (isRight)
import           Refined.Internal (Refined(Refined), RefineException, Predicate, refine)
import           Test.QuickCheck  (Arbitrary(arbitrary), suchThat, Gen)

--------------------------------------------------------------------------------

instance forall p a. (Arbitrary a, Predicate p a) => Arbitrary (Refined p a) where
  arbitrary = Refined <$> suchThat (arbitrary :: Gen a) (isRight . (refine :: a -> Either RefineException (Refined p a)))

--------------------------------------------------------------------------------

#endif
