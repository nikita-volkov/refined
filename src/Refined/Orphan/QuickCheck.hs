--------------------------------------------------------------------------------

-- Copyright © 2015 Nikita Volkov
-- Copyright © 2018 Remy Goldschmidt
-- Copyright © 2019 chessai
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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

--------------------------------------------------------------------------------

-- | This module exposes orphan instances for the 'Refined' type.
--   This is unavoidable given the current module structure.
module Refined.Orphan.QuickCheck () where

--------------------------------------------------------------------------------

#if HAVE_QUICKCHECK

import           Data.Either      (isRight)
import           Refined (Refined, RefineException, Predicate, refine, reifyPredicate)
import           Refined.Unsafe   (reallyUnsafeRefine)
import           Test.QuickCheck  (Arbitrary(arbitrary), suchThatMaybe, Gen, sized, resize)
import           Data.Typeable    (Typeable, showsTypeRep, typeRep)
import           Data.Proxy       (Proxy(Proxy))

--------------------------------------------------------------------------------

instance forall p a. (Arbitrary a, Typeable a, Typeable p, Predicate p a) => Arbitrary (Refined p a) where
  arbitrary = loop 0 arbitrary

loop :: forall p a. (Typeable p, Typeable a, Predicate p a)
  => Int -> Gen a -> Gen (Refined p a)
loop runs gen
  | runs < 100 = do
      m <- suchThatRefined gen
      case m of
        Just x -> do
          pure x
        Nothing -> do
          loop (runs + 1) gen
  | otherwise = error (refinedGenError (Proxy @p) (Proxy @a))

refinedGenError :: (Typeable p, Typeable a)
  => Proxy p -> Proxy a -> String
refinedGenError p a = "arbitrary :: Refined ("
  ++ typeName p
  ++ ") ("
  ++ typeName a
  ++ "): Failed to generate a value that satisfied"
  ++ " the predicate after 100 tries."

suchThatRefined :: forall p a. (Predicate p a)
  => Gen a -> Gen (Maybe (Refined p a))
suchThatRefined gen = do
  m <- suchThatMaybe gen (reifyPredicate @p @a)
  case m of
    Nothing -> pure Nothing
    Just x -> pure (Just (reallyUnsafeRefine x))

typeName :: Typeable a => Proxy a -> String
typeName = flip showsTypeRep "" . typeRep

--------------------------------------------------------------------------------

#endif
