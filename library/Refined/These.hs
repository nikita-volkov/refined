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

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE InstanceSigs       #-}

--------------------------------------------------------------------------------

-- | This module is defined internally to avoid using the 'these'
--   package, which brings in a lot of very heavy and unnecessary 
--   transitive dependencies. We export the type and constructors
--   here, in case a user should need it.
--   We provide a small API for working with the 'These' type here.
--   If one should need a fuller API, see https://hackage.haskell.org/package/these
--   Converting to/from the two types should be trivial, as the
--   data constructors are exported from both.
module Refined.These
  (
    -- * 'These' type    
    These(This, That, These)
  
    -- * Consumption
  , these
  , fromThese
  , mergeThese
  , mergeTheseWith

    -- * Traversals
  , here, there

    -- * Case selections
  , justThis
  , justThat
  , justThese

  , catThis
  , catThat
  , catThese
  
  , partitionThese

    -- * Case predicates
  , isThis
  , isThat
  , isThese

    -- * Map operations
  , mapThese
  , mapThis
  , mapThat
  ) where

--------------------------------------------------------------------------------

import Control.DeepSeq (NFData(rnf))
#if MIN_VERSION_base(4,10,0)
import Data.Bifoldable (Bifoldable(bifold, bifoldr, bifoldl))
#endif
#if MIN_VERSION_base(4,8,0)
import Data.Bifunctor  (Bifunctor(bimap, first, second))
#endif
import Data.Data       (Data)
import Data.Maybe      (isJust, mapMaybe)
import Data.Semigroup  (Semigroup((<>)))
import Data.Typeable   (Typeable)
import GHC.Generics    (Generic, Generic1)
 
-- | This is defined internally to avoid using the 'these'
--   package, which brings in a lot of very heavy and unnecessary 
--   transitive dependencies. We export the type and constructors
--   here, in case a user should need it.
data These a b = This a | That b | These a b
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, Generic1)

-- | Case analysis for the 'These' type.
these :: (a -> c) -> (b -> c) -> (a -> b -> c) -> These a b -> c
these l _ _ (This a) = l a
these _ r _ (That x) = r x
these _ _ lr (These a x) = lr a x

-- | Takes two default values and produces a tuple.
fromThese :: a -> b -> These a b -> (a, b)
fromThese _ x (This a   ) = (a, x)
fromThese a _ (That x   ) = (a, x)
fromThese _ _ (These a x) = (a, x)

-- | Coalesce with the provided operation.
mergeThese :: (a -> a -> a) -> These a a -> a
mergeThese = these id id

-- | BiMap and coalesce results with the provided operation.
mergeTheseWith :: (a -> c) -> (b -> c) -> (c -> c -> c) -> These a b -> c
mergeTheseWith f g op t = mergeThese op $ mapThese f g t

-- | A @Traversal@ of the first half of a 'These', suitable for use with @Control.Lens@.
here :: (Applicative f) => (a -> f b) -> These a t -> f (These b t)
here f (This x) = This <$> f x
here f (These x y) = flip These y <$> f x
here _ (That x) = pure (That x)

-- | A @Traversal@ of the second half of a 'These', suitable for use with @Control.Lens@.
there :: (Applicative f) => (a -> f b) -> These t a -> f (These t b)
there _ (This x) = pure (This x)
there f (These x y) = These x <$> f y
there f (That x) = That <$> f x

-- | @'justThis' = 'these' 'Just' (\_ -> 'Nothing') (\_ _ -> 'Nothing')@
justThis :: These a b -> Maybe a
justThis = these Just (\_ -> Nothing) (\_ _ -> Nothing)

-- | @'justThat' = 'these' (\_ -> 'Nothing') 'Just' (\_ _ -> 'Nothing')@
justThat :: These a b -> Maybe b
justThat = these (\_ -> Nothing) Just (\_ _ -> Nothing)

-- | @'justThese' = 'these' (\_ -> 'Nothing') (\_ -> 'Nothing') (\a b -> 'Just' (a, b))@
justThese :: These a b -> Maybe (a, b)
justThese = these (\_ -> Nothing) (\_ -> Nothing) (\a b -> Just (a, b))

isThis, isThat, isThese :: These a b -> Bool
-- | @'isThis' = 'isJust' . 'justThis'@
isThis  = isJust . justThis

-- | @'isThat' = 'isJust' . 'justThat'@
isThat  = isJust . justThat

-- | @'isThese' = 'isJust' . 'justThese'@
isThese = isJust . justThese

-- | 'Bifunctor' map.
mapThese :: (a -> c) -> (b -> d) -> These a b -> These c d
mapThese f _ (This  a  ) = This (f a)
mapThese _ g (That    x) = That (g x)
mapThese f g (These a x) = These (f a) (g x)

-- | @'mapThis' = over 'here'@
mapThis :: (a -> c) -> These a b -> These c b
mapThis f = mapThese f id

-- | @'mapThat' = over 'there'@
mapThat :: (b -> d) -> These a b -> These a d
mapThat f = mapThese id f

-- | Select all 'This' constructors from a list.
catThis :: [These a b] -> [a]
catThis = mapMaybe justThis

-- | Select all 'That' constructors from a list.
catThat :: [These a b] -> [b]
catThat = mapMaybe justThat

-- | Select all 'These' constructors from a list.
catThese :: [These a b] -> [(a, b)]
catThese = mapMaybe justThese

-- | Select each constructor and partition them into separate lists.
partitionThese :: [These a b] -> ( [(a, b)], ([a], [b]) )
partitionThese []             = ([], ([], []))
partitionThese (These x y:xs) = first ((x, y):)      $ partitionThese xs
partitionThese (This  x  :xs) = second (first  (x:)) $ partitionThese xs
partitionThese (That    y:xs) = second (second (y:)) $ partitionThese xs

instance (Semigroup a, Semigroup b) => Semigroup (These a b) where
    This  a   <> This  b   = This  (a <> b)
    This  a   <> That    y = These  a             y
    This  a   <> These b y = These (a <> b)       y
    That    x <> This  b   = These       b   x
    That    x <> That    y = That           (x <> y)
    That    x <> These b y = These       b  (x <> y)
    These a x <> This  b   = These (a <> b)  x
    These a x <> That    y = These  a       (x <> y)
    These a x <> These b y = These (a <> b) (x <> y)

#if MIN_VERSION_base(4,8,0)
instance Bifunctor These where
  bimap :: (a -> c) -> (b -> d) -> These a b -> These c d 
  bimap f _ (This a   ) = This  (f a)
  bimap _ g (That    b) = That        (g b)
  bimap f g (These a b) = These (f a) (g b)
  first :: (a -> c) -> These a b -> These c b
  first f = bimap f id
  second :: (b -> d) -> These a b -> These a d
  second f = bimap id f
#endif

instance Functor (These a) where
    fmap _ (This x) = This x
    fmap f (That y) = That (f y)
    fmap f (These x y) = These x (f y)

instance Semigroup a => Applicative (These a) where
  pure = That
  This  a   <*> _         = This a
  That    _ <*> This  b   = This b
  That    f <*> That    x = That (f x)
  That    f <*> These b x = These b (f x)
  These a _ <*> This  b   = This (a <> b)
  These a f <*> That    x = These a (f x)
  These a f <*> These b x = These (a <> b) (f x)

instance Semigroup a => Monad (These a) where
  return = pure
  This  a   >>= _ = This a
  That    x >>= k = k x
  These a x >>= k = case k x of
                        This  b   -> This  (a <> b)
                        That    y -> These a y
                        These b y -> These (a <> b) y

instance (NFData a, NFData b) => NFData (These a b) where
  rnf (This a) = rnf a
  rnf (That b) = rnf b
  rnf (These a b) = rnf a `seq` rnf b

instance Foldable (These a) where
    foldr _ z (This _) = z
    foldr f z (That x) = f x z
    foldr f z (These _ x) = f x z

instance Traversable (These a) where
    traverse _ (This  a  ) = pure $ This a
    traverse f (That    x) = That <$> f x
    traverse f (These a x) = These a <$> f x
    sequenceA (This  a  ) = pure $ This a
    sequenceA (That    x) = That <$> x
    sequenceA (These a x) = These a <$> x

#if MIN_VERSION_base(4,10,0)
instance Bifoldable These where
    bifold = these id id mappend
    bifoldr f g z = these (`f` z) (`g` z) (\x y -> x `f` (y `g` z))
    bifoldl f g z = these (z `f`) (z `g`) (\x y -> (z `f` x) `g` y)
#endif
