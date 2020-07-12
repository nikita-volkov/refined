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

{-# OPTIONS_GHC -Wall                        #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# OPTIONS_GHC -funbox-strict-fields        #-}

--------------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

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
module Refined
  ( -- * 'Refined' type
    Refined

    -- ** Creation
  , refine
  , refine_
  , refineThrow
  , refineFail
  , refineError
  , refineTH
  , refineTH_

    -- ** Consumption
  , unrefine

    -- * 'Predicate'
  , Predicate (validate)
  , reifyPredicate

    -- * Logical predicates
  , Not(..)
  , And(..)
  , type (&&)
  , Or(..)
  , type (||)
  , Xor(..)

    -- * Identity predicate
  , IdPred(..)

    -- * Numeric predicates
  , LessThan(..)
  , GreaterThan(..)
  , From(..)
  , To(..)
  , FromTo(..)
  , NegativeFromTo(..)
  , EqualTo(..)
  , NotEqualTo(..)
  , Odd(..)
  , Even(..)
  , DivisibleBy(..)
  , NaN(..)
  , Infinite(..)
  , Positive
  , NonPositive
  , Negative
  , NonNegative
  , ZeroToOne
  , NonZero

    -- * Foldable predicates
    -- ** Size predicates
  , SizeLessThan(..)
  , SizeGreaterThan(..)
  , SizeEqualTo(..)
  , Empty
  , NonEmpty
    -- ** Ordering predicates
  , Ascending(..)
  , Descending(..)

    -- * Weakening
  , Weaken (weaken)
  , andLeft
  , andRight
  , leftOr
  , rightOr

    -- * Strengthening
  , strengthen

    -- * Error handling

    -- ** 'RefineException'
  , RefineException
    ( RefineNotException
    , RefineAndException
    , RefineOrException
    , RefineXorException
    , RefineOtherException
    , RefineSomeException
    )
  , displayRefineException

    -- ** 'validate' helpers
  , throwRefineOtherException
  , throwRefineSomeException
  , success

    -- * Re-Exports
  , PP.pretty
 ) where

--------------------------------------------------------------------------------

import           Control.Exception            (Exception (displayException))
import           Data.Coerce                  (coerce)
import           Data.Either                  (isRight)
import           Data.Foldable                (foldl')
import           Data.Function                (fix)
import           Data.Proxy                   (Proxy(Proxy))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import           Data.Typeable                (TypeRep, Typeable, typeOf)
import           Data.Void                    (Void)

import           Control.Monad.Catch          (MonadThrow, SomeException)
import qualified Control.Monad.Catch          as MonadThrow
import           Control.Monad.Error.Class    (MonadError)
import qualified Control.Monad.Error.Class    as MonadError
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail           (MonadFail, fail)
import           Prelude                      hiding (fail)
#endif

import           GHC.Generics                 (Generic, Generic1)
import           GHC.TypeLits                 (type (<=), KnownNat, Nat, natVal)

import           Refined.Unsafe.Type          (Refined(Refined))

import qualified Data.Text.Prettyprint.Doc    as PP
import qualified Language.Haskell.TH.Syntax   as TH

#if HAVE_AESON
import           Control.Monad    ((<=<))
import           Data.Aeson       (FromJSON(parseJSON), ToJSON(toJSON))
#endif

#if HAVE_QUICKCHECK
import           Test.QuickCheck  (Arbitrary, Gen)
import qualified Test.QuickCheck  as QC
import           Data.Typeable    (showsTypeRep, typeRep)
#endif

import "these-skinny" Data.These                   (These(This,That,These))

--------------------------------------------------------------------------------

-- $setup
--
-- Doctest imports
--
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Data.Int
-- >>> import Data.Either (isLeft)
--

--------------------------------------------------------------------------------

infixl 0 |>
infixl 9 .>

-- | Helper function, stolen from the flow package.
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
{-# INLINE (|>) #-}

-- | Helper function, stolen from the flow package.
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = \x -> g (f x)
{-# INLINE (.>) #-}

-- | FIXME: doc
data Ordered a = Empty | Decreasing a | Increasing a

-- | FIXME: doc
inc :: Ordered a -> Bool
inc (Decreasing _) = False
inc _              = True
{-# INLINE inc #-}

-- | FIXME: doc
dec :: Ordered a -> Bool
dec (Increasing _) = False
dec _              = True
{-# INLINE dec #-}

increasing :: (Foldable t, Ord a) => t a -> Bool
increasing = inc . foldl' go Empty where
  go Empty y = Increasing y
  go (Decreasing x) _ = Decreasing x
  go (Increasing x) y
    | x <= y = Increasing y
    | otherwise = Decreasing y
{-# INLINABLE increasing #-}

decreasing :: (Foldable t, Ord a) => t a -> Bool
decreasing = dec . foldl' go Empty where
  go Empty y = Decreasing y
  go (Increasing x) _ = Increasing x
  go (Decreasing x) y
    | x >= y = Decreasing y
    | otherwise = Increasing y
{-# INLINABLE decreasing #-}

--------------------------------------------------------------------------------

-- | This instance makes sure to check the refinement.
--
--   @since 0.1.0.0
instance (Read x, Predicate p x) => Read (Refined p x) where
  readsPrec d = readParen (d > 10) $ \r1 -> do
    ("Refined", r2) <- lex r1
    (raw,       r3) <- readsPrec 11 r2
    case refine raw of
      Right val -> [(val, r3)]
      Left  _   -> []

#if HAVE_AESON
-- | @since 0.4
instance (FromJSON a, Predicate p a) => FromJSON (Refined p a) where
  parseJSON = refineFail <=< parseJSON

-- | @since 0.4
instance (ToJSON a, Predicate p a) => ToJSON (Refined p a) where
  toJSON = toJSON . unrefine
#endif /* HAVE_AESON */

#if HAVE_QUICKCHECK
-- | @since 0.4
instance forall p a. (Arbitrary a, Typeable a, Typeable p, Predicate p a) => Arbitrary (Refined p a) where
  arbitrary = loop 0 QC.arbitrary
    where
      loop :: Int -> Gen a -> Gen (Refined p a)
      loop !runs gen
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
  m <- QC.suchThatMaybe gen (reifyPredicate @p @a)
  case m of
    Nothing -> pure Nothing
    Just x -> pure (Just (Refined x))

typeName :: Typeable a => Proxy a -> String
typeName = flip showsTypeRep "" . typeRep
#endif /* HAVE_QUICKCHECK */

--------------------------------------------------------------------------------

-- | A smart constructor of a 'Refined' value.
--   Checks the input value at runtime.
--
--   @since 0.1.0.0
refine :: (Predicate p x) => x -> Either RefineException (Refined p x)
refine x = do
  let predicateByResult :: Either RefineException (Refined p x) -> p
      predicateByResult = const undefined
  fix $ \result -> do
    maybe (Right (Refined x)) Left
      (validate (predicateByResult result) x)
{-# INLINABLE refine #-}

-- | Like 'refine', but discards the refinement.
--   This _can_ be useful when you only need to validate
--   that some value at runtime satisfies some predicate.
--   See also 'reifyPredicate'.
--
--   @since 0.4.4
refine_ :: forall p x. (Predicate p x) => x -> Either RefineException x
refine_ = refine @p @x .> coerce
{-# INLINABLE refine_ #-}

-- | Constructs a 'Refined' value at run-time,
--   calling 'Control.Monad.Catch.throwM' if the value
--   does not satisfy the predicate.
--
--   @since 0.2.0.0
refineThrow :: (Predicate p x, MonadThrow m) => x -> m (Refined p x)
refineThrow = refine .> either MonadThrow.throwM pure
{-# INLINABLE refineThrow #-}

-- | Constructs a 'Refined' value at run-time,
--   calling 'Control.Monad.Fail.fail' if the value
--   does not satisfy the predicate.
--
--   @since 0.2.0.0
refineFail :: (Predicate p x, MonadFail m) => x -> m (Refined p x)
refineFail = refine .> either (displayException .> fail) pure
{-# INLINABLE refineFail #-}

-- | Constructs a 'Refined' value at run-time,
--   calling 'Control.Monad.Error.throwError' if the value
--   does not satisfy the predicate.
--
--   @since 0.2.0.0
refineError :: (Predicate p x, MonadError RefineException m)
            => x -> m (Refined p x)
refineError = refine .> either MonadError.throwError pure
{-# INLINABLE refineError #-}

--------------------------------------------------------------------------------

-- | Constructs a 'Refined' value at compile-time using @-XTemplateHaskell@.
--
--   For example:
--
--   > $$(refineTH 23) :: Refined Positive Int
--   > Refined 23
--
--   Here's an example of an invalid value:
--
--   > $$(refineTH 0) :: Refined Positive Int
--   > <interactive>:6:4:
--   >     Value is not greater than 0
--   >     In the Template Haskell splice $$(refineTH 0)
--   >     In the expression: $$(refineTH 0) :: Refined Positive Int
--   >     In an equation for ‘it’:
--   >         it = $$(refineTH 0) :: Refined Positive Int
--
--   If it's not evident, the example above indicates a compile-time failure,
--   which means that the checking was done at compile-time, thus introducing a
--   zero runtime overhead compared to a plain value construction.
--
--   It may be useful to use this function with the `th-lift-instances` package at https://hackage.haskell.org/package/th-lift-instances/
--
--   @since 0.1.0.0
refineTH :: (Predicate p x, TH.Lift x) => x -> TH.Q (TH.TExp (Refined p x))
refineTH =
  let refineByResult :: (Predicate p x)
        => TH.Q (TH.TExp (Refined p x))
        -> x
        -> Either RefineException (Refined p x)
      refineByResult = const refine
  in fix $ \loop -> refineByResult (loop undefined)
       .> either (show .> fail) TH.lift
       .> fmap TH.TExp

-- | Like 'refineTH', but immediately unrefines the value.
--   This is useful when some value need only be refined
--   at compile-time.
--
--   @since 0.4.4
refineTH_ :: forall p x. (Predicate p x, TH.Lift x)
  => x
  -> TH.Q (TH.TExp x)
refineTH_ =
  let refineByResult :: (Predicate p x)
        => TH.Q (TH.TExp x)
        -> x
        -> Either RefineException x
      refineByResult = const (refine_ @p @x)
  in fix $ \loop -> refineByResult (loop undefined)
       .> either (show .> fail) TH.lift
       .> fmap TH.TExp

--------------------------------------------------------------------------------

-- | Extracts the refined value.
--
--   @since 0.1.0.0
unrefine :: Refined p x -> x
unrefine = coerce
{-# INLINE unrefine #-}

--------------------------------------------------------------------------------

-- | A typeclass which defines a runtime interpretation of
--   a type-level predicate @p@ for type @x@.
--
--   @since 0.1.0.0
class (Typeable p) => Predicate p x where
  {-# MINIMAL validate #-}
  -- | Check the value @x@ according to the predicate @p@,
  --   producing an error 'RefineException' if the value
  --   does not satisfy.
  --
  --   /Note/: 'validate' is not intended to be used
  --   directly; instead, it is intended to provide the minimal
  --   means necessary for other utilities to be derived. As
  --   such, the 'Maybe' here should be interpreted to mean
  --   the presence or absence of a 'RefineException', and
  --   nothing else.
  --
  --   /Note/: An implementation of 'validate' should not
  --   case on its first argument, @p@. This is due to an
  --   implementation detail of the 'refine' function. See
  --   <https://github.com/nikita-volkov/refined/issues/56 this issue>
  --   for more details.
  --
  --   @since 0.1.0.0
  validate :: p -> x -> Maybe RefineException

--------------------------------------------------------------------------------

-- | Reify a 'Predicate' by turning it into a value-level predicate.
--
--   @since 0.4.2.3
reifyPredicate :: forall p a. Predicate p a => a -> Bool
reifyPredicate = refine @p @a .> isRight
{-# INLINABLE reifyPredicate #-}

--------------------------------------------------------------------------------

-- | A predicate which is satisfied for all types.
--   Arguments passed to @'validate'@ in @'validate' 'IdPred' x@
--   are not evaluated.
--
--   >>> isRight (refine @IdPred @Int undefined)
--   True
--
--   >>> isLeft (refine @IdPred @Int undefined)
--   False
--
--   @since 0.3.0.0
data IdPred
  = IdPred -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.3.0.0
instance Predicate IdPred x where
  validate _ _ = Nothing
  {-# INLINE validate #-}

--------------------------------------------------------------------------------

-- | The negation of a predicate.
--
--   >>> isRight (refine @(Not NonEmpty) @[Int] [])
--   True
--
--   >>> isLeft (refine @(Not NonEmpty) @[Int] [1,2])
--   True
--
--   @since 0.1.0.0
data Not p
  = Not -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    , Generic1 -- ^ @since 0.3.0.0
    )

-- | @since 0.1.0.0
instance (Predicate p x, Typeable p) => Predicate (Not p) x where
  validate p x = do
    maybe (Just (RefineNotException (typeOf p)))
          (const Nothing)
          (validate @p undefined x)

--------------------------------------------------------------------------------

-- | The conjunction of two predicates.
--
--   >>> isLeft (refine @(And Positive Negative) @Int 3)
--   True
--
--   >>> isRight (refine @(And Positive Odd) @Int 203)
--   True
--
--   @since 0.1.0.0
data And l r
  = And -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    , Generic1 -- ^ @since 0.3.0.0
    )

infixr 3 &&
-- | The conjunction of two predicates.
--
--   @since 0.2.0.0
type (&&) = And

-- | @since 0.1.0.0
instance ( Predicate l x, Predicate r x, Typeable l, Typeable r
         ) => Predicate (And l r) x where
  validate p x = do
    let a = validate @l undefined x
    let b = validate @r undefined x
    let throw err = Just (RefineAndException (typeOf p) err)
    case (a, b) of
      (Just  e, Just e1) -> throw (These e e1)
      (Just  e,       _) -> throw (This e)
      (Nothing, Just  e) -> throw (That e)
      (Nothing, Nothing) -> Nothing

--------------------------------------------------------------------------------

-- | The disjunction of two predicates.
--
--   >>> isRight (refine @(Or Even Odd) @Int 3)
--   True
--
--   >>> isRight (refine @(Or (LessThan 3) (GreaterThan 3)) @Int 2)
--   True
--
--   >>> isRight (refine @(Or Even Even) @Int 4)
--   True
--
--   @since 0.1.0.0
data Or l r
  = Or -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    , Generic1 -- ^ @since 0.3.0.0
    )

infixr 2 ||
-- | The disjunction of two predicates.
--
--   @since 0.2.0.0
type (||) = Or

-- | @since 0.2.0.0
instance ( Predicate l x, Predicate r x, Typeable l, Typeable r
         ) => Predicate (Or l r) x where
  validate p x = do
    let left  = validate @l undefined x
    let right = validate @r undefined x
    case (left, right) of
      (Just l, Just r) -> Just (RefineOrException (typeOf p) l r)
      _                -> Nothing

--------------------------------------------------------------------------------

-- | The exclusive disjunction of two predicates.
--
--
--   >>> isRight (refine @(Xor Even Odd) @Int 3)
--   True
--
--   >>> isLeft (refine @(Xor (LessThan 3) (EqualTo 2)) @Int 2)
--   True
--
--   >>> isLeft (refine @(Or Even Even) @Int 2)
--   True
--
--   @since 0.5
data Xor l r
  = Xor -- ^ @since 0.5
  deriving
    ( Generic -- ^ @since 0.5
    , Generic1 -- ^ @since 0.5
    )

-- not provided because it clashes with GHC.TypeLits.^
-- infixr 8 ^
-- The exclusive disjunction of two predicates.
-- type (^) = Xor

-- | @since 0.5
instance ( Predicate l x, Predicate r x, Typeable l, Typeable r
         ) => Predicate (Xor l r) x where
  validate p x = do
    let left = validate @l undefined x
    let right = validate @r undefined x
    case (left, right) of
      (Nothing, Nothing) -> Just (RefineXorException (typeOf p) Nothing)
      (Just  l, Just  r) -> Just (RefineXorException (typeOf p) (Just (l, r)))
      _ -> Nothing

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value has a length
-- which is less than the specified type-level number.
--
--   >>> isRight (refine @(SizeLessThan 4) @[Int] [1,2,3])
--   True
--
--   >>> isLeft (refine @(SizeLessThan 5) @[Int] [1,2,3,4,5])
--   True
--
--   >>> isRight (refine @(SizeLessThan 4) @Text "Hi")
--   True
--
--   >>> isLeft (refine @(SizeLessThan 4) @Text "Hello")
--   True
--
--   @since 0.2.0.0
data SizeLessThan (n :: Nat)
  = SizeLessThan -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.2.0.0
instance (Foldable t, KnownNat n) => Predicate (SizeLessThan n) (t a) where
  validate p x = sized p (x, "Foldable") length ((<), "less than")
-- | @since 0.5
instance (KnownNat n) => Predicate (SizeLessThan n) Text where
  validate p x = sized p (x, "Text") Text.length ((<), "less than")

-- | @since 0.5
instance (KnownNat n) => Predicate (SizeLessThan n) BS.ByteString where
  validate p x = sized p (x, "ByteString") BS.length ((<), "less than")

-- | @since 0.5
instance (KnownNat n) => Predicate (SizeLessThan n) BL.ByteString where
  validate p x = sized p (x, "ByteString") (fromIntegral . BL.length) ((<), "less than")

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value has a length
-- which is greater than the specified type-level number.
--
--   >>> isLeft (refine  @(SizeGreaterThan 3) @[Int] [1,2,3])
--   True
--
--   >>> isRight (refine @(SizeGreaterThan 3) @[Int] [1,2,3,4,5])
--   True
--
--   >>> isLeft (refine @(SizeGreaterThan 4) @Text "Hi")
--   True
--
--   >>> isRight (refine @(SizeGreaterThan 4) @Text "Hello")
--   True
--
--   @since 0.2.0.0
data SizeGreaterThan (n :: Nat)
  = SizeGreaterThan -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.2.0.0
instance (Foldable t, KnownNat n) => Predicate (SizeGreaterThan n) (t a) where
  validate p x = sized p (x, "Foldable") length ((>), "greater than")

-- | @since 0.5
instance (KnownNat n) => Predicate (SizeGreaterThan n) Text where
  validate p x = sized p (x, "Text") Text.length ((>), "greater than")

-- | @since 0.5
instance (KnownNat n) => Predicate (SizeGreaterThan n) BS.ByteString where
  validate p x = sized p (x, "ByteString") BS.length ((>), "greater than")

-- | @since 0.5
instance (KnownNat n) => Predicate (SizeGreaterThan n) BL.ByteString where
  validate p x = sized p (x, "ByteString") (fromIntegral . BL.length) ((>), "greater than")

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value has a length
-- which is equal to the specified type-level number.
--
--   >>> isRight (refine @(SizeEqualTo 4) @[Int] [1,2,3,4])
--   True
--
--   >>> isLeft (refine @(SizeEqualTo 35) @[Int] [1,2,3,4])
--   True
--
--   >>> isRight (refine @(SizeEqualTo 4) @Text "four")
--   True
--
--   >>> isLeft (refine @(SizeEqualTo 35) @Text "four")
--   True
--
--   @since 0.2.0.0
data SizeEqualTo (n :: Nat)
  = SizeEqualTo -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.2.0.0
instance (Foldable t, KnownNat n) => Predicate (SizeEqualTo n) (t a) where
  validate p x = sized p (x, "Foldable") length ((==), "equal to")

-- | @since 0.5
instance (KnownNat n) => Predicate (SizeEqualTo n) Text where
  validate p x = sized p (x, "Text") Text.length ((==), "equal to")

-- | @since 0.5
instance (KnownNat n) => Predicate (SizeEqualTo n) BS.ByteString where
  validate p x = sized p (x, "ByteString") BS.length ((==), "equal to")

-- | @since 0.5
instance (KnownNat n) => Predicate (SizeEqualTo n) BL.ByteString where
  validate p x = sized p (x, "ByteString") (fromIntegral . BL.length) ((==), "equal to")

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the 'Foldable' contains elements
-- in a strictly ascending order.
--
--   >>> isRight (refine @Ascending @[Int] [5, 8, 13, 21, 34])
--   True
--
--   >>> isLeft (refine @Ascending @[Int] [34, 21, 13, 8, 5])
--   True
--
--   @since 0.2.0.0
data Ascending
  = Ascending -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.2.0.0
instance (Foldable t, Ord a) => Predicate Ascending (t a) where
  validate p x = do
    if increasing x
    then Nothing
    else throwRefineOtherException
         (typeOf p)
         "Foldable is not in ascending order."

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the 'Foldable' contains elements
-- in a strictly descending order.
--
--   >>> isRight (refine @Descending @[Int] [34, 21, 13, 8, 5])
--   True
--
--   >>> isLeft (refine @Descending @[Int] [5, 8, 13, 21, 34])
--   True
--
--   @since 0.2.0.0
data Descending
  = Descending -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.2.0.0
instance (Foldable t, Ord a) => Predicate Descending (t a) where
  validate p x = do
    if decreasing x
    then Nothing
    else throwRefineOtherException
        (typeOf p)
        "Foldable is not in descending order."

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is less than the
--   specified type-level number.
--
--   >>> isRight (refine @(LessThan 12) @Int 11)
--   True
--
--   >>> isLeft (refine @(LessThan 12) @Int 12)
--   True
--
--   @since 0.1.0.0
data LessThan (n :: Nat)
  = LessThan -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.1.0.0
instance (Ord x, Num x, KnownNat n) => Predicate (LessThan n) x where
  validate p x = do
    let x' = fromIntegral (natVal p)
    if x < x'
    then Nothing
    else throwRefineOtherException
         (typeOf p)
         ("Value is not less than " <> PP.pretty (natVal p))

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is greater than the
--   specified type-level number.
--
--   >>> isRight (refine @(GreaterThan 65) @Int 67)
--   True
--
--   >>> isLeft (refine @(GreaterThan 65) @Int 65)
--   True
--
--   @since 0.1.0.0
data GreaterThan (n :: Nat)
  = GreaterThan -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.1.0.0
instance (Ord x, Num x, KnownNat n) => Predicate (GreaterThan n) x where
  validate p x = do
    let x' = fromIntegral (natVal p)
    if x > x'
    then Nothing
    else throwRefineOtherException
         (typeOf p)
         ("Value is not greater than " <> PP.pretty (natVal p))

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is greater than or equal to the
--   specified type-level number.
--
--   >>> isRight (refine @(From 9) @Int 10)
--   True
--
--   >>> isRight (refine @(From 10) @Int 10)
--   True
--
--   >>> isLeft (refine @(From 11) @Int 10)
--   True
--
--   @since 0.1.2
data From (n :: Nat)
  = From -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.1.2
instance (Ord x, Num x, KnownNat n) => Predicate (From n) x where
  validate p x = do
    let x' = fromIntegral (natVal p)
    if x >= x'
    then Nothing
    else throwRefineOtherException
         (typeOf p)
         ("Value is less than " <> PP.pretty (natVal p))

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is less than or equal to the
--   specified type-level number.
--
--   >>> isRight (refine @(To 23) @Int 17)
--   True
--
--   >>> isLeft (refine @(To 17) @Int 23)
--   True
--
--   @since 0.1.2
data To (n :: Nat)
  = To -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.1.2
instance (Ord x, Num x, KnownNat n) => Predicate (To n) x where
  validate p x = do
    let x' = fromIntegral (natVal p)
    if x <= x'
    then Nothing
    else throwRefineOtherException
         (typeOf p)
         ("Value is greater than " <> PP.pretty (natVal p))

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is within an inclusive range.
--
--   >>> isRight (refine @(FromTo 0 16) @Int 13)
--   True
--
--   >>> isRight (refine @(FromTo 13 15) @Int 13)
--   True
--
--   >>> isRight (refine @(FromTo 13 15) @Int 15)
--   True
--
--   >>> isLeft (refine @(FromTo 13 15) @Int 12)
--   True
--
--   @since 0.1.2
data FromTo (mn :: Nat) (mx :: Nat)
  = FromTo -- ^ @since 0.4.2
  deriving
    ( Generic-- ^ @since 0.3.0.0
    )

-- | @since 0.1.2
instance ( Ord x, Num x, KnownNat mn, KnownNat mx, mn <= mx
         ) => Predicate (FromTo mn mx) x where
  validate p x = do
    let mn' = natVal (Proxy @mn)
    let mx' = natVal (Proxy @mx)
    if x >= fromIntegral mn' && x <= fromIntegral mx'
    then Nothing
    else
      let msg = [ "Value is out of range (minimum: "
                , PP.pretty mn'
                , ", maximum: "
                , PP.pretty mx'
                , ")"
                ] |> mconcat
      in throwRefineOtherException (typeOf p) msg

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is equal to the specified
--   type-level number @n@.
--
--   >>> isRight (refine @(EqualTo 5) @Int 5)
--   True
--
--   >>> isLeft (refine @(EqualTo 6) @Int 5)
--   True
--
--   @since 0.1.0.0
data EqualTo (n :: Nat)
  = EqualTo -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.1.0.0
instance (Eq x, Num x, KnownNat n) => Predicate (EqualTo n) x where
  validate p x = do
    let x' = fromIntegral (natVal p)
    if x == x'
    then Nothing
    else throwRefineOtherException
         (typeOf p)
         ("Value does not equal " <> PP.pretty (natVal p))

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is not equal to the specified
--   type-level number @n@.
--
--   >>> isRight (refine @(NotEqualTo 6) @Int 5)
--   True
--
--   >>> isLeft (refine @(NotEqualTo 5) @Int 5)
--   True
--
--   @since 0.2.0.0
data NotEqualTo (n :: Nat)
  = NotEqualTo -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.2.0.0
instance (Eq x, Num x, KnownNat n) => Predicate (NotEqualTo n) x where
  validate p x = do
    let x' = fromIntegral (natVal p)
    if x /= x'
    then Nothing
    else throwRefineOtherException
         (typeOf p)
         ("Value does equal " <> PP.pretty (natVal p))

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is greater or equal than a negative
--   number specified as a type-level (positive) number @n@ and less than a
--   type-level (positive) number @m@.
--
--   >>> isRight (refine @(NegativeFromTo 5 12) @Int (-3))
--   True
--
--   >>> isLeft (refine @(NegativeFromTo 4 3) @Int (-5))
--   True
--
--   @since 0.4
data NegativeFromTo (n :: Nat) (m :: Nat)
  = NegativeFromTo -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.4
instance (Ord x, Num x, KnownNat n, KnownNat m) => Predicate (NegativeFromTo n m) x where
  validate p x = do
    let n' = natVal (Proxy @n)
    let m' = natVal (Proxy @m)
    if x >= fromIntegral (negate n') && x <= fromIntegral m'
    then Nothing
    else
      let msg = [ "Value is out of range (minimum: "
                , PP.pretty (negate n')
                , ", maximum: "
                , PP.pretty m'
                , ")"
                ] |> mconcat
      in throwRefineOtherException (typeOf p) msg

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is divisible by @n@.
--
--   >>> isRight (refine @(DivisibleBy 3) @Int 12)
--   True
--
--   >>> isLeft (refine @(DivisibleBy 2) @Int 37)
--   True
--
--   @since 0.4.2
data DivisibleBy (n :: Nat)
  = DivisibleBy -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.4.2
instance (Integral x, KnownNat n) => Predicate (DivisibleBy n) x where
  validate p x = do
    let x' = fromIntegral (natVal p)
    if x `mod` x' == 0
    then Nothing
    else throwRefineOtherException
         (typeOf p)
         ("Value is not divisible by " <> PP.pretty (natVal p))

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is odd.
--
--   >>> isRight (refine @Odd @Int 33)
--   True
--
--   >>> isLeft (refine @Odd @Int 32)
--   True
--
--   @since 0.4.2
data Odd
  = Odd -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | @since 0.4.2
instance (Integral x) => Predicate Odd x where
  validate p x = do
    if odd x
    then Nothing
    else throwRefineOtherException
         (typeOf p)
         "Value is not odd."

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is IEEE "not-a-number" (NaN).
--
--   >>> isRight (refine @NaN @Double) (0/0)
--   True
--
--   >>> isLeft (refine @NaN @Double) 13.9
--   True
--
--   @since 0.5
data NaN
  = NaN -- ^ @since 0.5
  deriving
    ( Generic -- ^ @since 0.5
    )

-- | @since 0.5
instance (RealFloat x) => Predicate NaN x where
  validate p x = do
    if isNaN x
    then Nothing
    else throwRefineOtherException
         (typeOf p)
         "Value is not IEEE \"not-a-number\" (NaN)."

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is IEEE infinity or negative infinity.
--
--   >>> isRight (refine @Infinite @Double) (1/0)
--   True
--
--   >>> isRight (refine @Infinite @Double) (-1/0)
--   True
--
--   >>> isLeft (refine @Infinite @Double) 13.20
--   True
--
--   @since 0.5
data Infinite
  = Infinite -- ^ @since 0.5
  deriving
    ( Generic -- ^ @since 0.5
    )

-- | @since 0.5
instance (RealFloat x) => Predicate Infinite x where
  validate p x = do
    if isInfinite x
    then Nothing
    else throwRefineOtherException
         (typeOf p)
         "Value is not IEEE infinity or negative infinity."

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is even.
--
--   >>> isRight (refine @Even @Int 32)
--   True
--
--   >>> isLeft (refine @Even @Int 33)
--   True
--
--   @since 0.4.2
data Even
  = Even -- ^ @since 0.4.2
  deriving
    ( Generic -- ^ @since 0.4.2
    )

-- | @since 0.4.2
instance (Integral x) => Predicate Even x where
  validate p x = do
    if even x
    then Nothing
    else throwRefineOtherException
         (typeOf p)
         "Value is not even."

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is greater than zero.
--
--   @since 0.1.0.0
type Positive = GreaterThan 0

-- | A 'Predicate' ensuring that the value is less than or equal to zero.
--
--   @since 0.1.2
type NonPositive = To 0

-- | A 'Predicate' ensuring that the value is less than zero.
--
--   @since 0.1.0.0
type Negative = LessThan 0

-- | A 'Predicate' ensuring that the value is greater than or equal to zero.
--
--   @since 0.1.2
type NonNegative = From 0

-- | An inclusive range of values from zero to one.
--
--   @since 0.1.0.0
type ZeroToOne = FromTo 0 1

-- | A 'Predicate' ensuring that the value is not equal to zero.
--
--   @since 0.2.0.0
type NonZero = NotEqualTo 0

-- | A 'Predicate' ensuring that the type is non-empty.
--
--   @since 0.5
type Empty = SizeEqualTo 0

-- | A 'Predicate' ensuring that the type is non-empty.
--
--   @since 0.2.0.0
type NonEmpty = SizeGreaterThan 0

--------------------------------------------------------------------------------

-- | A typeclass containing "safe" conversions between refined
--   predicates where the target is /weaker/ than the source:
--   that is, all values that satisfy the first predicate will
--   be guaranteed to satisy the second.
--
--   Take care: writing an instance declaration for your custom
--   predicates is the same as an assertion that 'weaken' is
--   safe to use:
--
--   @
--   instance 'Weaken' Pred1 Pred2
--   @
--
--   For most of the instances, explicit type annotations for
--   the result value's type might be required.
--
-- @since 0.2.0.0
class Weaken from to where
  weaken :: Refined from x -> Refined to x
  weaken = coerce

-- | @since 0.2.0.0
instance (n <= m)         => Weaken (LessThan n)    (LessThan m)
-- | @since 0.2.0.0
instance (n <= m)         => Weaken (LessThan n)    (To m)
-- | @since 0.2.0.0
instance (n <= m)         => Weaken (To n)          (To m)
-- | @since 0.2.0.0
instance (m <= n)         => Weaken (GreaterThan n) (GreaterThan m)
-- | @since 0.2.0.0
instance (m <= n)         => Weaken (GreaterThan n) (From m)
-- | @since 0.2.0.0
instance (m <= n)         => Weaken (From n)        (From m)
-- | @since 0.2.0.0
instance (p <= n, m <= q) => Weaken (FromTo n m)    (FromTo p q)
-- | @since 0.2.0.0
instance (p <= n)         => Weaken (FromTo n m)    (From p)
-- | @since 0.2.0.0
instance (m <= q)         => Weaken (FromTo n m)    (To q)

-- | This function helps type inference.
--   It is equivalent to the following:
--
-- @
-- instance Weaken (And l r) l
-- @
--
--   @since 0.2.0.0
andLeft :: Refined (And l r) x -> Refined l x
andLeft = coerce

-- | This function helps type inference.
--   It is equivalent to the following:
--
-- @
-- instance Weaken (And l r) r
-- @
--
--   @since 0.2.0.0
andRight :: Refined (And l r) x -> Refined r x
andRight = coerce

-- | This function helps type inference.
--   It is equivalent to the following:
--
-- @
-- instance Weaken l (Or l r)
-- @
--
--   @since 0.2.0.0
leftOr :: Refined l x -> Refined (Or l r) x
leftOr = coerce

-- | This function helps type inference.
--   It is equivalent to the following:
--
-- @
-- instance Weaken r (Or l r)
-- @
--
--   @since 0.2.0.0
rightOr :: Refined r x -> Refined (Or l r) x
rightOr = coerce

-- | Strengthen a refinement by composing it with another.
--
--   @since 0.4.2.2
strengthen :: forall p p' x. (Predicate p x, Predicate p' x)
  => Refined p x
  -> Either RefineException (Refined (p && p') x)
strengthen r = refine @(p && p') (unrefine r)
{-# inlineable strengthen #-}

--------------------------------------------------------------------------------

-- | An exception encoding the way in which a 'Predicate' failed.
--
--   @since 0.2.0.0
data RefineException
  = -- | A 'RefineException' for failures involving the 'Not' predicate.
    --
    --   @since 0.2.0.0
    RefineNotException
    { _RefineException_typeRep   :: !TypeRep
      -- ^ The 'TypeRep' of the @'Not' p@ type.
    }

  | -- | A 'RefineException' for failures involving the 'And' predicate.
    --
    --   @since 0.2.0.0
    RefineAndException
    { _RefineException_typeRep   :: !TypeRep
      -- ^ The 'TypeRep' of the @'And' l r@ type.
    , _RefineException_andChild  :: !(These RefineException RefineException)
      -- ^ A 'These' encoding which branch(es) of the 'And' failed:
      --   if the 'RefineException' came from the @l@ predicate, then
      --   this will be 'This', if it came from the @r@ predicate, this
      --   will be 'That', and if it came from both @l@ and @r@, this
      --   will be 'These'.

      -- note to self: what am I, Dr. Seuss?
    }

  | -- | A 'RefineException' for failures involving the 'Or' predicate.
    --
    --   @since 0.2.0.0
    RefineOrException
    { _RefineException_typeRep   :: !TypeRep
      -- ^ The 'TypeRep' of the @'Or' l r@ type.
    , _RefineException_orLChild  :: !RefineException
      -- ^ The 'RefineException' for the @l@ failure.
    , _RefineException_orRChild  :: !RefineException
      -- ^ The 'RefineException' for the @l@ failure.
    }

  | -- | A 'RefineException' for failures involving the 'Xor' predicate.
    --
    --   @since 0.5
    RefineXorException
    { _RefineException_typeRep   :: !TypeRep
    , _RefineException_children  :: !(Maybe (RefineException, RefineException))
    }

  | -- | A 'RefineException' for failures involving all other predicates.
    --
    --   @since 0.2.0.0
    RefineOtherException
    { _RefineException_typeRep   :: !TypeRep
      -- ^ The 'TypeRep' of the predicate that failed.
    , _RefineException_message   :: !(PP.Doc Void)
      -- ^ A custom message to display.
    }
  | -- | A 'RefineException' for failures involving all other predicates with custom exception.
    --
    --   @since 0.5
    RefineSomeException
    { _RefineException_typeRep   :: !TypeRep
      -- ^ The 'TypeRep' of the predicate that failed.
    , _RefineException_Exception :: !SomeException
      -- ^ A custom exception.
    }
  deriving
    ( Generic -- ^ @since 0.3.0.0
    )

-- | /Note/: Equivalent to @'displayRefineException'@.
--
--   @since 0.2.0.0
instance Show RefineException where
  show = PP.pretty .> show

data ExceptionTree a
  = NodeNone
  | NodeSome !TypeRep SomeException
  | NodeOther !TypeRep !(PP.Doc Void)
  | NodeNot !TypeRep
  | NodeOr !TypeRep [ExceptionTree a]
  | NodeAnd !TypeRep [ExceptionTree a]
  | NodeXor !TypeRep [ExceptionTree a]

showTree :: ExceptionTree RefineException -> PP.Doc ann
showTree = PP.pretty . unlines . showOne "  " "" ""
  where
    showOne :: String -> String -> String -> ExceptionTree RefineException -> [String]
    showOne leader tie arm = \case
      NodeNone ->
        [
        ]
      NodeSome tr e ->
        [ leader
          <> arm
          <> tie
          <> "The predicate ("
          <> show tr
          <> ") failed with the exception: "
          <> displayException e
          <> "."
        ]
      NodeOther tr p ->
        [ leader
          <> arm
          <> tie
          <> "The predicate ("
          <> show tr
          <> ") failed with the message: "
          <> show p
          <> "."
        ]
      NodeNot tr ->
        [ leader
          <> arm
          <> tie
          <> "The predicate ("
          <> show tr
          <> ") does not hold."
        ]
      NodeOr tr rest -> nodeRep tr : showChildren rest (leader <> extension)
      NodeAnd tr rest -> nodeRep tr : showChildren rest (leader <> extension)
      -- can be empty since both can be satisfied
      NodeXor tr [] ->
        [ leader
          <> arm
          <> tie
          <> "The predicate ("
          <> show tr
          <> ") does not hold, because both predicates were satisfied."
        ]
      NodeXor tr rest -> nodeRep tr : showChildren rest (leader <> extension)
      where
        nodeRep :: TypeRep -> String
        -- TODO: make tr bold
        nodeRep tr = leader <> arm <> tie <> show tr

        extension :: String
        extension = case arm of
          ""  -> ""
          "└" -> "    "
          _   -> "│   "

    showChildren :: [ExceptionTree RefineException] -> String -> [String]
    showChildren children leader =
      let arms = replicate (length children - 1) "├" <> ["└"]
      in concat (zipWith (showOne leader "── ") arms children)

refineExceptionToTree :: RefineException -> ExceptionTree RefineException
refineExceptionToTree = go
  where
    go = \case
      RefineSomeException tr e -> NodeSome tr e
      RefineOtherException tr p -> NodeOther tr p
      RefineNotException tr -> NodeNot tr
      RefineOrException tr l r -> NodeOr tr [go l, go r]
      RefineAndException tr (This l) -> NodeAnd tr [go l]
      RefineAndException tr (That r) -> NodeAnd tr [go r]
      RefineAndException tr (These l r) -> NodeAnd tr [go l, go r]
      RefineXorException tr Nothing -> NodeXor tr []
      RefineXorException tr (Just (l, r)) -> NodeXor tr [go l, go r]

-- | Display a 'RefineException' as a @'PP.Doc' ann@
--
--   This function can be extremely useful for debugging
--   @'RefineException's@, especially deeply nested ones.
--
--   Consider:
--
--   @
--   myRefinement = refine
--     \@(And
--         (Not (LessThan 5))
--         (Xor
--           (DivisibleBy 10)
--           (And
--             (EqualTo 4)
--             (EqualTo 3)
--           )
--         )
--      )
--     \@Int
--     3
--   @
--
--   This function will show the following tree structure, recursively breaking down
--   every issue:
--
--   @
--   And (Not (LessThan 5)) (Xor (EqualTo 4) (And (EqualTo 4) (EqualTo 3)))
--   ├── The predicate (Not (LessThan 5)) does not hold.
--   └── Xor (DivisibleBy 10) (And (EqualTo 4) (EqualTo 3))
--       ├── The predicate (DivisibleBy 10) failed with the message: Value is not divisible by 10
--       └── And (EqualTo 4) (EqualTo 3)
--           └── The predicate (EqualTo 4) failed with the message: Value does not equal 4
--   @
--
--   /Note/: Equivalent to @'show' \@'RefineException'@
--
--   @since 0.2.0.0
displayRefineException :: RefineException -> PP.Doc ann
displayRefineException = refineExceptionToTree .> showTree

-- | Pretty-print a 'RefineException'.
--
--   /Note/: Equivalent to 'displayRefineException'.
--
--   @since 0.2.0.0
instance PP.Pretty RefineException where
  pretty = displayRefineException

-- | Encode a 'RefineException' for use with \Control.Exception\.
--
--   /Note/: Equivalent to @'displayRefineException'@.
--
--   @since 0.2.0.0
instance Exception RefineException where
  displayException = show

--------------------------------------------------------------------------------

-- | A handler for a @'RefineException'@.
--
--   'throwRefineOtherException' is useful for defining what
--   behaviour 'validate' should have in the event of a predicate failure.
--
--   @since 0.2.0.0
throwRefineOtherException
  :: TypeRep
  -- ^ The 'TypeRep' of the 'Predicate'. This can usually be given by using 'typeOf'.
  -> PP.Doc Void
  -- ^ A 'PP.Doc' 'Void' encoding a custom error message to be pretty-printed.
  -> Maybe RefineException
throwRefineOtherException rep
  = RefineOtherException rep .> Just

-- | A handler for a @'RefineException'@.
--
--   'throwRefineSomeException' is useful for defining what
--   behaviour 'validate' should have in the event of a predicate failure
--   with a specific exception.
--
--   @since 0.5
throwRefineSomeException
  :: TypeRep
  -- ^ The 'TypeRep' of the 'Predicate'. This can usually be given by using 'typeOf'.
  -> SomeException
  -- ^ A custom exception.
  -> Maybe RefineException
throwRefineSomeException rep
  = RefineSomeException rep .> Just

-- | An implementation of 'validate' that always succeeds.
--
--   ==== __Examples__
--
--   @
--   data ContainsLetterE = ContainsLetterE
--
--   instance Predicate ContainsLetterE 'Text' where
--     validate _ t = do
--       if 'Data.Text.any' ('==' \'e\') t
--       then 'Nothing'
--       else 'success'
--   @
--
--   @since 0.5
success
  :: Maybe RefineException
success
  = Nothing

--------------------------------------------------------------------------------

-- | Helper function for sized predicates.
sized :: (Typeable (p n), KnownNat n)
  => p n
     -- ^ predicate
  -> (a, PP.Doc Void)
     -- ^ (value, type)
  -> (a -> Int)
     -- ^ length of value
  -> (Int -> Int -> Bool, PP.Doc Void)
     -- ^ (compare :: Length -> KnownNat -> Bool, comparison string)
  -> Maybe RefineException
sized p (x, typ) lenF (cmp, cmpDesc) = do
  let x' = fromIntegral (natVal p)
  let sz = lenF x
  if cmp sz x'
  then Nothing
  else
    let msg =
          [ "Size of ", typ, " is not ", cmpDesc
          , PP.pretty x'
          , "\n  "
          , "Size is: "
          , PP.pretty sz
          ] |> mconcat
    in throwRefineOtherException (typeOf p) msg
