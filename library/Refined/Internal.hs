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

{-# OPTIONS_GHC -Wall                        #-}
{-# OPTIONS_GHC -funbox-strict-fields        #-}

--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

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
module Refined.Internal
  ( -- * 'Refined'
    Refined(Refined)

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
  , NegativeFromTo

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
 
  , (|>)
  , (.>)
 ) where

--------------------------------------------------------------------------------

import           Prelude
                 (Num, fromIntegral, negate, undefined)

import           Control.Applicative          (Applicative (pure))
import           Control.Exception            (Exception (displayException))
import           Control.Monad                (Monad, unless)
import           Data.Bool                    (Bool(True,False),(&&), otherwise)
import           Data.Coerce                  (coerce)
import           Data.Either
                 (Either (Left, Right), either)
import           Data.Eq                      (Eq, (==), (/=))
import           Data.Foldable                (Foldable(length, foldl'))
import           Data.Function                (const, flip, ($), (.))
import           Data.Functor                 (Functor, fmap)
import           Data.Functor.Identity        (Identity (runIdentity))
import           Data.Monoid                  (mconcat)
import           Data.Ord                     (Ord, (<), (<=), (>), (>=))
import           Data.Proxy                   (Proxy (Proxy))
import           Data.Semigroup               (Semigroup((<>)))
import           Data.Typeable                (TypeRep, Typeable, typeOf)
import           Data.Void                    (Void)
import           Text.Read                    (Read (readsPrec), lex, readParen)
import           Text.Show                    (Show (show))

import           Control.Monad.Catch          (MonadThrow)
import qualified Control.Monad.Catch          as MonadThrow
import           Control.Monad.Error.Class    (MonadError)
import qualified Control.Monad.Error.Class    as MonadError
import           Control.Monad.Fail           (MonadFail, fail)
import           Control.Monad.Fix            (MonadFix, fix)
import           Control.Monad.Trans.Class    (MonadTrans (lift))

import           Control.Monad.Trans.Except   (ExceptT)
import qualified Control.Monad.Trans.Except   as ExceptT

import           GHC.Generics                 (Generic, Generic1)
import           GHC.TypeLits                 (type (<=), KnownNat, Nat, natVal)

import           Refined.These                (These(This,That,These))

import qualified Data.Text.Prettyprint.Doc    as PP
import qualified Language.Haskell.TH.Syntax   as TH

--------------------------------------------------------------------------------

infixl 0 |>
infixl 9 .>

-- | Helper function, stolen from the 'flow' package.
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
{-# INLINE (|>) #-}

-- | Helper function, stolen from the 'flow' package.
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

-- | A refinement type, which wraps a value of type @x@,
--   ensuring that it satisfies a type-level predicate @p@.
newtype Refined p x = Refined x
  deriving (Eq, Foldable , Ord, Show, Typeable) 

type role Refined nominal nominal

-- | This instance makes sure to check the refinement.
instance (Read x, Predicate p x) => Read (Refined p x) where
  readsPrec d = readParen (d > 10) $ \r1 -> do
    ("Refined", r2) <- lex r1
    (raw,       r3) <- readsPrec 11 r2
    case refine raw of
      Right val -> [(val, r3)]
      Left  _   -> []

instance (TH.Lift x) => TH.Lift (Refined p x) where
  lift (Refined a) = [|Refined a|]

--------------------------------------------------------------------------------

-- | A smart constructor of a 'Refined' value.
--   Checks the input value at runtime.
refine :: (Predicate p x) => x -> Either RefineException (Refined p x)
refine x = do
  let predicateByResult :: RefineM (Refined p x) -> p
      predicateByResult = const undefined
  runRefineM $ fix $ \result -> do
    validate (predicateByResult result) x
    pure (Refined x)
{-# INLINABLE refine #-}

-- | Constructs a 'Refined' value at run-time,
--   calling 'Control.Monad.Catch.throwM' if the value
--   does not satisfy the predicate.
refineThrow :: (Predicate p x, MonadThrow m) => x -> m (Refined p x)
refineThrow = refine .> either MonadThrow.throwM pure
{-# INLINABLE refineThrow #-}

-- | Constructs a 'Refined' value at run-time,
--   calling 'Control.Monad.Fail.fail' if the value
--   does not satisfy the predicate.
refineFail :: (Predicate p x, MonadFail m) => x -> m (Refined p x)
refineFail = refine .> either (displayException .> fail) pure
{-# INLINABLE refineFail #-}

-- | Constructs a 'Refined' value at run-time,
--   calling 'Control.Monad.Error.throwError' if the value
--   does not satisfy the predicate.
refineError :: (Predicate p x, MonadError RefineException m)
            => x -> m (Refined p x)
refineError = refine .> either MonadError.throwError pure
{-# INLINABLE refineError #-}

--------------------------------------------------------------------------------

-- | Constructs a 'Refined' value at compile-time using @-XTemplateHaskell@.
--
--   For example:
--
--   >>> $$(refineTH 23) :: Refined Positive Int
--   Refined 23
--
--   Here's an example of an invalid value:
--
--   >>> $$(refineTH 0) :: Refined Positive Int
--   <interactive>:6:4:
--       Value is not greater than 0
--       In the Template Haskell splice $$(refineTH 0)
--       In the expression: $$(refineTH 0) :: Refined Positive Int
--       In an equation for ‘it’:
--           it = $$(refineTH 0) :: Refined Positive Int
--
--   If it's not evident, the example above indicates a compile-time failure,
--   which means that the checking was done at compile-time, thus introducing a
--   zero runtime overhead compared to a plain value construction.
--
--   It may be useful to use this function with the `th-lift-instances` package at https://hackage.haskell.org/package/th-lift-instances/
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

--------------------------------------------------------------------------------

-- | Extracts the refined value.
{-# INLINE unrefine #-}
unrefine :: Refined p x -> x
unrefine = coerce

--------------------------------------------------------------------------------

-- | A typeclass which defines a runtime interpretation of
--   a type-level predicate @p@ for type @x@.
class (Typeable p) => Predicate p x where
  {-# MINIMAL validate #-} 
  -- | Check the value @x@ according to the predicate @p@,
  --   producing an error string if the value does not satisfy.
  validate :: (Monad m) => p -> x -> RefineT m ()

--------------------------------------------------------------------------------

-- | A predicate which is satisfied for all types.
data IdPred
  deriving (Generic)

instance Predicate IdPred x where
  validate _ _ = pure ()
  {-# INLINE validate #-}

--------------------------------------------------------------------------------

-- | The negation of a predicate.
data Not p
  deriving (Generic, Generic1)

instance (Predicate p x, Typeable p) => Predicate (Not p) x where
  validate p x = do
    result <- runRefineT (validate @p undefined x)
    case result of
      Left r -> throwRefine (RefineNotException (typeOf p) r)
      Right () -> pure ()

--------------------------------------------------------------------------------

-- | The conjunction of two predicates.
data And l r
  deriving (Generic, Generic1)

infixr 3 &&
-- | The conjunction of two predicates.
type (&&) = And

instance ( Predicate l x, Predicate r x, Typeable l, Typeable r
         ) => Predicate (And l r) x where
  validate p x = do
    a <- lift $ runRefineT $ validate @l undefined x
    b <- lift $ runRefineT $ validate @r undefined x
    let throw err = throwRefine (RefineAndException (typeOf p) err)
    case (a, b) of
      (Left  e, Left e1) -> throw (These e e1)
      (Left  e,       _) -> throw (This e)
      (Right _, Left  e) -> throw (That e)
      (Right _, Right _) -> pure ()

--------------------------------------------------------------------------------

-- | The disjunction of two predicates.
data Or l r
  deriving (Generic, Generic1)

infixr 2 ||
-- | The disjunction of two predicates.
type (||) = Or

instance ( Predicate l x, Predicate r x, Typeable l, Typeable r
         ) => Predicate (Or l r) x where
  validate p x = do
    left  <- lift $ runRefineT $ validate @l undefined x
    right <- lift $ runRefineT $ validate @r undefined x
    case (left, right) of
      (Left l, Left r) -> throwRefine (RefineOrException (typeOf p) l r)
      _                -> pure ()

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the 'Foldable' has a length
-- which is less than the specified type-level number.
data SizeLessThan (n :: Nat)
  deriving (Generic)

instance (Foldable t, KnownNat n) => Predicate (SizeLessThan n) (t a) where
  validate p x = do
    let x' = natVal p
        sz = length x
    unless (sz < fromIntegral x') $ do
      throwRefineOtherException (typeOf p)
        ( [ "Size of Foldable is not less than "
          , PP.pretty x'
          , newline
          , twoSpaces
          , "Size is: "
          , PP.pretty sz
          ] |> mconcat
        )

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the 'Foldable' has a length
-- which is greater than the specified type-level number.
data SizeGreaterThan (n :: Nat)
  deriving (Generic)

instance (Foldable t, KnownNat n) => Predicate (SizeGreaterThan n) (t a) where
  validate p x = do
    let x' = natVal p
        sz = length x
    unless (sz > fromIntegral x') $ do
      throwRefineOtherException (typeOf p)
        ( [ "Size of Foldable is not greater than "
          , PP.pretty x'
          , newline
          , twoSpaces
          , "Size is: "
          , PP.pretty sz
          ] |> mconcat
        )

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the 'Foldable' has a length
-- which is equal to the specified type-level number.
data SizeEqualTo (n :: Nat)
  deriving (Generic)

instance (Foldable t, KnownNat n) => Predicate (SizeEqualTo n) (t a) where
  validate p x = do
    let x' = natVal p
        sz = length x
    unless (sz == fromIntegral x') $ do
      throwRefineOtherException (typeOf p)
        ( [ "Size of Foldable is not equal to "
          , PP.pretty x'
          , newline
          , twoSpaces
          , "Size is: "
          , PP.pretty sz
          ] |> mconcat
        )

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the 'Foldable' contains elements
-- in a strictly ascending order.
data Ascending
  deriving (Generic)

instance (Foldable t, Ord a) => Predicate Ascending (t a) where
  validate p x = do
    unless (increasing x) $ do
      throwRefineOtherException (typeOf p) ( "Foldable is not in ascending order." )

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the 'Foldable' contains elements
-- in a strictly descending order.
data Descending
  deriving (Generic)

instance (Foldable t, Ord a) => Predicate Descending (t a) where
  validate p x = do
    unless (decreasing x) $ do
      throwRefineOtherException (typeOf p) ( "Foldable is not in descending order." )

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is less than the
--   specified type-level number.
data LessThan (n :: Nat)
  deriving (Generic)

instance (Ord x, Num x, KnownNat n) => Predicate (LessThan n) x where
  validate p x = do
    let x' = natVal p
    unless (x < fromIntegral x') $ do
      throwRefineOtherException (typeOf p) ( "Value is not less than " <> PP.pretty x' )

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is greater than the
--   specified type-level number.
data GreaterThan (n :: Nat)
  deriving (Generic)

instance (Ord x, Num x, KnownNat n) => Predicate (GreaterThan n) x where
  validate p x = do
    let x' = natVal p
    unless (x > fromIntegral x') $ do
      throwRefineOtherException (typeOf p) ( "Value is not greater than " <> PP.pretty x' )

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is greater than or equal to the
--   specified type-level number.
data From (n :: Nat)
  deriving (Generic)

instance (Ord x, Num x, KnownNat n) => Predicate (From n) x where
  validate p x = do
    let x' = natVal p
    unless (x >= fromIntegral x') $ do
      throwRefineOtherException (typeOf p) ( "Value is less than " <> PP.pretty x' )

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is less than or equal to the
--   specified type-level number.
data To (n :: Nat)
  deriving (Generic)

instance (Ord x, Num x, KnownNat n) => Predicate (To n) x where
  validate p x = do
    let x' = natVal p
    unless (x <= fromIntegral x') $ do
      throwRefineOtherException (typeOf p) ( "Value is greater than " <> PP.pretty x' )

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is within an inclusive range.
data FromTo (mn :: Nat) (mx :: Nat)
  deriving (Generic)

instance ( Ord x, Num x, KnownNat mn, KnownNat mx, mn <= mx
         ) => Predicate (FromTo mn mx) x where
  validate p x = do
    let mn' = natVal (Proxy @mn)
    let mx' = natVal (Proxy @mx)
    unless ((x >= fromIntegral mn') && (x <= fromIntegral mx')) $ do
      let msg = [ "Value is out of range (minimum: "
                , PP.pretty mn'
                , ", maximum: "
                , PP.pretty mx'
                , ")"
                ] |> mconcat
      throwRefineOtherException (typeOf p) msg

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is equal to the specified
--   type-level number @n@.
data EqualTo (n :: Nat)
  deriving (Generic)

instance (Eq x, Num x, KnownNat n) => Predicate (EqualTo n) x where
  validate p x = do
    let x' = natVal p
    unless (x == fromIntegral x') $ do
      throwRefineOtherException (typeOf p) ( "Value does not equal " <> PP.pretty x' )

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is not equal to the specified
--   type-level number @n@.
data NotEqualTo (n :: Nat)
  deriving (Generic)

instance (Eq x, Num x, KnownNat n) => Predicate (NotEqualTo n) x where
  validate p x = do
    let x' = natVal p
    unless (x /= fromIntegral x') $ do
      throwRefineOtherException (typeOf p) ( "Value does equal " <> PP.pretty x' )

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is greater or equal than a negative
--   number specified as a type-level (positive) number @n@ and less than a
--   type-level (positive) number @m@.
data NegativeFromTo (n :: Nat) (m :: Nat)
  deriving (Generic)

instance (Ord x, Num x, KnownNat n, KnownNat m, n <= m) => Predicate (NegativeFromTo n m) x where
  validate p x = do
    let n' = natVal (Proxy @n)
        m' = natVal (Proxy @m)
    unless (x >= negate (fromIntegral n') && x <= fromIntegral m') $ do
      let msg = [ "Value is out of range (minimum: "
                , PP.pretty (negate n')
                , ", maximum: "
                , PP.pretty m'
                , ")"
                ] |> mconcat
      throwRefineOtherException (typeOf p) msg

--------------------------------------------------------------------------------

-- | A 'Predicate' ensuring that the value is greater than zero.
type Positive = GreaterThan 0

-- | A 'Predicate' ensuring that the value is less than or equal to zero.
type NonPositive = To 0

-- | A 'Predicate' ensuring that the value is less than zero.
type Negative = LessThan 0

-- | A 'Predicate' ensuring that the value is greater than or equal to zero.
type NonNegative = From 0

-- | An inclusive range of values from zero to one.
type ZeroToOne = FromTo 0 1

-- | A 'Predicate' ensuring that the value is not equal to zero.
type NonZero = NotEqualTo 0

-- | A 'Predicate' ensuring that the 'Foldable' is non-empty.
type NonEmpty = SizeGreaterThan 0

--------------------------------------------------------------------------------

-- |
-- A typeclass containing "safe" conversions between refined predicates
-- where the target is /weaker/ than the source: that is, all values that
-- satisfy the first predicate will be guarunteed to satisy the second.
--
-- Take care: writing an instance declaration for your custom predicates is
-- the same as an assertion that 'weaken' is safe to use:
--
-- @
-- instance 'Weaken' Pred1 Pred2
-- @
--
-- For most of the instances, explicit type annotations for the result
-- value's type might be required.
class Weaken from to where
  weaken :: Refined from x -> Refined to x
  weaken = coerce

instance (n <= m)         => Weaken (LessThan n)    (LessThan m)
instance (n <= m)         => Weaken (LessThan n)    (To m)
instance (n <= m)         => Weaken (To n)          (To m)
instance (m <= n)         => Weaken (GreaterThan n) (GreaterThan m)
instance (m <= n)         => Weaken (GreaterThan n) (From m)
instance (m <= n)         => Weaken (From n)        (From m)
instance (p <= n, m <= q) => Weaken (FromTo n m)    (FromTo p q)
instance (p <= n)         => Weaken (FromTo n m)    (From p)
instance (m <= q)         => Weaken (FromTo n m)    (To q)

-- | This function helps type inference.
--   It is equivalent to the following:
--
-- @
-- instance Weaken (And l r) l
-- @
andLeft :: Refined (And l r) x -> Refined l x
andLeft = coerce

-- | This function helps type inference.
--   It is equivalent to the following:
--
-- @
-- instance Weaken (And l r) r
-- @
andRight :: Refined (And l r) x -> Refined r x
andRight = coerce

-- | This function helps type inference.
--   It is equivalent to the following:
--
-- @
-- instance Weaken l (Or l r)
-- @
leftOr :: Refined l x -> Refined (Or l r) x
leftOr = coerce

-- | This function helps type inference.
--   It is equivalent to the following:
--
-- @
-- instance Weaken r (Or l r)
-- @
rightOr :: Refined r x -> Refined (Or l r) x
rightOr = coerce

--------------------------------------------------------------------------------

-- | An exception encoding the way in which a 'Predicate' failed.
data RefineException
  = -- | A 'RefineException' for failures involving the 'Not' predicate.
    RefineNotException
    { _RefineException_typeRep   :: !TypeRep
      -- ^ The 'TypeRep' of the @'Not' p@ type.
    , _RefineException_notChild  :: !RefineException
      -- ^ The 'RefineException' for the @p@ failure.
    }

  | -- | A 'RefineException' for failures involving the 'And' predicate.
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
    RefineOrException
    { _RefineException_typeRep   :: !TypeRep
      -- ^ The 'TypeRep' of the @'Or' l r@ type.
    , _RefineException_orLChild  :: !RefineException
      -- ^ The 'RefineException' for the @l@ failure.
    , _RefineException_orRChild  :: !RefineException
      -- ^ The 'RefineException' for the @l@ failure.
    }

  | -- | A 'RefineException' for failures involving all other predicates.
    RefineOtherException
    { _RefineException_typeRep   :: !TypeRep
      -- ^ The 'TypeRep' of the predicate that failed.
    , _RefineException_message  :: !(PP.Doc Void)
      -- ^ A custom message to display.
    }
  deriving (Generic)

instance Show RefineException where
  show = PP.pretty .> show

twoSpaces, newline :: PP.Doc ann
{-# INLINE twoSpaces #-}
{-# INLINE newline   #-}
twoSpaces = "  "
newline = "\n"

-- | Display a 'RefineException' as a @'PP.Doc' ann@
displayRefineException :: RefineException -> PP.Doc ann
displayRefineException = \case
  RefineOtherException tr msg ->
    [ "The predicate ("
    , PP.pretty (show tr)
    , ") does not hold: "
    , newline
    , twoSpaces
    , PP.pretty (show msg)
    ] |> mconcat
  RefineNotException tr notChild ->
    [ "The negation of the predicate ("
    , PP.pretty (show tr)
    , ") does not hold:"
    , newline
    , twoSpaces
    , displayRefineException notChild
    , newline
    ] |> mconcat
  RefineOrException tr orLChild orRChild ->
    [ "Both subpredicates failed in: ("
    , PP.pretty (show tr)
    , "):"
    , newline
    , twoSpaces
    , displayRefineException orLChild
    , newline
    , twoSpaces
    , displayRefineException orRChild
    , newline
    , twoSpaces
    ] |> mconcat
  RefineAndException tr andChild ->
    (
      [ "The predicate ("
      , PP.pretty (show tr)
      , ") does not hold:"
      , newline
      , twoSpaces
      ] |> mconcat
    )
    <> case andChild of
         This a -> mconcat [ "The left subpredicate does not hold:", newline, twoSpaces, displayRefineException a, newline ]
         That b -> mconcat [ "The right subpredicate does not hold:", newline, twoSpaces, displayRefineException b, newline ]
         These a b -> mconcat [ twoSpaces, "Neither subpredicate holds: ", newline
                              , twoSpaces, displayRefineException a, newline
                              , twoSpaces, displayRefineException b, newline
                              ]

-- | Pretty-print a 'RefineException'.
instance PP.Pretty RefineException where
  pretty = displayRefineException

-- | Encode a 'RefineException' for use with \Control.Exception\.
instance Exception RefineException where
  displayException = show

--------------------------------------------------------------------------------

-- | A monad transformer that adds @'RefineException'@s to other monads.
--   
--   The @'pure'@ and @'Control.Monad.return'@ functions yield computations that produce
--   the given value, while @'>>='@ sequences two subcomputations, exiting
--   on the first @'RefineException'@.
newtype RefineT m a
  = RefineT (ExceptT RefineException m a)
  deriving ( Functor, Applicative, Monad, MonadFix
           , MonadError RefineException, MonadTrans
           , Generic, Generic1
           )

-- | The inverse of @'RefineT'@.
runRefineT
  :: RefineT m a
  -> m (Either RefineException a)
runRefineT = coerce .> ExceptT.runExceptT

-- | Map the unwrapped computation using the given function.
--
--   @'runRefineT' ('mapRefineT' f m) = f ('runRefineT' m)@
mapRefineT
  :: (m (Either RefineException a) -> n (Either RefineException b))
  -> RefineT m a
  -> RefineT n b
mapRefineT f = coerce .> ExceptT.mapExceptT f .> coerce

--------------------------------------------------------------------------------

-- | @'RefineM' a@ is equivalent to @'RefineT' 'Identity' a@ for any type @a@.
type RefineM a = RefineT Identity a

-- | Constructs a computation in the 'RefineM' monad. (The inverse of @'runRefineM'@).
refineM
  :: Either RefineException a
  -> RefineM a
refineM = ExceptT.except .> coerce

-- | Run a monadic action of type @'RefineM' a@,
--   yielding an @'Either' 'RefineException' a@.
--
--   This is just defined as @'runIdentity' '.' 'runRefineT'@.
runRefineM
  :: RefineM a
  -> Either RefineException a
runRefineM = runRefineT .> runIdentity

--------------------------------------------------------------------------------

-- | One can use @'throwRefine'@ inside of a monadic
--   context to begin processing a @'RefineException'@.
throwRefine
  :: (Monad m)
  => RefineException
  -> RefineT m a
throwRefine = MonadError.throwError

-- | A handler function to handle previous @'RefineException'@s
--   and return to normal execution. A common idiom is:
--
--   @ do { action1; action2; action3 } `'catchRefine'` handler @
--
--   where the action functions can call @'throwRefine'@. Note that
--   handler and the do-block must have the same return type.
catchRefine
  :: (Monad m)
  => RefineT m a
  -> (RefineException -> RefineT m a)
  -> RefineT m a
catchRefine = MonadError.catchError

-- | A handler for a @'RefineException'@.
--   
--   'throwRefineOtherException' is useful for defining what
--   behaviour 'validate' should have in the event of a predicate failure.
throwRefineOtherException
  :: (Monad m)
  => TypeRep
  -- ^ The 'TypeRep' of the 'Predicate'. This can usually be given by using 'typeOf'.
  -> PP.Doc Void
  -- ^ A 'PP.Doc' 'Void' encoding a custom error message to be pretty-printed. 
  -> RefineT m a
throwRefineOtherException rep
  = RefineOtherException rep .> throwRefine

--------------------------------------------------------------------------------
