module Refined
(
  Refined,
  refine,
  refineTH,
  unrefine,
  -- * Predicate Interface
  Predicate(..),
  -- * Standard Predicates
  -- ** Logical
  Not,
  And,
  Or,
  -- ** Numeric
  LessThan,
  GreaterThan,
  From,
  To,
  FromTo,
  EqualTo,
  Positive,
  NonPositive,
  Negative,
  NonNegative,
  ZeroToOne,
)
where

import BasePrelude
import GHC.TypeLits
import qualified Language.Haskell.TH.Syntax as TH


-- |
-- A refinement type, 
-- which wraps a value of type @x@,
-- ensuring that it satisfies a type-level predicate @p@.
newtype Refined p x =
  Refined x
  deriving (Show, Eq, Ord, Typeable, Data, Generic)

instance (Read x, Predicate p x) => Read (Refined p x) where
  readsPrec d =
    readParen (d > 10) $ \r1 -> do
      ("Refined", r2) <- lex r1
      (raw,       r3) <- readsPrec 11 r2
      case refine raw of
        Right val -> [(val, r3)]
        Left  _   -> []

instance TH.Lift x => TH.Lift (Refined p x) where
  lift (Refined a) =
    [|Refined a|]

-- |
-- A smart constructor of a 'Refined' value.
-- Checks the input value at runtime.
{-# INLINABLE refine #-}
refine :: forall p x. Predicate p x => x -> Either String (Refined p x)
refine x =
  maybe (Right (Refined x)) Left $
  validate (undefined :: p) x

-- |
-- Constructs a 'Refined' value with checking at compile-time using Template Haskell.
-- E.g.,
-- 
-- >>> $$(refineTH 23) :: Refined Positive Int
-- Refined 23
-- 
-- Here's an example of an invalid value:
-- 
-- >>> $$(refineTH 0) :: Refined Positive Int
-- <interactive>:6:4:
--     Value is not greater than 0
--     In the Template Haskell splice $$(refineTH 0)
--     In the expression: $$(refineTH 0) :: Refined Positive Int
--     In an equation for ‘it’:
--         it = $$(refineTH 0) :: Refined Positive Int
-- 
-- If it's not evident, the example above indicates a compile-time failure, 
-- which means that the checking was done at compile-time, 
-- thus introducing a zero runtime overhead compared to a plain value construction.
refineTH :: forall p x. (Predicate p x, TH.Lift x) => x -> TH.Q (TH.TExp (Refined p x))
refineTH =
  fmap TH.TExp . either fail TH.lift . (refine :: x -> Either String (Refined p x))

-- |
-- Extracts the refined value.
{-# INLINE unrefine #-}
unrefine :: Refined p x -> x
unrefine =
  unsafeCoerce
  

-- * Predicate
-------------------------

-- |
-- A class which defines a runtime interpretation of
-- a type-level predicate @p@ for type @x@.
class Predicate p x where
  -- |
  -- Check the value @x@ according to the predicate @p@,
  -- producing an error string if the value does not satisfy.
  validate :: p -> x -> Maybe String


-- * Rules
-------------------------


-- ** Logical
-------------------------

-- |
-- A logical negation of a predicate.
data Not r

instance Predicate r x => Predicate (Not r) x where
  validate _ =
    maybe (Just "A subpredicate didn't fail") (const Nothing) .
    validate (undefined :: r)

-- |
-- A logical conjunction predicate, composed of two other predicates.
data And l r

instance (Predicate l x, Predicate r x) => Predicate (And l r) x where
  validate _ x =
    fmap (showString "The left subpredicate failed with: ") 
         (validate (undefined :: l) x) 
      <|>
    fmap (showString "The right subpredicate failed with: ") 
         (validate (undefined :: r) x)

-- |
-- A logical disjunction predicate, composed of two other predicates.
data Or l r

instance (Predicate l x, Predicate r x) => Predicate (Or l r) x where
  validate _ x =
    case (validate (undefined :: l) x, validate (undefined :: r) x) of
      (Just a, Just b) -> 
        Just $ "Both subpredicates failed. First with: " <> a <> ". Second with: " <> b <> "."
      _ -> 
        Nothing


-- ** Numeric
-------------------------

-- |
-- A predicate, which ensures that a value is less than the specified type-level number.
data LessThan (n :: Nat)

instance (Ord x, Num x, KnownNat n) => Predicate (LessThan n) x where
  validate p x =
    if x < fromIntegral x'
      then Nothing
      else Just ("Value is not less than " <> show x')
    where
      x' = natVal p

-- |
-- A predicate, which ensures that a value is greater than the specified type-level number.
data GreaterThan (n :: Nat)

instance (Ord x, Num x, KnownNat n) => Predicate (GreaterThan n) x where
  validate p x =
    if x > fromIntegral x'
      then Nothing
      else Just ("Value is not greater than " <> show x')
    where
      x' = natVal p

-- |
-- A predicate, which ensures that a value is greater than or equal to the specified type-level number.
data From (n :: Nat)

instance (Ord x, Num x, KnownNat n) => Predicate (From n) x where
  validate p x =
    if x >= fromIntegral x'
      then Nothing
      else Just ("Value is less than " <> show x')
    where
      x' = natVal p

-- |
-- A predicate, which ensures that a value is less than or equal to the specified type-level number.
data To (n :: Nat)

instance (Ord x, Num x, KnownNat n) => Predicate (To n) x where
  validate p x =
    if x <= fromIntegral x'
      then Nothing
      else Just ("Value is greater than " <> show x')
    where
      x' = natVal p

-- |
-- A predicate, which ensures that a value is between or equal to either of the specified type-level numbers.
data FromTo (mn :: Nat) (mx :: Nat)

instance (Ord x, Num x, KnownNat mn, KnownNat mx, mn <= mx) => Predicate (FromTo mn mx) x where
  validate p x =
    if x >= fromIntegral mn' && x <= fromIntegral mx'
      then Nothing
      else Just ("Value is out of range (minimum: " <> show mn' <> ", maximum: " <> show mx' <> ")")
    where
      mn' = natVal (Proxy :: Proxy mn)
      mx' = natVal (Proxy :: Proxy mx)

-- |
-- A predicate, which ensures that a value equals to the specified type-level number.
data EqualTo (n :: Nat)

instance (Ord x, Num x, KnownNat n) => Predicate (EqualTo n) x where
  validate p x =
    if x == fromIntegral x'
      then Nothing
      else Just ("Value does not equal " <> show x')
    where
      x' = natVal p

-- |
-- A predicate, which ensures that the value is greater than zero.
type Positive =
  GreaterThan 0

-- |
-- A predicate, which ensures that the value is less than or equal to zero.
type NonPositive =
  To 0

-- |
-- A predicate, which ensures that the value is less than zero.
type Negative = 
  LessThan 0

-- |
-- A predicate, which ensures that the value is greater than or equal to zero.
type NonNegative =
  From 0

-- |
-- A range of values from zero to one, including both.
type ZeroToOne =
  FromTo 0 1
