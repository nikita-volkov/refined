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
  Zero,
  Positive,
  Negative,
)
where

import BasePrelude
import qualified Language.Haskell.TH.Syntax as TH


-- |
-- A refinement type, 
-- which wraps a value of type @x@,
-- ensuring that it satisfies a type-level predicate @p@.
newtype Refined p x =
  Refined x
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance TH.Lift x => TH.Lift (Refined p x) where
  lift (Refined a) =
    [|Refined a|]

-- |
-- A smart constructor of a Refined value.
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
-- Here's an example of an ivalid value:
-- 
-- >>> $$(refineTH 0) :: Refined Positive Int
-- <interactive>:18:4:
--     Non positive value
--     In the Template Haskell splice $$(refineTH 0)
--     In the expression: $$(refineTH 0) :: Refined Positive Int
--     In an equation for ‘it’: it = $$(refineTH 0) :: Refined Positive Int
-- 
-- If it's not evident, the above is a compile-time failure, 
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
-- A predicate, which ensures that the value is equal to zero.
-- By itself it's probably not very useful,
-- however it allows to define complex predicates using logical combinators.
-- E.g., see the definition of 'Negative'.
data Zero

instance (Num x, Eq x) => Predicate Zero x where
  validate _ =
    \case
      0 -> Nothing
      _ -> Just "A non-zero value"

-- |
-- A predicate, which ensures that the value is greater than zero.
data Positive

instance (Ord x, Num x) => Predicate Positive x where
  validate _ =
    \case
      x | x > 0 -> Nothing
      _ -> Just "A non-positive value"

-- |
-- A predicate, which ensures that the value is less than zero.
type Negative = 
  And (Not Positive) (Not Zero)
