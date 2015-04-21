module Refined
(
  Refined,
  refine,
  refineTH,
  unrefine,
  -- * Refinement
  Refinement(..),
  -- * Standard Refinement Rules
  -- ** Logical
  Not,
  And,
  Or,
  -- ** Numeric
  NonZero,
  Positive,
  Negative,
  NonNegative,
  NonPositive,
)
where

import BasePrelude
import qualified Language.Haskell.TH.Syntax as TH


-- |
-- A value of type @x@ refined according to a rule @r@.
newtype Refined r x =
  Refined x
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance TH.Lift x => TH.Lift (Refined r x) where
  lift (Refined a) =
    [|Refined a|]

-- |
-- A smart constructor of a Refined value.
-- Checks the input value at runtime.
{-# INLINABLE refine #-}
refine :: forall r x. Refinement r x => x -> Either String (Refined r x)
refine x =
  maybe (Right (Refined x)) Left $
  validate (undefined :: r) x

-- |
-- Constructs a Refined value with checking at compile-time using Template Haskell.
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
refineTH :: forall r x. (Refinement r x, TH.Lift x) => x -> TH.Q (TH.TExp (Refined r x))
refineTH =
  fmap TH.TExp . either fail TH.lift . (refine :: x -> Either String (Refined r x))

-- |
-- Extracts the refined value.
{-# INLINE unrefine #-}
unrefine :: Refined r x -> x
unrefine =
  unsafeCoerce
  

-- * Refinement
-------------------------

-- |
-- A refinement of type @x@ according to rule @r@.
class Refinement r x where
  -- |
  -- Check the value @x@ according to the refinement rule @r@,
  -- producing an error string if the value does not satisfy.
  validate :: r -> x -> Maybe String


-- * Rules
-------------------------


-- ** Logical
-------------------------

-- |
-- A logical negation of a refinement rule.
data Not r

instance Refinement r x => Refinement (Not r) x where
  validate _ =
    maybe (Just "A subrule didn't fail") (const Nothing) .
    validate (undefined :: r)

-- |
-- A logical conjunction refinement rule, composed of two other rules.
data And l r

instance (Refinement l x, Refinement r x) => Refinement (And l r) x where
  validate _ x =
    fmap (showString "The left subrule failed with: ") 
         (validate (undefined :: l) x) 
      <|>
    fmap (showString "The right subrule failed with: ") 
         (validate (undefined :: r) x)

-- |
-- A logical disjunction refinement rule, composed of two other rules.
data Or l r

instance (Refinement l x, Refinement r x) => Refinement (Or l r) x where
  validate _ x =
    case (validate (undefined :: l) x, validate (undefined :: r) x) of
      (Just a, Just b) -> 
        Just $ "Both subrules failed. First with: " <> a <> ". Second with: " <> b <> "."
      _ -> 
        Nothing


-- ** Numeric
-------------------------

-- |
-- A refinement rule, which ensures that the value does not equal to zero.
data NonZero

instance (Num x, Eq x) => Refinement NonZero x where
  validate _ =
    \case
      0 -> Just "A zero value"
      _ -> Nothing

-- |
-- A refinement rule, which ensures that the value is greater than zero.
data Positive

instance (Ord x, Num x) => Refinement Positive x where
  validate _ =
    \case
      x | x > 0 -> Nothing
      _ -> Just "A non-positive value"

-- |
-- A refinement rule, which ensures that the value is less than zero.
type Negative = 
  And (Not Positive) NonZero

-- |
-- A refinement rule, which ensures that the value is greater than or equal to zero.
type NonNegative =
  Not Negative

-- |
-- A refinement rule, which ensures that the value is less than or equal to zero.
type NonPositive = 
  Not Positive

