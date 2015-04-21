module Refined
(
  Refined,
  pack,
  unpack,
  packTH,
  -- * Refinement
  Refinement(..),
  -- * Standard Refinements
  Not,
  And,
  Positive,
  NonZero,
  Negative,
  NonPositive,
  NonNegative,
)
where

import BasePrelude
import qualified Language.Haskell.TH.Syntax as TH


newtype Refined r x =
  Refined x
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance TH.Lift x => TH.Lift (Refined r x) where
  lift (Refined a) =
    [|Refined a|]

-- |
-- A smart constructor of a Refined value.
-- Checks the input value at runtime.
{-# INLINABLE pack #-}
pack :: forall r x. Refinement r x => x -> Either String (Refined r x)
pack x =
  maybe (Right (Refined x)) Left $
  runRefinement (undefined :: r) x

-- |
-- Extracts the packed value.
{-# INLINE unpack #-}
unpack :: Refined r x -> x
unpack =
  unsafeCoerce

-- |
-- Constructs a Refined value with checking at compile-time using Template Haskell.
-- E.g.,
-- 
-- >>> $$(packTH 23) :: Refined Positive Int
-- Refined 23
-- 
-- Here's an example of an ivalid value:
-- 
-- >>> $$(packTH 0) :: Refined Positive Int
-- <interactive>:18:4:
--     Non positive value
--     In the Template Haskell splice $$(packTH 0)
--     In the expression: $$(packTH 0) :: Refined Positive Int
--     In an equation for ‘it’: it = $$(packTH 0) :: Refined Positive Int
packTH :: forall r x. (Refinement r x, TH.Lift x) => x -> TH.Q (TH.TExp (Refined r x))
packTH =
  fmap TH.TExp . either fail TH.lift . (pack :: x -> Either String (Refined r x))
  

-- * Refinement
-------------------------

class Refinement r x where
  runRefinement :: r -> x -> Maybe String


data Not r

instance Refinement r x => Refinement (Not r) x where
  runRefinement _ =
    maybe (Just "A subrestriction didn't fail") (const Nothing) .
    runRefinement (undefined :: r)

data And l r

instance (Refinement l x, Refinement r x) => Refinement (And l r) x where
  runRefinement _ x =
    fmap (showString "The left subrestriction failed with: ") 
         (runRefinement (undefined :: l) x) 
      <|>
    fmap (showString "The right subrestriction failed with: ") 
         (runRefinement (undefined :: r) x)

-- |
-- A restriction rule, which ensures that the value is greater than zero.
-- 
-- Imposes an 'Ord' and a 'Num' constraint on the value.
data Positive

instance (Ord x, Num x) => Refinement Positive x where
  runRefinement _ =
    \case
      x | x > 0 -> Nothing
      _ -> Just "A non-positive value"

data NonZero

instance (Num x, Eq x) => Refinement NonZero x where
  runRefinement _ =
    \case
      0 -> Just "A zero value"
      _ -> Nothing

type Negative = 
  And (Not Positive) NonZero

type NonPositive = 
  Not Positive

type NonNegative =
  Not Negative

