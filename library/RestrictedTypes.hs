module RestrictedTypes
(
  Restricted,
  pack,
  unpack,
  packTH,
  -- * Restriction
  Restriction(..),
  -- * Standard Restrictions
  Positive,
)
where

import BasePrelude
import qualified Language.Haskell.TH.Syntax as TH


newtype Restricted (r :: * -> *) x =
  Restricted x
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance TH.Lift x => TH.Lift (Restricted r x) where
  lift (Restricted a) =
    [|Restricted a|]

-- |
-- A smart constructor of a Restricted value.
-- Checks the input value at runtime.
{-# INLINABLE pack #-}
pack :: forall r x. Restriction r x => x -> Either String (Restricted r x)
pack =
  fmap Restricted . runRestriction (undefined :: r x)

-- |
-- Extracts the packed value.
{-# INLINE unpack #-}
unpack :: Restricted r x -> x
unpack =
  unsafeCoerce

-- |
-- Constructs a Restricted value with checking at compile-time using Template Haskell.
-- E.g.,
-- 
-- >>> $$(packTH 23) :: Restricted Positive Int
-- Restricted 23
-- 
-- Here's an example of an ivalid value:
-- 
-- >>> $$(packTH 0) :: Restricted Positive Int
-- <interactive>:18:4:
--     Non positive value
--     In the Template Haskell splice $$(packTH 0)
--     In the expression: $$(packTH 0) :: Restricted Positive Int
--     In an equation for ‘it’: it = $$(packTH 0) :: Restricted Positive Int
packTH :: forall r x. (Restriction r x, TH.Lift x) => x -> TH.Q (TH.TExp (Restricted r x))
packTH =
  fmap TH.TExp . either fail TH.lift . 
  fmap Restricted . runRestriction (undefined :: r x)
  

-- * Restriction
-------------------------

class Restriction r x where
  runRestriction :: r x -> x -> Either String x


-- |
-- A restriction rule, which ensures that the value is greater than zero.
-- 
-- Imposes an 'Ord' and a 'Num' constraint on the value.
data Positive n

instance (Ord n, Num n) => Restriction Positive n where
  runRestriction _ =
    \case
      n | n > 0 -> Right n
      _ -> Left "Non positive value"

  