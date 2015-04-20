module RestrictedTypes
(
  Restricted,
  pack,
  unpack,
  -- * Restrictions
  Positive,
)
where

import BasePrelude
import qualified Language.Haskell.TH.Syntax as TH


newtype Restricted (a :: * -> *) b =
  Restricted b
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic, TH.Lift)

{-# INLINABLE pack #-}
pack :: forall a b. Restriction a b => b -> Either String (Restricted a b)
pack =
  fmap Restricted . restrictionFn (undefined :: a b)

{-# INLINE unpack #-}
unpack :: Restricted a b -> b
unpack =
  unsafeCoerce


class Restriction a b where
  restrictionFn :: a b -> b -> Either String b


-- * Restrictions
-------------------------

-- |
-- A restriction rule, which ensures that the value is greater than zero.
-- 
-- Imposes an 'Ord' and a 'Num' constraint on the value.
data Positive n

instance (Ord n, Num n) => Restriction Positive n where
  restrictionFn _ =
    \case
      n | n > 0 -> Right n
      _ -> Left "Not positive"

  