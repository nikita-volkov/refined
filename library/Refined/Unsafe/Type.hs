-- | This module exports the 'Refined' type with its
-- constructor. This is very risky! In particular, the 'Coercible'
-- instances will be visible throughout the importing module.
-- It is usually better to build the necessary coercions locally
-- using the utilities in "Refined.Unsafe", but in some cases
-- it may be more convenient to write a separate module that
-- imports this one and exports some large coercion.
module Refined.Unsafe.Type (Refined (..)) where

import Refined.Internal
