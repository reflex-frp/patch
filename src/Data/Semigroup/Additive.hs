{-# LANGUAGE ConstraintKinds #-}

{-|
Description : A deprecated module containing a deprecated alias to the class for commutative semigroups
-}
module Data.Semigroup.Additive
  {-# DEPRECATED "Use 'Data.Semigroup.Commutative'" #-}
  ( Additive
  ) where

import Data.Semigroup.Commutative

{-# DEPRECATED Additive "Use 'Data.Semigroup.Commutative.Commutative'" #-}
-- | Deprecated alias for 'Commutative'
type Additive = Commutative
