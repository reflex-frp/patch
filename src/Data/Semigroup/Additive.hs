{-# LANGUAGE ConstraintKinds #-}

{-|
Description : A class for commutative semigroups
-}
module Data.Semigroup.Additive
  {-# DEPRECATED "Use 'Data.Semigroup.Commutative'" #-}
  ( Additive
  ) where

import Data.Semigroup.Commutative

{-# DEPRECATED Additive "Use 'Data.Semigroup.Commutative.Commutative'" #-}
type Additive = Commutative
