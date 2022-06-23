{-# LANGUAGE TypeOperators #-}
-- TODO upstream somwhere else?
module Control.Category.DecidablyEmpty where

import Control.Category
import Data.Type.Equality

class Category c => DecidablyEmpty c where
  isId :: c a b -> Maybe (a :~: b)
