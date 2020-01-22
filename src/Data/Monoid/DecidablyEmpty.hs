{-# LANGUAGE DefaultSignatures #-}

-- TODO upstream somwhere else?
module Data.Monoid.DecidablyEmpty where

import Data.Proxy

class Monoid a => DecidablyEmpty a where
  isEmpty :: a -> Bool
  default isEmpty :: Eq a => a -> Bool
  isEmpty = (==) mempty

instance DecidablyEmpty (Proxy a) where
  isEmpty ~Proxy = True
