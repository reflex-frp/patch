-- TODO upstream somwhere else?
module Data.Monoid.DecidablyEmpty where

class Monoid a => DecidablyEmpty a where
  isEmpty :: a -> Bool
