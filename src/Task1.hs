{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (foldl, foldr)

data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving Show

data Order = PreOrder | InOrder | PostOrder
  deriving Show

torder :: Order -> Maybe a -> Tree a -> [a]
torder = error "TODO: define torder"

type Forest a = [Tree a]

forder :: Order -> Maybe a -> Maybe a -> Forest a -> [a]
forder = error "TODO: define forder"

