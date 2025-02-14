{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (compare, foldl, foldr, Ordering(..))

import Task1 (Tree(..))

data Ordering = LT | EQ | GT
  deriving Show

type Cmp a = a -> a -> Ordering

compare :: Ord a => Cmp a
compare = error "TODO: define compare"

listToBST :: Cmp a -> [a] -> Tree a
listToBST = error "TODO: define listToBST"

bstToList :: Tree a -> [a]
bstToList = error "TODO: define bstToList"

isBST :: Cmp a -> Tree a -> Bool
isBST = error "TODO: define isBST"

tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup = error "TODO: define tlookup"

tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert = error "TODO: define tinsert"

tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete = error "TODO: define tdelete"
