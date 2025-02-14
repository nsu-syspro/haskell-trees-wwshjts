{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (compare, foldl, foldr, Ordering(..))

import Task1 (Tree(..))

-----------------------------------
--
-- Computes sum from 1 to n
--
-- Usage example:
--
-- >>> sumtorial 5
-- 16

type Map k v = Tree (k, v)

listToMap :: Ord k => [(k, v)] -> Map k v
listToMap = error "TODO: define listToMap"

mapToList :: Map k v -> [(k, v)]
mapToList = error "TODO: define mapToList"

mlookup :: Ord k => k -> Map k v -> Maybe v
mlookup = error "TODO: define mlookup"

minsert :: Ord k => k -> v -> Map k v -> Map k v
minsert = error "TODO: define minsert"

mdelete :: Ord k => k -> Map k v -> Map k v
mdelete = error "TODO: define mdelete"
