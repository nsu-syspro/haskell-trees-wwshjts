{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (compare, foldl, foldr, Ordering(..))

import Task1 (Tree(..))

-- * Type definitions

-- | Ordering enumeration
data Ordering = LT | EQ | GT
  deriving Show

-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
--
-- Usage example:
--
-- >>> compare 2 3
-- LT
-- >>> compare 'a' 'a'
-- EQ
-- >>> compare "Haskell" "C++"
-- GT
--
compare :: Ord a => Cmp a
compare l r
    | l < r  = LT
    | l == r = EQ
    | l > r  = GT
    | otherwise = error "Something bad happened in compare"

-- | Conversion of list to binary search tree
-- using given comparison function
--
-- Usage example:
--
-- >>> listToBST compare [2,3,1]
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> listToBST compare ""
-- Leaf
--
listToBST :: Cmp a -> [a] -> Tree a
listToBST = sListToBst 

-- | Conversion from binary search tree to list
--
-- Resulting list will be sorted
-- if given tree is valid BST with respect
-- to some 'Cmp' comparison.
--
-- Usage example:
--
-- >>> bstToList (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- [1,2,3]
-- >>> bstToList Leaf
-- []
--
bstToList :: Tree a -> [a]
bstToList = error "TODO: define bstToList"

-- | Tests whether given tree is a valid binary search tree
-- with respect to given comparison function
--
-- Usage example:
--
-- >>> isBST compare (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- True
-- >>> isBST compare (Leaf :: Tree Char)
-- True
-- >>> isBST compare (Branch 5 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- False
--
isBST :: Cmp a -> Tree a -> Bool
isBST = error "TODO: define isBST"

-- | Searches given binary search tree for
-- given value with respect to given comparison
--
-- Returns found value (might not be the one that was given)
-- wrapped into 'Just' if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> tlookup compare 2 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Just 2
-- >>> tlookup compare 'a' Leaf
-- Nothing
-- >>> tlookup (\x y -> compare (x `mod` 3) (y `mod` 3)) 5 (Branch 2 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf))
-- Just 2
--
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup = error "TODO: define tlookup"

-- | Inserts given value into given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- If the same value with respect to comparison
-- was already present in the 'Tree' then replaces it with given value.
--
-- Usage example:
--
-- >>> tinsert compare 0 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 (Branch 0 Leaf Leaf) Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 'a' Leaf
-- Branch 'a' Leaf Leaf
--
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert _   e Leaf = Branch e Leaf Leaf
tinsert cmp e (Branch val l r) =
    case cmp e val of
        LT -> Branch val (tinsert cmp e l) r
        EQ -> Branch val (tinsert cmp e l) r
        GT -> Branch val l (tinsert cmp e r)


-- | Deletes given value from given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- Returns updated 'Tree' if the value was present in it;
-- or unchanged 'Tree' otherwise.
--
-- Usage example:
--
-- >>> tdelete compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 Leaf (Branch 3 Leaf Leaf)
-- >>> tdelete compare 'a' Leaf
-- Leaf
--
tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete = error "TODO: define tdelete"


-- | Bulding balanced BST using sorted list
-- O(n*logn)

sListToBst :: Cmp a -> [a] -> Tree a
sListToBst cmp xs = sListToBstImpl cmp (mSort cmp xs)

sListToBstImpl :: Cmp a -> [a] -> Tree a
sListToBstImpl _   []  = Leaf
sListToBstImpl _   [x] = Branch x Leaf Leaf 
sListToBstImpl cmp xs  
    = Branch central (sListToBstImpl cmp (init (fst splitted))) (sListToBstImpl cmp (snd splitted))
        where 
            central   = takeCentral xs
            splitted  = splitByHalf xs 


-- | Merge sort
mSort :: Cmp a -> [a] -> [a]
mSort _   []  = []
mSort _   [x] = [x]
mSort cmp xs  = merge cmp (mSortBoth cmp (splitByHalf xs))
    where mSortBoth cmpN (l, r) = (mSort cmpN l, mSort cmpN r)

merge :: Cmp a -> ([a], [a]) -> [a]
merge cmp (l, r) =  mergeImpl cmp l r

mergeImpl :: Cmp a -> [a] -> [a] -> [a]
mergeImpl _   xs [] = xs
mergeImpl _   [] ys = ys
mergeImpl cmp (x : xs) (y : ys) = 
    case cmp x y of
        LT -> x : mergeImpl cmp xs (y : ys)
        EQ -> x : mergeImpl cmp xs (y : ys)
        GT -> y : mergeImpl cmp (x : xs) ys
-- 

takeCentral :: [a] -> a 
takeCentral xs = last (fst (splitByHalf xs))

splitByHalf :: [a] -> ([a], [a])
splitByHalf xs = splitBy half xs
    where half = (length xs `div` 2) + (length xs `mod` 2)

splitBy :: Int -> [a] -> ([a], [a])
splitBy n xs
    | n <= 0 || null xs = undefined 
    | otherwise         = splitByImpl n ([], xs)


splitByImpl :: Int -> ([a], [a]) -> ([a], [a])
splitByImpl 0 res          = res
splitByImpl _ (ls, [])     = (ls, [])
splitByImpl n (ls, r : rs) = splitByImpl (n - 1) (ls ++ [r], rs)

