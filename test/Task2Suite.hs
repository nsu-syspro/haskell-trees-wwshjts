module Task2Suite where

import Test.Tasty hiding (TestTree)
import qualified Test.Tasty as T
import Test.Tasty.QuickCheck

import Prelude hiding (compare, Ordering(..))

import Task1
import Task2

import Task1Suite

import Data.List (sort, nub, delete)


task2Tests :: T.TestTree
task2Tests = testGroup "Task2"
  [ testProperty "bstToList tree == sort (bstToList tree)" $
      withMaxSuccess 100 $ counterexample "unexpected result for" $
        \(TestBST tree) ->
          bstToList (tree :: Tree Int) === sort (bstToList tree)

  , testProperty "bstToList (listToBST list) == sort unique list" $
      withMaxSuccess 100 $ counterexample "unexpected result for" $
        \xs ->
          bstToList (listToBST compare (xs :: [Int])) === sort (nub xs)

  , testProperty "isBST tree == isUniqueSorted (bstToList tree)" $
      withMaxSuccess 1000 $ counterexample "unexpected result for" $
        \(TestTree tree) ->
          classify (isUniqueSorted (bstToList tree)) "is BST" $
            isBST compare (tree :: Tree Int) === isUniqueSorted (bstToList tree)

  , testProperty "tlookup" $
      withMaxSuccess 1000 $ counterexample "unexpected tlookup result of" $
        \(x, TestBST tree) ->
          classify (x `elem` bstToList tree) "contains" $
            tlookup compare x (tree :: Tree Int) ===
              if x `elem` bstToList tree
              then Just x
              else Nothing

  , testProperty "tlookup returns found value (not the one that was searched for)" $
      withMaxSuccess 100 $
        \(Blind x) ->
          counterexample ("unexpected result of `tlookup (cmpMod3) " ++ show x ++ " (" ++ show bstMod3 ++ ")`") $
            tlookup (cmpModN 3) x bstMod3 === Just (x `mod` 3)

  , testProperty "tinsert updates value if exists" $
      withMaxSuccess 100 $
        \(Blind x) ->
          counterexample ("unexpected result of `bstToList (tinsert (cmpMod3) " ++ show x ++ " (" ++ show bstMod3 ++ ")`") $
            bstToList (tinsert (cmpModN 3) x bstMod3) === map (\y -> if x `mod` 3 == y then x else y) [0,1,2]

  , testProperty "bstToList (tinsert x tree) == sort unique (x : bstToList tree)" $
      withMaxSuccess 1000 $ counterexample "unexpected result for" $
        \(x, TestBST tree) ->
          classify (x `elem` bstToList tree) "contains" $
            bstToList (tinsert compare x (tree :: Tree Int)) === sort (nub (x : bstToList tree))

  , testProperty "bstToList (tdelete x tree) == delete x (bstToList tree)" $
      withMaxSuccess 1000 $ counterexample "unexpected result for" $
        \(x, TestBST tree) ->
          classify (x `elem` bstToList tree) "contains" $
            bstToList (tdelete compare x (tree :: Tree Int)) === delete x (bstToList tree)
  ]

cmpModN :: Integral a => a -> Cmp a
cmpModN n x y = compare (x `mod` n) (y `mod` n)

bstMod3 :: Tree Int
bstMod3 = Branch 1 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf)

isUniqueSorted :: Ord a => [a] -> Bool
isUniqueSorted xs = xs == sort (nub xs)

newtype TestBST a = TestBST { unTestBST :: Tree a }

instance Show a => Show (TestBST a) where
  show (TestBST x) = show x

instance (Bounded a, Enum a, Ord a, Arbitrary a) => Arbitrary (TestBST a) where
  arbitrary = sized (arbitrarySizedBST Nothing)
  shrink = map (TestBST . unTestTree) . shrink . TestTree . unTestBST

arbitrarySizedBST :: (Bounded a, Enum a, Ord a, Arbitrary a) => Maybe (a, a) -> Int -> Gen (TestBST a)
arbitrarySizedBST _ 0 = pure $ TestBST Leaf
arbitrarySizedBST (Just (l, r)) _ | l > r = pure $ TestBST Leaf
arbitrarySizedBST bounds m = do
  v <- maybe arbitrary chooseEnum bounds
  let lbounds = case bounds of
        Nothing -> (minBound, pred v)
        Just (l, _) -> (l, pred v)
  let rbounds = case bounds of
        Nothing -> (succ v, maxBound)
        Just (_, r) -> (succ v, r)
  TestBST l <- arbitrarySizedBST (Just lbounds) (m `div` 2)
  TestBST r <- arbitrarySizedBST (Just rbounds) (m `div` 2)
  pure $ TestBST (Branch v l r)
