module Task1Suite where

import Test.Tasty hiding (TestTree)
import qualified Test.Tasty as T
import Test.Tasty.QuickCheck

import Task1 (forder, Forest, Tree(..), torder, Order(..))

import Data.Maybe (maybeToList)
import Data.List (intercalate)


task1Tests :: T.TestTree
task1Tests = testGroup "Task1"
  [ testProperty "torder PreOrder" $
      withMaxSuccess 100 $ counterexample "unexpected pre-order for" $
        \(def, TestTree tree) -> 
          let o = PreOrder
          in torder o def (tree :: Tree Int) === case tree of
                Leaf -> maybeToList def
                Branch v l r -> [v] ++ torder o def l ++ torder o def r

  , testProperty "torder InOrder" $
      withMaxSuccess 100 $ counterexample "unexpected in-order for" $
        \(def, TestTree tree) -> 
          let o = InOrder
          in torder o def (tree :: Tree Int) === case tree of
                Leaf -> maybeToList def
                Branch v l r -> torder o def l ++ [v] ++ torder o def r

  , testProperty "torder PostOrder" $
      withMaxSuccess 100 $ counterexample "unexpected post-order for" $
        \(def, TestTree tree) -> 
          let o = PostOrder
          in torder o def (tree :: Tree Int) === case tree of
                Leaf -> maybeToList def
                Branch v l r -> torder o def l ++ torder o def r ++ [v]

  , testProperty "forder" $
      withMaxSuccess 100 $ counterexample "unexpected forder for" $
        \(TestOrder order, sep, def, testForest) -> 
          let forest = map unTestTree testForest
          in forder order sep def (forest :: Forest Int) ===
              intercalate (maybeToList sep) (map (torder order def) forest)
  ]

newtype TestOrder = TestOrder { unTestOrder :: Order }

instance Show TestOrder where
  show (TestOrder x) = show x

instance Arbitrary TestOrder where
  arbitrary = elements $ map TestOrder [PreOrder, InOrder, PostOrder]

newtype TestTree a = TestTree { unTestTree :: Tree a }

instance Show a => Show (TestTree a) where
  show (TestTree x) = show x

type TestForest a = [TestTree a]

instance Arbitrary a => Arbitrary (TestTree a) where
  arbitrary = sized arbitrarySizedTree
  shrink (TestTree Leaf) = []
  shrink (TestTree (Branch x l r)) = map TestTree $
    -- shrink Branch to Leaf
    [Leaf] ++
    -- shrink to subterms
    [l, r] ++
    -- recursively shrink subterms
    [Branch x' l' r' | (x', TestTree l', TestTree r') <- shrink (x, TestTree l, TestTree r)]

arbitrarySizedTree :: Arbitrary a => Int -> Gen (TestTree a)
arbitrarySizedTree 0 = pure $ TestTree Leaf
arbitrarySizedTree m = do
  v <- arbitrary
  TestTree l <- arbitrarySizedTree (m `div` 2)
  TestTree r <- arbitrarySizedTree (m `div` 2)
  pure $ TestTree (Branch v l r)
