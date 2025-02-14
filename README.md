# Haskell assignment base

<img alt="points bar" align="right" height="36" src="../../blob/badges/.github/badges/points-bar.svg" />

<details>
<summary>Guidelines</summary>

## Guidelines

When solving the homework, strive to create not just code that works, but code that is readable and concise.
Try to write small functions which perform just a single task, and then combine those smaller
pieces to create more complex functions.

Don’t repeat yourself: write one function for each logical task, and reuse functions as necessary.

Don't be afraid to introduce new functions where you see fit.

### Sources

Each task has corresponding source file in [src](src) directory where you should implement the solution.

### Building

All solutions should compile without warnings with following command:

```bash
stack build
```

### Testing

You can and should run automated tests before pushing solution to GitHub via

```bash
stack test --test-arguments "-p TaskX"
```

where `X` in `TaskX` should be number of corresponding Task to be tested.

So to run all test for the first task you should use following command:

```bash
stack test --test-arguments "-p Task1"
```

You can also run tests for all tasks with just

```bash
stack test
```

### Debugging

For debugging you should use GHCi via stack:

```bash
stack ghci
```

You can then load your solution for particular task using `:load TaskX` command.

Here is how to load Task1 in GHCi:

```bash
$ stack ghci
ghci> :load Task1
[1 of 1] Compiling Task1 ( .../src/Task1.hs, interpreted )
Ok, one module loaded.
```

> **Note:** if you updated solution, it can be quickly reloaded in the same GHCi session with `:reload` command
> ```bash
> ghci> :reload
> ```

</details>

## Preface

In this assignment you will implement some basic tree processing functionality
starting with traversal, binary search tree and then implementing tree-based map structure.
Each task is based on results from all previous ones, so they should be implemented in order.

## Task 1 (3 points)

### Tree traversal

The first task is to define functions for traversing tree and forest (list of trees).

There are [many ways](https://en.wikipedia.org/wiki/Tree_traversal) to traverse trees.
However, we will focus on the most common ones derived from depth-first search:

- Pre-order
- In-order
- Post-order

All of which are perfectly summarized in [Wikipedia](https://en.wikipedia.org/wiki/Tree_traversal#Depth-first_search):

[![](https://upload.wikimedia.org/wikipedia/commons/7/75/Sorted_binary_tree_ALL_RGB.svg)](https://en.wikipedia.org/wiki/Tree_traversal#Depth-first_search)

> Depth-first traversal (dotted path) of a binary tree:  
> Pre-order (node visited at position red ●):  
>     F, B, A, D, C, E, G, I, H;  
> In-order (node visited at position green ●):  
>     A, B, C, D, E, F, G, H, I;  
> Post-order (node visited at position blue ●):  
>     A, C, E, D, B, H, I, G, F.

### Task

In [src/Task1.hs](src/Task1.hs] you will find following type definitions of binary tree, forest and enumeration of selected orders:

```haskell
data Tree a = Leaf a | Branch a (Tree a) (Tree a)
  deriving Show

type Forest a = [Tree a]

data Order = PreOrder | InOrder | PostOrder
  deriving Show
```

Your goal is to implement following functions for tree and forest traversal:

- `torder` which returns values of given `Tree` in specified `Order` with optional leaf value
  ```haskell
  torder :: Order    -- ^ Order of resulting traversal
         -> Maybe a  -- ^ Optional leaf value
         -> Tree a   -- ^ Tree to traverse
         -> [a]      -- ^ List of values in specified order
  ```
  **Example:**
  ```haskell
  >>> torder PreOrder  (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
  "A.B.."
  >>> torder InOrder   (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
  ".A.B."
  >>> torder PostOrder (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
  "...BA"
  ```
- `forder` which returns values of given `Forest` separated by optional separator (first `Maybe` arg)
  where each tree is traversed in specified `Order` with optional leaf value (second `Maybe` arg)
  ```haskell
  forder :: Order     -- ^ Order of tree traversal
         -> Maybe a   -- ^ Optional separator between resulting tree orders
         -> Maybe a   -- ^ Optional leaf value
         -> Forest a  -- ^ List of trees to traverse
         -> [a]       -- ^ List of values in specified tree order
  ```
  **Example:**
  ```haskell
  >>> forder PreOrder  (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
  ".|C..|A.B.."
  >>> forder InOrder   (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
  ".|.C.|.A.B."
  >>> forder PostOrder (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
  ".|..C|...BA"
  ```

> [!TIP]
>
> For this task you might want to implement and use functions
> [maybeToList](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Maybe.html#v:maybeToList) and
> [intercalate](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:intercalate).
>
> Feel free to check out their documentation, but please try to implement them yourself without looking into library source code.

## Task 2 (4 points)

Implement "sumtorial" of n.

$$
f(n) =\begin{cases}
1& \text{if } n = 0\\
\sum_{i=1}^n{i}& \text{otherwise}
\end{cases}
$$

```haskell
sumtorial :: Integer
```
**Example:**
```haskell
>>> sumtorial 0
1
>>> sumtorial 5
16
```
