{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Brick.Widgets.Tree
    ( Forest(..)
    , forestName
    , forestTrees
    , forestSelected
    , Tree(..)
    , treeElem
    , treeChildren
    -- , nodeCollapsed
    , forest
    -- , renderTree
    -- , treeCollapse
    ) where

import Data.List.Zipper

import Brick.Types
import Brick.Widgets.Core
import Control.Lens hiding ((|>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Sequence      (Seq, (|>))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence      as Seq

type IsSelected = Bool
type IsCollapsed = Bool

-- | A single tree of a forest, which has 0 or more children trees and may be
-- collapsed.
data Tree a = Tree
    { _treeElem      :: a
    , _treeChildren  :: [Tree a]
    , _treeCollapsed :: Bool
    }
makeLenses ''Tree

-- | A forest of trees with a single selected tree if non-empty.
data Forest a = Forest
    { _forestName     :: Name
    , _forestTrees    :: [Tree a]
    , _forestSelected :: Maybe (Z (Seq Int))
    }
makeLenses ''Forest

-- | Construct a 'Forest, given a forest of 'Tree's.
forest :: Name -> [Tree a] -> Forest a
forest name trees = Forest name trees (zipper (dfForest mempty trees))
  where
    -- Depth-first traversal of a tree, given the tree, and the path
    -- taken thus far. Does not traverse past collapsed nodes, but does
    -- include them.
    --
    -- For example,
    --
    --                   o0
    --     dfTree []    / \
    --                 o1   o2
    --                /  \
    --               x0   o3
    --              / \
    --             o4  o5
    --
    --     == [[], [0], [0,0], [0,1], [1]]
    --         ^    ^     ^      ^     ^
    --         o0   o1    x0     o3    o4
    --
    -- where 'o' represents a non-collapsed tree, and
    --       'x' represents a collapsed tree
    --
    dfTree :: Seq Int -> Tree a -> [Seq Int]
    dfTree is (Tree _ _ True) = [is]
    dfTree is (Tree _ ns _) = is : dfForest is ns

    -- Like 'dfTree', but for a forest of trees.
    --
    -- For example,
    --
    --                        o0          o5
    --     dfForest [] [     / \         / \    ]
    --                      o1  o2      o6  o7
    --                     / \             /
    --                    o3  o4    ,     x0
    --                                   /
    --                                  o8
    --
    --     == [[0], [0,0], [0,0,0], [0,0,1], [0,1], [1], [1,0], [1,1], [1,1,0]]
    --          ^     ^       ^        ^       ^     ^     ^      ^       ^
    --          o0    o1      o3       o4      o2    o5    o6     o7      x0
    --
    -- where 'o' represents a non-collapsed tree, and
    --       'x' represents a collapsed tree
    --
    dfForest :: Seq Int -> [Tree a] -> [Seq Int]
    dfForest is ns = concatMap go (zip [0..] ns)
      where
        go :: (Int, Tree a) -> [Seq Int]
        go (i, n) = dfTree (is |> i) n

-- | Render a forest, given an element rendering function. Children of collapsed
-- elements will not be rendered.
-- renderTree :: Tree a -> (NodeSelected -> NodeCollapsed -> a -> Widget) -> Widget
-- renderTree (Tree name nodes sel) draw = renderNodes nodes sel draw
--
-- renderNodes
--     :: [Node a]
--     -> [Int]
--     -> (NodeSelected -> NodeCollapsed -> a -> Widget)
--     -> Widget
-- renderNodes ns idx draw =
--     vBox (map (\(n,i) -> renderNode n i draw) (zip ns (iterate idxRight idx)))
--
-- renderNode
--     :: Node a
--     -> [Int]
--     -> (NodeSelected -> NodeCollapsed -> a -> Widget)
--     -> Widget
-- renderNode (Node x xs collapsed) idx draw =
--     let w0 = draw (idx == [0]) collapsed x
--         w1 = renderNodes xs (idxDown idx) draw
--     in if collapsed
--            then w0
--            else w0 <=> padLeft (Pad 1) w1

-- | Collapse (or uncollapse) the selected node of a tree.
-- treeCollapse :: Tree a -> Tree a
-- treeCollapse t = t & treeNodes %~ nodesCollapse (t^.treeSelected)

-- nodesCollapse :: [Int] -> [Node a] -> [Node a]
-- nodesCollapse ns = over (nodeIndex ns . nodeCollapsed) not

-- | Move "down" one node. This will move to this node's first child, or if it
-- has no children, its
-- treeMoveDown :: Tree a -> Tree a
-- treeMoveDown (Tree name xs is) = Tree name xs (nodesMoveDown xs is)
--
-- nodesMoveDown :: [Node a] -> [Int] -> [Int]
-- nodesMoveDown _  [] = []
-- nodesMoveDown xs [i] = [clamp 0 (length xs - 1) (i + 1)]
-- nodesMoveDown xs (i:is) = (xs !! i)^.nodeChildren

--------------------------------------------------------------------------------

-- clamp :: Int -> Int -> Int -> Int
-- clamp i j n = max i (min j n)

-- Invariant: list of ints is not empty
-- treeIndex :: [Int] -> Traversal' [Node a] (Node a)
-- nodeIndex [n] = ix n
-- nodeIndex (n:ns) = ix n . nodeChildren . nodeIndex ns
--
-- idxDown :: [Int] -> [Int]
-- idxDown []     = []
-- idxDown (_:xs) = xs
--
-- idxRight :: [Int] -> [Int]
-- idxRight = over (ix 0) (subtract 1)
