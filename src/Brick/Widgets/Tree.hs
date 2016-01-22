{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Brick.Widgets.Tree
    ( Forest(..)
    , forestName
    , forestTrees
    , forestSelected
    , Tree(..)
    , treeElem
    , treeChildren
    , forest
    , renderForest
    , forestCollapse
    , forestMoveDown
    , forestMoveUp
    , IsSelected
    , IsCollapsed
    ) where

import Data.List.Zipper

import Control.Arrow      ((>>>))
import Control.Monad
import Brick.Types
import Brick.Widgets.Core
import Control.Lens       hiding ((|>))
import Data.Sequence      (Seq, ViewL(..), (|>))
import GHC.Exts

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
    -- Invariant: Sequences are non-empty.
    , _forestSelected :: Maybe (Z (Seq Int))
    }
makeLenses ''Forest

-- | Construct a 'Forest', given a forest of 'Tree's.
forest :: Name -> [Tree a] -> Forest a
forest name trees = Forest name trees (zipper (dfForest mempty trees))

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
renderForest :: forall a. (IsSelected -> IsCollapsed -> a -> Widget) -> Forest a -> Widget
renderForest draw (Forest name ts selected) = Widget Greedy Greedy $ do
    render (viewport name Vertical (treesWidget (fmap zpeek selected) ts))
  where
    treeWidget :: Maybe (Seq Int) -> Tree a -> Widget
    treeWidget idx (Tree x xs collapsed) =
        let w0 = draw (isSelected idx) collapsed x
            w1 = treesWidget (idxDown idx) xs
        in if collapsed
               then w0
               else w0 <=> padLeft (Pad 2) w1
      where
        isSelected :: Maybe (Seq Int) -> Bool
        isSelected (Just s) | s == Seq.singleton 0 = True
        isSelected _ = False

    treesWidget :: Maybe (Seq Int) -> [Tree a] -> Widget
    treesWidget idx trees = vBox (map (uncurry treeWidget) (zip (iterate idxRight idx) trees))

-- | Collapse (or uncollapse) the selected 'Tree' of a 'Forest'.
forestCollapse :: forall a. Forest a -> Forest a
forestCollapse (Forest name trees Nothing) = Forest name trees Nothing
forestCollapse (Forest name trees (Just z)) =
    let selected :: Traversal' [Tree a] (Tree a)
        selected = treeIndex (toList (zpeek z))

        trees' :: [Tree a]
        trees' = over (selected . treeCollapsed) not trees
    in forest name trees'

-- | Move down one node, depth-first.
forestMoveDown :: Forest a -> Forest a
forestMoveDown = over (forestSelected . _Just) zdown

-- | Move up one node, depth-first.
forestMoveUp :: Forest a -> Forest a
forestMoveUp = over (forestSelected . _Just) zup

--------------------------------------------------------------------------------

-- Invariant: list of ints is not empty
treeIndex :: [Int] -> Traversal' [Tree a] (Tree a)
treeIndex [n] = ix n
treeIndex (n:ns) = ix n . treeChildren . treeIndex ns

idxDown :: Maybe (Seq Int) -> Maybe (Seq Int)
idxDown = join . fmap safeTail
  where
    safeTail :: Seq Int -> Maybe (Seq Int)
    safeTail = Seq.viewl >>> \case
        EmptyL  -> Nothing
        _ :< xs -> Just xs

idxRight :: Maybe (Seq Int) -> Maybe (Seq Int)
idxRight = over (_Just . seqHead) (subtract 1)
  where
    seqHead :: Setter' (Seq a) a
    seqHead = sets (\f -> Seq.viewl >>> \case
                              EmptyL  -> mempty
                              x :< xs -> f x <| xs)
