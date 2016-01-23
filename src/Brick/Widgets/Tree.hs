{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Brick.Widgets.Tree
    ( Forest(..)
    , forestName
    , forestTrees
    , forestSelected
    , forest
    , Tree(..)
    , treeElem
    , treeChildren
    , tree
    , renderForest
    , forestCollapse
    , forestMoveDown
    , forestMoveUp
    , IsSelected
    , IsCollapsed
    ) where

import Data.List.Zipper

import Control.Arrow      ((>>>))
import Brick.Types
import Brick.Widgets.Core
import Control.Lens       hiding (Index, (|>))
import Data.Sequence      (Seq, ViewL(..), (|>))
import GHC.Exts

import qualified Data.Sequence      as Seq

type IsSelected = Bool
type IsCollapsed = Bool

-- No compile-time protection here; just documentation that these Seqs should
-- all be non-empty.
type NonEmptySeq a = Seq a

-- | A zipper of sequences of integers; each sequence represents the unique path
-- to a node in the forest (as indices into each parent's list of children).
-- Zipping up and down moves through the forest depth-first.
type Indices = Z Index
type Index   = NonEmptySeq Int

-- | A single tree of a forest, which has 0 or more children trees and may be
-- collapsed.
data Tree a = Tree
    { _treeElem        :: a
    , _treeChildren    :: [Tree a]
    , _treeCollapsed   :: !Bool
    } deriving Show
makeLenses ''Tree

tree :: a -> [Tree a] -> Tree a
tree x xs = Tree x xs False

-- | A forest of trees with a single selected tree if non-empty.
data Forest a = Forest
    { _forestName     :: Name
    , _forestTrees    :: [Tree a]
    , _forestSelected :: Maybe Indices
    } deriving Show
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
dfTree :: Index -> Tree a -> [Index]
dfTree is (Tree _ _  True) = [is]
dfTree is (Tree _ ns _)    = is : dfForest is ns

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
dfForest :: Index -> [Tree a] -> [Index]
dfForest is ns = concatMap go (zip [0..] ns)
  where
    go :: (Int, Tree a) -> [Index]
    go (i, n) = dfTree (is |> i) n

-- | Render a forest, given an element rendering function. Children of collapsed
-- elements will not be rendered.
renderForest :: forall a. (IsSelected -> IsCollapsed -> a -> Widget) -> Forest a -> Widget
renderForest draw (Forest name ts selected) = Widget Greedy Greedy $ do
    render (viewport name Vertical (treesWidget (fmap zpeek selected) ts))
  where
    treeWidget :: Maybe Index -> Tree a -> Widget
    treeWidget idx (Tree x xs collapsed) =
        let selected :: Bool
            selected = idx == Just (Seq.singleton 0)

            makeVisible :: Widget -> Widget
            makeVisible
                | selected  = visible
                | otherwise = id

            w0 :: Widget
            w0 = makeVisible (draw selected collapsed x)

            w1 :: Widget
            w1 = treesWidget (idx >>= idxDown) xs

        in if collapsed
               then w0
               else w0 <=> padLeft (Pad 2) w1

    treesWidget :: Maybe Index -> [Tree a] -> Widget
    treesWidget idx trees = vBox (map (uncurry treeWidget) (zip rights trees))
      where
        rights :: [Maybe Index]
        rights = iterate (>>= idxRight) idx

-- | Collapse (or uncollapse) the selected 'Tree' of a 'Forest'.
forestCollapse :: forall a. Forest a -> Forest a
forestCollapse (Forest name trees Nothing) = Forest name trees Nothing
forestCollapse (Forest name trees (Just z)) =
    -- So we recreate the index zipper every time a comment is collapsed...
    -- perhaps some re-thinking of the type is in order. But, at least moving
    -- up and down is fast.
    let selected :: Traversal' [Tree a] (Tree a)
        selected = treeIndex (toList (zpeek z))

        trees' :: [Tree a]
        trees' = over (selected . treeCollapsed) not trees
    in applyN (zllen z) forestMoveDown (forest name trees')
  where
    applyN :: forall a. Int -> (a -> a) -> a -> a
    applyN 0 _ x = x
    applyN n f x = applyN (n-1) f (f $! x)

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

-- Follow an index down one level. Only succeeds when the index is indeed
-- pointing at a child of the current tree.
idxDown :: Index -> Maybe Index
idxDown = Seq.viewl >>> \case
    0 :< xs ->
        if length xs > 0
            then Just xs
            else Nothing
    _ -> Nothing

-- Shift all top-level indices to the "right", to accomodate walking down a list
-- of sibling trees. That is to say, an index such as
--
--    [ 1, 0, 1 ]
--
-- relative to the *tail* of the current level of trees is
--
--    Just [ 0, 0, 1 ]
--
-- and an index such as
--
--    [ 0, 1 ]
--
-- relative to the *tail* of the current level of trees is
--
--    Nothing
--
-- (because the 0 refers to the head sibling).
--
idxRight :: Index -> Maybe Index
idxRight = Seq.viewl >>> \case
    0 :< _  -> Nothing
    x :< xs -> Just (x-1 <| xs)
    EmptyL  -> error "Invariant violated: index empty"
