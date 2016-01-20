{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Brick.Widgets.Tree
    ( Tree(..)
    , treeName
    , treeNodes
    , treeSelected
    , Node(..)
    , nodeElem
    , nodeChildren
    , nodeCollapsed
    , tree
    , renderTree
    , treeCollapse
    ) where

import Brick.Types
import Brick.Widgets.Core
import Control.Lens
import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.List.NonEmpty as NonEmpty

type NodeSelected = Bool
type NodeCollapsed = Bool

data Node a = Node
    { _nodeElem      :: a
    , _nodeChildren  :: [Node a]
    , _nodeCollapsed :: Bool
    }
makeLenses ''Node

data Tree a = Tree
    { _treeName :: Name
    , _treeNodes :: [Node a]
    , _treeSelected :: (Maybe (NonEmpty Int))
    }
makeLenses ''Tree

tree :: Name -> [Node a] -> Tree a
tree name nodes = Tree name nodes idx
  where
    idx = if null nodes
              then Nothing
              else Just (pure 0)

renderTree :: Tree a -> (NodeSelected -> NodeCollapsed -> a -> Widget) -> Widget
renderTree (Tree name nodes sel) draw = renderNodes nodes (NonEmpty.toList <$> sel) draw

renderNodes
    :: [Node a]
    -> Maybe [Int]
    -> (NodeSelected -> NodeCollapsed -> a -> Widget)
    -> Widget
renderNodes ns idx draw =
    vBox (map (\(n,i) -> renderNode n i draw) (zip ns (iterate idxRight idx)))

renderNode
    :: Node a
    -> Maybe [Int]
    -> (NodeSelected -> NodeCollapsed -> a -> Widget)
    -> Widget
renderNode (Node x xs collapsed) idx draw =
    let w0 = draw (idx == Just [0]) collapsed x
        w1 = renderNodes xs (idxDown idx) draw
    in if collapsed
           then w0
           else w0 <=> padLeft (Pad 1) w1

treeCollapse :: Tree a -> Tree a
treeCollapse t = t & treeNodes %~ nodesCollapse (t^.treeSelected)

nodesCollapse :: Maybe (NonEmpty Int) -> [Node a] -> [Node a]
nodesCollapse Nothing = id
nodesCollapse (Just ns) = over (nodeIndex ns . nodeCollapsed) not

nodeIndex :: NonEmpty Int -> Traversal' [Node a] (Node a)
nodeIndex (n :| []) = ix n
nodeIndex (n :| (m:ms)) = ix n . nodeChildren . nodeIndex (m :| ms)

--------------------------------------------------------------------------------

idxDown :: Maybe [Int] -> Maybe [Int]
idxDown Nothing       = Nothing
idxDown (Just [])     = Nothing
idxDown (Just (_:xs)) = Just xs

-- Invariant: cannot be Just []
idxRight :: Maybe [Int] -> Maybe [Int]
idxRight Nothing       = Nothing
idxRight (Just (0:_))  = Nothing
idxRight (Just (n:ns)) = Just (n-1 : ns)
idxRight err           = error ("idxLeft: " ++ show err)
