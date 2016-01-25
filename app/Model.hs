{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Model where

import Brick.Widgets.Tree

import Brick.Widgets.Core (str)
import Brick.Widgets.Edit (Editor, editor)
import Brick.Widgets.List (List, list)
import Control.Lens       (makeLenses)
import Data.Monoid
import Data.Text          (Text)
import Reddit.Types       (Comment, Post)

data Viewing
    = ViewingPosts    -- Viewing subreddit posts
    | ViewingComments -- Viewing post comments

data Selected
    = SelectedEditor    -- Selected editor
    | NotSelectedEditor -- Selected post list or comments forest
    deriving Eq

data AppState = AppState
    { _appBreadcrumbs :: Maybe (Text, Maybe Post) -- The subreddit/post breadcrumbs
    , _appPosts       :: List Post                -- List of loaded posts
    , _appComments    :: Forest Comment           -- Forest of loaded comments
    , _appEditor      :: Editor                   -- Editor at the bottom for choosing a subreddit
    , _appError       :: Maybe String             -- The error to be displayed
    , _appViewing     :: Viewing                  -- Viewing posts list or comments list?
    , _appSelected    :: Selected                 -- Selected the editor or the posts/comments list?
    }
makeLenses ''AppState

initialState :: AppState
initialState = AppState
    { _appBreadcrumbs = Nothing
    , _appPosts       = list "posts" mempty 1
    , _appComments    = forest "comments" []
    , _appEditor      = editor "editor" (str . concat) (Just 1) ""
    , _appError       = Nothing
    , _appViewing     = ViewingPosts
    , _appSelected    = SelectedEditor
    }
