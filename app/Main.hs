{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Main where

import Model
import View

import Brick.Auto
import Brick.Widgets.Tree
import Brick.Widgets.WrappedString

import Brick.Markup
import Brick.Widgets.Dialog
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Auto           hiding ((<+>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Data.Bits
import Data.Default           (def)
import Data.List              (intersperse)
import Data.Maybe
import Data.Text              (Text)
import Data.Text.Markup
import Graphics.Vty           (Key(..))
import Prelude                hiding ((.), id)
import Reddit
import Reddit.Types.Listing
import Text.Printf

import qualified Data.Text            as T
import qualified Data.Text.Zipper     as Z
import qualified Data.Vector          as V
import qualified Graphics.Vty         as Vty
import qualified Reddit.Types.Comment as Comment
import qualified Reddit.Types.Post    as Post
import qualified Reddit.Types.User    as User

-- An event is a key press - we don't use mouse events or resize events.
type Event = Key

main :: IO ()
main = void (defaultMain redditApp initialState)
  where
    redditApp :: App AppState Event
    redditApp = App drawApp chooseCursor editorHandler pure (const def) eventToKey

    chooseCursor :: AppState -> [CursorLocation] -> Maybe CursorLocation
    chooseCursor _ [] = Nothing
    chooseCursor s (c:_) =
        case s^.appSelected of
            SelectedEditor    -> Just c
            NotSelectedEditor -> Nothing

    eventToKey :: Vty.Event -> Maybe Key
    eventToKey (Vty.EvKey key _) = Just key
    eventToKey _ = Nothing

editorHandler :: AppState -> Handler AppState Event
editorHandler = handler step
  where
    step :: AppState -> Event -> EventM (Next AppState Event)
    step _  KEsc = halt
    step st KEnter = do
        let subreddit =
                case T.pack (concat (getEditContents (st^.appEditor))) of
                    "" -> "all"
                    x  -> x

        result <- liftIO (runRedditAnon
                              (getPosts'
                                   (Options Nothing (Just 20))
                                   Hot
                                   (Just (R subreddit))))
        case result of
            Left err ->
                let error_st = st & appError  .~ Just (show err)
                    next_st  = st & appEditor %~ applyEdit clearEditor
                                  & appError  .~ Nothing
                in next error_st (errorDialogHandler next_st editorHandler)
            Right listing -> do
                let posts = V.fromList (contents listing)
                    st' = st & appBreadcrumbs         .~ Just (subreddit, Nothing)
                             & appPosts.listElementsL .~ posts
                             & appPosts.listSelectedL .~ (if V.null posts then Nothing else Just 0)
                             & appEditor              %~ applyEdit clearEditor
                             & appSelected            .~ NotSelectedEditor
                next st' postsHandler

    step st KTab = do
        let st' = st & appSelected .~ NotSelectedEditor
        case st^.appViewing of
            ViewingPosts    -> next st' postsHandler
            ViewingComments -> next st' commentsHandler
    step st key = do
        ed' <- handleEvent (Vty.EvKey key []) (st^.appEditor)
        continue (st & appEditor .~ ed')

postsHandler :: AppState -> Handler AppState Event
postsHandler = handler step
  where
    step :: AppState -> Event -> EventM (Next AppState Event)
    step _ KEsc = halt
    step st key | key == KEnter || key == KChar 'l' = do
        case st ^. appPosts . to listSelectedElement of
            Just (_, post) -> do
                result <- liftIO (runRedditAnon (getPostComments (Post.postID post)))
                case result of
                    Left err ->
                        let error_st = st & appError .~ Just (show err)
                            next_st  = st & appError .~ Nothing
                        in next error_st (errorDialogHandler next_st postsHandler)
                    Right (Comment.PostComments _ comment_references) ->
                        let st' = st & appBreadcrumbs._Just._2   .~ Just post
                                     & appComments               .~ commentsToForest comment_references
                                     & appViewing                .~ ViewingComments
                        in next st' commentsHandler
            Nothing -> continue st
    step st key | key == KTab || key == KChar '/' = do
        let st' = st & appEditor   %~ applyEdit clearEditor
                     & appSelected .~ SelectedEditor
        next st' editorHandler
    step st key | key == KUp || key == KChar 'k' =
        continue (st & appPosts %~ listMoveUp)
    step st key | key == KDown || key == KChar 'j' =
        continue (st & appPosts %~ listMoveDown)
    step st _ = continue st

commentsHandler :: AppState -> Handler AppState Event
commentsHandler = handler step
  where
    step :: AppState -> Event -> EventM (Next AppState Event)
    step st key | key == KEsc || key == KChar 'h' = do
        let st' = st & appBreadcrumbs._Just._2 .~ Nothing
                     & appViewing              .~ ViewingPosts
        next st' postsHandler
    step st key | key == KTab || key == KChar '/' = do
        let st' = st & appEditor   %~ applyEdit clearEditor
                     & appSelected .~ SelectedEditor
        next st' editorHandler
    step st KEnter =
        continue (st & appComments %~ forestCollapse)
    step st key | key == KUp || key == KChar 'k' =
        continue (st & appComments %~ forestMoveUp)
    step st key | key == KDown || key == KChar 'j' =
        continue (st & appComments %~ forestMoveDown)
    step st _ = continue st

errorDialogHandler
    :: AppState
    -> (AppState -> Handler AppState Event)
    -> AppState
    -> Handler AppState Event
errorDialogHandler state1 k state0 =
    go                                     -- Emit state0 for all events until Enter is pressed.
    --> lmap (\_ -> Just state1) (onFor 1) -- Emit state1 so 'next' doesn't have to handle this 'Enter'.
    --> k state1                           -- Move on
  where
    go :: Interval EventM Event (Maybe AppState)
    go = arr (\case
                  KEnter -> Nothing
                  _      -> Just (Just state0))

--------------------------------------------------------------------------------

commentsToForest :: [Comment.CommentReference] -> Forest Comment
commentsToForest = forest "comments" . catMaybes . map commentToTree

commentsToTrees :: [Comment.CommentReference] -> [Tree Comment]
commentsToTrees = catMaybes . map commentToTree

commentToTree :: Comment.CommentReference -> Maybe (Tree Comment)
commentToTree (Comment.Actual c) = Just (tree c (commentsToTrees (contents (Comment.replies c))))
commentToTree _ = Nothing

clearEditor :: Z.TextZipper String -> Z.TextZipper String
clearEditor = Z.killToEOL . Z.gotoBOL

pattern KTab = KChar '\t'
