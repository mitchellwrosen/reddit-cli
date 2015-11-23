{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main where

import Brick.Auto

import Brick.Markup
import Brick.Widgets.Dialog
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Auto           hiding ((<+>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Data.Default           (def)
import Data.List              (intersperse)
import Data.Maybe
import Data.Text              (Text)
import Data.Text.Markup
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

type Event = Vty.Event

data Viewing
    = ViewingPosts    -- Viewing subreddit posts
    | ViewingComments -- Viewing post comments

data Selected
    = SelectedEditor -- Selected editor
    | SelectedList   -- Selected post/comments list
    deriving Eq

data AppState = AppState
    { _appBreadcrumbs :: Maybe (Text, Maybe Post) -- The subreddit/post breadcrumgs
    , _appPosts       :: List Post                -- List of loaded posts
    , _appComments    :: List Comment             -- List of loaded comments
    , _appEditor      :: Editor                   -- Editor at the bottom for choosing a subreddit
    , _appError       :: Maybe String             -- The error to be displayed
    , _appViewing     :: Viewing                  -- Viewing posts list or comments list?
    , _appSelected    :: Selected                 -- Selected the editor or the posts/comments list?
    }
makeLenses ''AppState

initialState :: AppState
initialState = AppState
    { _appBreadcrumbs = Nothing
    , _appPosts       = list "posts" V.empty 1
    , _appComments    = list "comments" V.empty 1
    , _appEditor      = editor "editor" (str . concat) (Just 1) ""
    , _appError       = Nothing
    , _appViewing     = ViewingPosts
    , _appSelected    = SelectedEditor
    }

main :: IO ()
main = void (defaultMain redditApp initialState)
  where
    redditApp :: App AppState Event
    redditApp = App draw chooseCursor editorHandler pure (const def) id

    draw :: AppState -> [Widget]
    draw AppState{..} =
        let widget :: Widget
            widget =
                padBottom (Pad 1) (
                    case _appBreadcrumbs of
                        Just (subreddit, mpost) ->
                            case mpost of
                                Just post -> txt ("/r/" <> subreddit <> " - " <> Post.title post)
                                Nothing   -> txt ("/r/" <> subreddit)
                        Nothing -> txt ("Enter e.g. \"/haskell\" below"))
                <=>
                case _appViewing of
                    ViewingPosts    -> renderList _appPosts    drawPost
                    ViewingComments -> renderList _appComments drawComment
                <=>
                renderEditor _appEditor
                <=>
                footer
        in case _appError of
               Just err -> [renderDialog errorDialog (str err), widget]
               Nothing  -> [widget]
      where
        drawPost :: Bool -> Post -> Widget
        drawPost is_selected post = str (printf "%4d  " (Post.score post)) <+> markup (Post.title post @@ title_markup)
          where
            title_markup =
                if is_selected && _appSelected == SelectedList
                    then Vty.black `on` Vty.white
                    else mempty

        drawComment :: Bool -> Comment -> Widget
        drawComment is_selected c = markup (Comment.body c @@ comment_markup)
          where
            comment_markup =
                if is_selected && _appSelected == SelectedList
                    then fg Vty.magenta
                    else mempty

        footer :: Widget
        footer = mkFooter $
            case (_appSelected, _appViewing, n) of
                (SelectedEditor, ViewingPosts, 0) ->
                    [legend "esc q" "quit", legend "/abc" "subreddit"]
                (SelectedEditor, ViewingPosts, _) ->
                    [legend "esc q" "quit", legend "/abc" "subreddit", legend "tab" "posts"]
                (SelectedEditor, ViewingComments, 0) ->
                    [legend "esc" "back", legend "/abc" "subreddit"]
                (SelectedEditor, ViewingComments, _) ->
                    [legend "esc" "back", legend "/abc" "subreddit", legend "tab" "comments"]
                (SelectedList, ViewingPosts, 0) ->
                    [legend "esc" "quit", legend "tab /" "subreddit", legend "r" "refresh"]
                (SelectedList, ViewingPosts, _) ->
                    [legend "esc" "quit", legend "tab /" "subreddit", legend "r" "refresh", legend "enter l ⇨ " "comments"]
                (SelectedList, ViewingComments, _) ->
                    [legend "esc h ⇦ " "back", legend "tab /" "subreddit", legend "r" "refresh"]
          where
            n = case _appViewing of
                    ViewingPosts    -> _appPosts^.listElementsL.to V.length
                    ViewingComments -> _appComments^.listElementsL.to V.length

            legend :: Text -> Text -> Widget
            legend x y = colored Vty.cyan x <+> txt (" " <> y)

            mkFooter :: [Widget] -> Widget
            mkFooter = hBox . intersperse (colored Vty.yellow " | ")

        colored :: Vty.Color -> Text -> Widget
        colored c t = markup (t @@ fg c)

        errorDialog :: Dialog ()
        errorDialog = dialog "error" (Just "Error") (Just (0, [("Ok", ())])) 80

    chooseCursor :: AppState -> [CursorLocation] -> Maybe CursorLocation
    chooseCursor _ [] = Nothing
    chooseCursor s (c:_) =
        case s^.appSelected of
            SelectedEditor -> Just c
            SelectedList   -> Nothing

editorHandler :: AppState -> Handler AppState Event
editorHandler = handler step
  where
    step :: AppState -> Event -> EventM (Next AppState Event)
    step st ev
        | ev `isKey` Vty.KEsc = halt
        | ev `isKey` Vty.KEnter = do
            let msubreddit = T.pack (concat (getEditContents (st^.appEditor)))
            case T.uncons msubreddit of
                Just ('/', subreddit) -> do
                    result <- liftIO (runRedditAnon (getPosts' (Options Nothing (Just 20)) Hot (Just (R subreddit))))
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
                                         & appSelected            .~ SelectedList
                            next st' postsHandler

                _ ->
                    let error_st = st & appError    .~ Just ("Bad subreddit: '" ++ T.unpack msubreddit ++ "'")
                        next_st  = st & appEditor   %~ applyEdit clearEditor
                                      & appError    .~ Nothing
                                      & appSelected .~ SelectedList
                    in next error_st (errorDialogHandler next_st editorHandler)
        | ev `isKey` Vty.KChar '\t' = do
            let st' = st & appSelected .~ SelectedList
            next st' postsHandler
        | otherwise = do
            ed' <- handleEvent ev (st^.appEditor)
            continue (st & appEditor .~ ed')

postsHandler :: AppState -> Handler AppState Event
postsHandler = handler step
  where
    step :: AppState -> Event -> EventM (Next AppState Event)
    step st ev
        | ev `isKey` Vty.KEsc = halt
        | ev `isKey` Vty.KEnter = do
            case st ^. appPosts . to listSelectedElement of
                Just (_, post) -> do
                    result <- liftIO (runRedditAnon (getPostComments (Post.postID post)))
                    case result of
                        Left err ->
                            let error_st = st & appError .~ Just (show err)
                                next_st  = st & appError .~ Nothing
                            in next error_st (errorDialogHandler next_st postsHandler)
                        Right (Comment.PostComments _ comment_references) ->
                            let comments = V.fromList [ comment | Comment.Actual comment <- comment_references ]
                                st' = st & appBreadcrumbs._Just._2   .~ Just post
                                         & appComments.listElementsL .~ comments
                                         & appComments.listSelectedL .~ (if V.null comments then Nothing else Just 0)
                                         & appViewing                .~ ViewingComments
                            in next st' commentsHandler
                Nothing -> continue st
        | ev `isKey` Vty.KChar '/' || ev `isKey` Vty.KChar '\t' = do
            let st' = st & appEditor   %~ applyEdit (Z.insertChar '/' . clearEditor)
                         & appSelected .~ SelectedEditor
            next st' editorHandler
        | otherwise = do
            ps' <- handleEvent ev (st^.appPosts)
            continue (st & appPosts .~ ps')

commentsHandler :: AppState -> Handler AppState Event
commentsHandler = handler step
  where
    step :: AppState -> Event -> EventM (Next AppState Event)
    step st evt
        | evt `isKey` Vty.KEsc = do
            let st' = st & appBreadcrumbs._Just._2 .~ Nothing
                         & appViewing              .~ ViewingPosts
            next st' postsHandler
        | otherwise = do
            cs' <- handleEvent evt (st^.appComments)
            continue (st & appComments .~ cs')

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
    go = arr (\ev -> if ev `isKey` Vty.KEnter then Nothing else Just (Just state0))

--------------------------------------------------------------------------------

clearEditor :: Z.TextZipper String -> Z.TextZipper String
clearEditor = Z.killToEOL . Z.gotoBOL

isKey :: Vty.Event -> Vty.Key -> Bool
isKey (Vty.EvKey k _) k' = k == k'
isKey _ _ = False
