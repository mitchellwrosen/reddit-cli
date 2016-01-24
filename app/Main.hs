{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main where

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

type Event = Key

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
    , _appPosts       = list "posts" V.empty 1
    , _appComments    = forest "comments" []
    , _appEditor      = editor "editor" (str . concat) (Just 1) ""
    , _appError       = Nothing
    , _appViewing     = ViewingPosts
    , _appSelected    = SelectedEditor
    }

main :: IO ()
main = void (defaultMain redditApp initialState)
  where
    redditApp :: App AppState Event
    redditApp = App draw chooseCursor editorHandler pure (const def) eventToKey

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
                        Nothing -> txt ("Enter e.g. \"haskell\" below"))
                <=>
                case _appViewing of
                    ViewingPosts    -> renderList _appPosts drawPost
                    ViewingComments -> renderForest drawComment _appComments
                <=>
                case _appSelected of
                    SelectedEditor    -> txt "/r/" <+> renderEditor _appEditor
                    NotSelectedEditor -> emptyWidget
                <=>
                footer
        in case _appError of
               Just err -> [renderDialog errorDialog (str err), widget]
               Nothing  -> [widget]
      where
        drawPost :: Bool -> Post -> Widget
        drawPost is_selected post =
            str (printf "%4d  " (Post.score post)) <+>
            markup (Post.title post @@ title_markup)
          where
            title_markup =
                if is_selected && _appSelected == NotSelectedEditor
                    then Vty.black `on` Vty.white
                    else mempty

        drawComment :: IsSelected -> IsCollapsed -> Comment -> Widget
        drawComment is_selected is_collapsed c = comment_widget
          where
            comment_widget :: Widget
            comment_widget
                | is_collapsed =
                    str (printf "%4d  " (fromMaybe 0 (Comment.score c))) <+>
                    (markup (("+ " <> unUsername (Comment.author c)) @@ comment_markup))
                | otherwise =
                    str (printf "%4d  " (fromMaybe 0 (Comment.score c))) <+>
                    (markup (("- " <> unUsername (Comment.author c)) @@ comment_markup)
                     <=>
                     padRight (Pad 4) (wrappedTxt (Comment.body c) comment_markup))

            comment_markup :: Vty.Attr
            comment_markup =
                if is_selected && _appSelected == NotSelectedEditor
                    then Vty.black `on` Vty.white
                    else mempty

            unUsername :: User.Username -> Text
            unUsername (Username x) = x

        footer :: Widget
        footer = mkFooter $
            case (_appSelected, _appViewing, n) of
                (SelectedEditor, ViewingPosts, 0) ->
                    [legend "esc" "quit"]
                (SelectedEditor, ViewingPosts, _) ->
                    [legend "esc" "quit", legend "tab" "posts"]
                (SelectedEditor, ViewingComments, 0) ->
                    [legend "esc" "back"]
                (SelectedEditor, ViewingComments, _) ->
                    [legend "esc" "back", legend "tab" "comments"]
                (NotSelectedEditor, ViewingPosts, 0) ->
                    [legend "esc" "quit", legend "tab" "subreddit", legend "r" "refresh"]
                (NotSelectedEditor, ViewingPosts, _) ->
                    [legend "esc" "quit", legend "tab" "subreddit", legend "enter" "comments", legend "r" "refresh"]
                (NotSelectedEditor, ViewingComments, 0) ->
                    [legend "esc" "back", legend "tab" "subreddit", legend "r" "refresh"]
                (NotSelectedEditor, ViewingComments, _) ->
                    [legend "esc" "back", legend "tab" "subreddit", legend "enter" "collapse", legend "r" "refresh"]
          where
            n = case _appViewing of
                    ViewingPosts    -> _appPosts^.listElementsL.to V.length
                    ViewingComments -> _appComments^.forestTrees.to length

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
