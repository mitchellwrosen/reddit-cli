{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module View
    ( drawApp
    ) where

import Model

import Brick.Widgets.Tree
import Brick.Widgets.WrappedString

import Brick.Markup         (markup)
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Brick.Widgets.Dialog (Dialog, dialog, renderDialog)
import Brick.Widgets.Edit   (renderEditor)
import Brick.Widgets.List   (listElementsL, renderList)
import Control.Lens
import Data.List
import Data.Monoid
import Data.Text            (Text)
import Data.Text.Markup     ((@@))
import Reddit.Types.Comment (Comment)
import Reddit.Types.Post    (Post)
import Text.Printf          (printf)

import qualified Data.Text            as T
import qualified Graphics.Vty         as Vty
import qualified Reddit.Types.Comment as Comment
import qualified Reddit.Types.Post    as Post
import qualified Reddit.Types.User    as User

drawApp :: AppState -> [Widget]
drawApp AppState{..} =
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
                ViewingPosts ->
                    let draw :: IsSelected -> Post -> Widget
                        draw =
                            case _appSelected of
                                SelectedEditor    -> const (drawPost False)
                                NotSelectedEditor -> drawPost
                    in renderList _appPosts draw
                ViewingComments ->
                    let draw :: IsSelected -> IsCollapsed -> Comment -> Widget
                        draw =
                            case _appSelected of
                                SelectedEditor    -> const (drawComment False)
                                NotSelectedEditor -> drawComment
                    in renderForest draw _appComments
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
                ViewingPosts    -> _appPosts^.listElementsL.to length
                ViewingComments -> _appComments^.forestTrees.to length

        legend :: Text -> Text -> Widget
        legend x y = colored Vty.cyan x <+> txt (" " <> y)

        mkFooter :: [Widget] -> Widget
        mkFooter = hBox . intersperse (colored Vty.yellow " | ")

    colored :: Vty.Color -> Text -> Widget
    colored c t = markup (t @@ fg c)

    errorDialog :: Dialog ()
    errorDialog = dialog "error" (Just "Error") (Just (0, [("Ok", ())])) 80

-- | Render a post listing.
drawPost :: Bool -> Post -> Widget
drawPost is_selected post =
    str (printf "%4d  " (Post.score post)) <+>
    markup (Post.title post @@ title_markup)
  where
    title_markup =
        if is_selected
            then Vty.black `on` Vty.white
            else mempty

-- | Render a comment listing.
drawComment :: IsSelected -> IsCollapsed -> Comment -> Widget
drawComment is_selected is_collapsed c = comment_widget
  where
    comment_widget :: Widget
    comment_widget
        | is_collapsed = header
        | otherwise =
            header
            <=>
            padRight (Pad 4) (wrappedTxt (Comment.body c) comment_markup)

    header :: Widget
    header = markup (
        (unUsername (Comment.author c) <> " · " <> maybe "?" tshow (Comment.score c))
        @@ Vty.withStyle mempty Vty.bold)

    comment_markup :: Vty.Attr
    comment_markup =
        if is_selected
            then Vty.black `on` Vty.white
            else mempty

    unUsername :: User.Username -> Text
    unUsername (User.Username x) = x

--------------------------------------------------------------------------------

tshow :: Show a => a -> Text
tshow = T.pack . show
