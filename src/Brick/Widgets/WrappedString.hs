module Brick.Widgets.WrappedString
    ( wrappedStr
    , wrappedTxt
    ) where

import Data.Text.Paragraph

import Brick.Markup
import Brick.Types
import Brick.Widgets.Core
import Data.List
import Data.Text               (Text, pack)
import Data.Text.Markup
import Graphics.Vty.Attributes (Attr)

wrappedStr :: String -> Attr -> Widget
wrappedStr s = wrappedTxt (pack s)

wrappedTxt :: Text -> Attr -> Widget
wrappedTxt t attr = Widget Fixed Fixed $ do
    c <- getContext
    render (markup (justifyLeft (availWidth c) t @@ attr))
