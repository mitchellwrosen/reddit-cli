module Brick.Widgets.WrappedString
    ( wrappedStr
    , wrappedTxt
    ) where

import Brick.Markup
import Brick.Types
import Brick.Widgets.Core
import Data.List
import Data.Text               (Text, pack, unpack)
import Data.Text.Markup
import Graphics.Vty.Attributes (Attr)
import Text.Format.Para

wrappedStr :: String -> Attr -> Widget
wrappedStr s attr = Widget Fixed Fixed $ do
    c <- getContext
    let s' = intercalate "\n" (formatParas (availWidth c) Nothing (lines s))
    render (markup (pack s' @@ attr))

wrappedTxt :: Text -> Attr -> Widget
wrappedTxt t = wrappedStr (unpack t)
