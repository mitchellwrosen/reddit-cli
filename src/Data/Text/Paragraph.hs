{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Paragraph
    ( justifyLeft
    ) where

import Data.List
import Data.Monoid
import Data.Text (Text)

import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TL

justifyLeft :: Int -> Text -> Text
justifyLeft wmax
    = TL.toStrict
    . TL.toLazyText
    . mconcat
    . intersperse "\n"
    . map (justifyLeftLine wmax)
    . T.lines

justifyLeftLine :: Int -> Text -> TL.Builder
justifyLeftLine wmax = go mempty 0 . T.words
  where
    go :: TL.Builder -> Int -> [Text] -> TL.Builder
    go acc _ [] = acc
    -- The beginning of the line is a special case in that words added do not have a
    -- preceding space.
    go acc 0 (t:ts)
        -- Word takes up entire line by itself - append it and a newline and recurse
        | T.length t > wmax =
            let (t1, t2) = T.splitAt wmax t
            in go (acc <> (TL.fromText t1 <> "\n")) 0 (t2:ts)
        -- Word fits in a line - append it
        | otherwise = go (acc <> TL.fromText t) (0 + T.length t) ts
    go acc wcur (t:ts)
        -- Preceding space + word does not fit.
        | wcur + T.length t + 1 > wmax =
            -- If the word wouldn't even fit on a line by itself, and we can put at
            -- least one letter on this line, do so.
            --
            -- Otherwise, don't split the word, and just move to the next line.
            if T.length t > wmax && wcur + 2 <= wmax
                then let (t1, t2) = T.splitAt (wmax - wcur - 1) t
                     in go (acc <> (" " <> (TL.fromText t1 <> "\n"))) 0 (t2:ts)
                else go (acc <> "\n") 0 (t:ts)
        -- Preceding space + word does fit, so append it.
        | otherwise = go (acc <> (" " <> TL.fromText t)) (wcur + T.length t + 1) ts
