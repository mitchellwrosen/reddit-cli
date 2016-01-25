{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Data.Text.Paragraph

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "left justify" $ do
        let go xs ys = justifyLeft 5 xs `shouldBe` ys

        it "doesnt affect short lines"     (go "foo"            "foo")
        it "retains newlines 1"            (go "foo\nbar"       "foo\nbar")
        it "retains newlines 2"            (go "foo\n\nbar"     "foo\n\nbar")
        it "moves short words down a line" (go "foo bar"        "foo\nbar")
        it "moves long words down a line"  (go "foo barbar"     "foo b\narbar")
        it "splits long words 1"           (go "foobar"         "fooba\nr")
        it "splits long words 2"           (go "foobarfoobar"   "fooba\nrfoob\nar")
        it "splits long words 3"           (go "foobar\nfoobar" "fooba\nr\nfooba\nr")
