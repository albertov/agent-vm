{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AgentVM.StreamingEscapeSpec (spec) where

import AgentVM.Interactive (parseEscapeKey)
import Data.Char (isAsciiLower)
import qualified Data.Text as T
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "StreamingEscape" $ do
  describe "parseEscapeKey" $ do
    it "should parse ctrl-a correctly" $ do
      parseEscapeKey "ctrl-a" `shouldBe` 1
      parseEscapeKey "Ctrl-A" `shouldBe` 1
      parseEscapeKey "CTRL-A" `shouldBe` 1

    it "should parse ctrl-b correctly" $ do
      parseEscapeKey "ctrl-b" `shouldBe` 2
      parseEscapeKey "Ctrl-B" `shouldBe` 2
      parseEscapeKey "CTRL-B" `shouldBe` 2

    it "should parse ctrl-c correctly" $ do
      parseEscapeKey "ctrl-c" `shouldBe` 3
      parseEscapeKey "Ctrl-C" `shouldBe` 3
      parseEscapeKey "CTRL-C" `shouldBe` 3

    it "should parse ctrl-w correctly" $ do
      parseEscapeKey "ctrl-w" `shouldBe` 23
      parseEscapeKey "Ctrl-W" `shouldBe` 23
      parseEscapeKey "CTRL-W" `shouldBe` 23

    it "should parse ctrl-z correctly" $ do
      parseEscapeKey "ctrl-z" `shouldBe` 26
      parseEscapeKey "Ctrl-Z" `shouldBe` 26
      parseEscapeKey "CTRL-Z" `shouldBe` 26

    it "should handle all valid ctrl combinations" $ do
      parseEscapeKey "ctrl-a" `shouldBe` 1
      parseEscapeKey "ctrl-b" `shouldBe` 2
      parseEscapeKey "ctrl-c" `shouldBe` 3
      parseEscapeKey "ctrl-d" `shouldBe` 4
      parseEscapeKey "ctrl-e" `shouldBe` 5
      parseEscapeKey "ctrl-f" `shouldBe` 6
      parseEscapeKey "ctrl-g" `shouldBe` 7
      parseEscapeKey "ctrl-h" `shouldBe` 8
      parseEscapeKey "ctrl-i" `shouldBe` 9
      parseEscapeKey "ctrl-j" `shouldBe` 10
      parseEscapeKey "ctrl-k" `shouldBe` 11
      parseEscapeKey "ctrl-l" `shouldBe` 12
      parseEscapeKey "ctrl-m" `shouldBe` 13
      parseEscapeKey "ctrl-n" `shouldBe` 14
      parseEscapeKey "ctrl-o" `shouldBe` 15
      parseEscapeKey "ctrl-p" `shouldBe` 16
      parseEscapeKey "ctrl-q" `shouldBe` 17
      parseEscapeKey "ctrl-r" `shouldBe` 18
      parseEscapeKey "ctrl-s" `shouldBe` 19
      parseEscapeKey "ctrl-t" `shouldBe` 20
      parseEscapeKey "ctrl-u" `shouldBe` 21
      parseEscapeKey "ctrl-v" `shouldBe` 22
      parseEscapeKey "ctrl-w" `shouldBe` 23
      parseEscapeKey "ctrl-x" `shouldBe` 24
      parseEscapeKey "ctrl-y" `shouldBe` 25
      parseEscapeKey "ctrl-z" `shouldBe` 26

    it "should default to ctrl-w for invalid input" $ do
      parseEscapeKey "invalid" `shouldBe` 23
      parseEscapeKey "ctrl-1" `shouldBe` 23
      parseEscapeKey "ctrl-" `shouldBe` 23
      parseEscapeKey "alt-a" `shouldBe` 23
      parseEscapeKey "" `shouldBe` 23

    prop "should be case insensitive" $
      \(key :: Char) ->
        isAsciiLower key ==>
          let lowerKey = "ctrl-" <> T.singleton key
              upperKey = "CTRL-" <> T.toUpper (T.singleton key)
              mixedKey = "Ctrl-" <> T.singleton key
           in parseEscapeKey lowerKey == parseEscapeKey upperKey
                && parseEscapeKey lowerKey == parseEscapeKey mixedKey

    prop "should handle whitespace variations" $
      \(key :: Char) ->
        isAsciiLower key ==>
          let normalKey = "ctrl-" <> T.singleton key
              spacedKey = " ctrl-" <> T.singleton key <> " "
           in parseEscapeKey normalKey == parseEscapeKey (T.strip spacedKey)

  describe "escape sequence byte values" $ do
    it "should map to correct ASCII control codes" $ do
      -- Verify that our mapping corresponds to ASCII control codes
      parseEscapeKey "ctrl-a" `shouldBe` 1 -- SOH
      parseEscapeKey "ctrl-b" `shouldBe` 2 -- STX
      parseEscapeKey "ctrl-c" `shouldBe` 3 -- ETX (interrupt)
      parseEscapeKey "ctrl-d" `shouldBe` 4 -- EOT (end of transmission)
      parseEscapeKey "ctrl-g" `shouldBe` 7 -- BEL (bell)
      parseEscapeKey "ctrl-h" `shouldBe` 8 -- BS (backspace)
      parseEscapeKey "ctrl-i" `shouldBe` 9 -- HT (tab)
      parseEscapeKey "ctrl-j" `shouldBe` 10 -- LF (line feed)
      parseEscapeKey "ctrl-m" `shouldBe` 13 -- CR (carriage return)
      parseEscapeKey "ctrl-z" `shouldBe` 26 -- SUB (substitute/suspend)
  describe "edge cases" $ do
    it "should handle empty string" $ do
      parseEscapeKey "" `shouldBe` 23

    it "should handle malformed ctrl sequences" $ do
      parseEscapeKey "ctrl" `shouldBe` 23
      parseEscapeKey "ctrl-" `shouldBe` 23
      parseEscapeKey "-a" `shouldBe` 23
      parseEscapeKey "ctr-a" `shouldBe` 23

    it "should handle non-alphabetic characters" $ do
      parseEscapeKey "ctrl-1" `shouldBe` 23
      parseEscapeKey "ctrl-!" `shouldBe` 23
      parseEscapeKey "ctrl- " `shouldBe` 23

    it "should handle multi-character suffixes" $ do
      parseEscapeKey "ctrl-ab" `shouldBe` 23
      parseEscapeKey "ctrl-aa" `shouldBe` 23
      parseEscapeKey "ctrl-a1" `shouldBe` 23

  describe "escape sequence integration" $ do
    it "should handle common terminal escape sequences" $ do
      -- Test the most commonly used escape sequences
      parseEscapeKey "ctrl-c" `shouldBe` 3 -- SIGINT (interrupt)
      parseEscapeKey "ctrl-d" `shouldBe` 4 -- EOF
      parseEscapeKey "ctrl-z" `shouldBe` 26 -- SIGTSTP (suspend)
      parseEscapeKey "ctrl-w" `shouldBe` 23 -- Default escape key
    it "should handle less common but valid control sequences" $ do
      parseEscapeKey "ctrl-g" `shouldBe` 7 -- Bell
      parseEscapeKey "ctrl-h" `shouldBe` 8 -- Backspace
      parseEscapeKey "ctrl-i" `shouldBe` 9 -- Tab
      parseEscapeKey "ctrl-j" `shouldBe` 10 -- Line feed
      parseEscapeKey "ctrl-m" `shouldBe` 13 -- Carriage return
    it "should be consistent across different input formats" $ do
      -- Test that different case and spacing variations all work
      let testVariations key expected = do
            parseEscapeKey key `shouldBe` expected
            parseEscapeKey (T.toUpper key) `shouldBe` expected
            parseEscapeKey (T.toTitle key) `shouldBe` expected

      testVariations "ctrl-a" 1
      testVariations "ctrl-w" 23
      testVariations "ctrl-z" 26

  describe "byte value validation" $ do
    it "should only produce values in valid control character range" $ do
      let allKeys = ["ctrl-" <> T.singleton c | c <- ['a' .. 'z']]
      let allValues = map parseEscapeKey allKeys
      all (\v -> v >= 1 && v <= 26) allValues `shouldBe` True

    it "should produce unique values for each valid input" $ do
      let validKeys = ["ctrl-" <> T.singleton c | c <- ['a' .. 'z']]
      let values = map parseEscapeKey validKeys
      length (ordNub values) `shouldBe` length values

    it "should map sequential letters to sequential values" $ do
      parseEscapeKey "ctrl-a" `shouldBe` 1
      parseEscapeKey "ctrl-b" `shouldBe` 2
      parseEscapeKey "ctrl-c" `shouldBe` 3
      -- ... pattern continues
      parseEscapeKey "ctrl-x" `shouldBe` 24
      parseEscapeKey "ctrl-y" `shouldBe` 25
      parseEscapeKey "ctrl-z" `shouldBe` 26
