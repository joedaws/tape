{-# LANGUAGE OverloadedStrings #-}
module EventSpec (spec) where

import qualified Tape
import Tape (tapeText)
import Event
import Test.Hspec
import Data.Maybe (isJust)

spec :: Spec
spec = do
  describe "initialState" $ do
    it "starts with exactly 1 tape" $
      length (_tapes initialState) `shouldBe` 1

  describe "calcMaxTapes" $ do
    it "returns 1 for a very short terminal" $
      calcMaxTapes 5 `shouldBe` 1
    it "returns correct value for height 24" $
      -- (24 - 4) `div` 2 = 10
      calcMaxTapes 24 `shouldBe` 10
    it "returns correct value for height 40" $
      -- (40 - 4) `div` 2 = 18
      calcMaxTapes 40 `shouldBe` 18

  describe "addTapeToSt" $ do
    it "adds a tape below existing ones" $
      length (_tapes (addTapeToSt st40)) `shouldBe` 2
    it "sets focusIdx to the new tape's index" $
      _focusIdx (addTapeToSt st40) `shouldBe` 1
    it "new tape is empty" $
      tapeText (last (_tapes (addTapeToSt st40))) `shouldBe` ""
    it "sets statusMsg at max capacity" $
      isJust (_statusMsg (addTapeToSt st5)) `shouldBe` True
    it "does not add a tape at max capacity" $
      length (_tapes (addTapeToSt st5)) `shouldBe` 1

  describe "focusNextSt" $ do
    it "advances focusIdx from 0 to 1" $
      _focusIdx (focusNextSt (addTapeToSt st40){ _focusIdx = 0}) `shouldBe` 1
    it "wraps focusIdx from last to 0" $
      _focusIdx (focusNextSt (addTapeToSt st40)) `shouldBe` 0  -- focusIdx=1, n=2 → 0

  describe "focusPrevSt" $ do
    it "decrements focusIdx from 1 to 0" $
      _focusIdx (focusPrevSt (addTapeToSt st40)) `shouldBe` 0
    it "wraps focusIdx from 0 to last" $
      _focusIdx (focusPrevSt (addTapeToSt st40){ _focusIdx = 0}) `shouldBe` 1

  describe "modifyFocusedTapeSt" $ do
    it "only modifies the tape at focusIdx" $ do
      let st2  = addTapeToSt st40  -- focusIdx=1
          st2' = modifyFocusedTapeSt (\t -> Tape.insert t 'x') st2
      tapeText (_tapes st2' !! 0) `shouldBe` ""
      tapeText (_tapes st2' !! 1) `shouldBe` "x"

  describe "tickTimer" $ do
    it "does nothing when no timer" $
      _timerSecs (tickTimer stNoTimer) `shouldBe` Nothing
    it "decrements positive timer" $
      _timerSecs (tickTimer (stWithTimer 60)) `shouldBe` Just 59
    it "holds at zero (no underflow)" $
      _timerSecs (tickTimer (stWithTimer 0)) `shouldBe` Just 0

  describe "advanceReel" $ do
    it "increments reelRotation from 0 to 1" $
      _reelRotation (advanceReel initialState) `shouldBe` 1
    it "wraps reelRotation from 3 to 0" $
      _reelRotation (advanceReel (initialState { _reelRotation = 3 })) `shouldBe` 0

  where
    st40        = initialState { _termHeight = 40 }
    st5         = initialState { _termHeight = 5 }
    stWithTimer n = initialState { _timerSecs = Just n }
    stNoTimer   = initialState { _timerSecs = Nothing }
