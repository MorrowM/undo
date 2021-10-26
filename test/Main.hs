module Main where

import           Control.Undo.Pure
import           Data.Maybe
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Undo" $ do
    let u = singleton 'a'
    it "the current state should be the initial state before any pushes" $
      current u `shouldBe` 'a'
    it "pushing adds a new state" $
      current (push 'b' u) `shouldBe` 'b'
    it "undoing after pushing should reveal the original state" $
      current <$> undo (push 'b' u) `shouldBe` Just 'a'
    it "undo'ing after pushing should reveal the original state" $
      current (undo' $ push 'b' u) `shouldBe` 'a'
    it "history should show all past states in reverse chronological order" $
      history (push 'd' . push 'c' . push 'b' $ u) `shouldBe` "cba"
    it "redo should navigate to a future state" $
      current (redo' 1 . undo' . push 'c' . undo' . push 'b' $ u) `shouldBe` 'b'
    it "redoBy should navigate using a predicate" $
      current (redoBy' (=='c') . undo' . push 'c' . undo' . push 'b' $ u) `shouldBe` 'c'
