{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import Test.Hspec.Hedgehog

--import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Following is only for example
-- | you must adapt it accordingly
main :: IO ()
main = hspec $
  describe "test common properties" $ do
    it "test property with hedgehog - property 1" $ hedgehog $ do
        success
