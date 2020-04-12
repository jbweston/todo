{-# LANGUAGE ScopedTypeVariables #-}

module Data.TaskSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text ()

import Data.Task

spec :: Spec
spec = do
  describe "makePriority" $
    prop "Only takes capital letters" $
      \(p :: Char) -> False
  describe "makeTag" $
    prop "Only takes words" $
      \(p :: Text) -> False
  describe "makeTagType" $
    prop "Only takes words" $
      \(p :: Text) -> False
  describe "makeDescription" $
    prop "Only takes single lines" $
      \(p :: Text) -> False
