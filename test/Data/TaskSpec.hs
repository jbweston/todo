{-# LANGUAGE ScopedTypeVariables #-}

module Data.TaskSpec (spec) where

import Control.Applicative (liftA2)
import qualified Data.Char as C
import qualified Data.Maybe as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances.Text ()

import Data.Task

spec :: Spec
spec = do
  describe "makePriority" $ makePriority `onlyTakes` capitalLetters
  describe "makeTag" $ makeTag `onlyTakes` singleWords
  describe "makeTagType" $ makeTagType `onlyTakes` singleWords
  describe "makeProject" $ makeProject `onlyTakes` singleWords
  describe "makeContext" $ makeContext `onlyTakes` singleWords
  describe "makeDescription" $ makeDescription `onlyTakes` singleLines


-- Utilities

(.&&.) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&.) = liftA2 (&&)

onlyTakes f (p, pred) =
    prop ("Only takes " ++ p) $ \x ->
      let fx = f x in if pred x then M.isJust fx else M.isNothing fx

capitalLetters = ("capital letters", C.isAscii .&&. C.isUpper)
singleWords = ("single words", (not . T.null) .&&. (not . T.any C.isSpace))
singleLines = ("single lines", (not . T.null) .&&. (not . T.any (== '\n')))
