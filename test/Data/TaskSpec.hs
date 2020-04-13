{-# LANGUAGE ScopedTypeVariables #-}

module Data.TaskSpec (spec) where

import Control.Applicative (liftA2)
import qualified Data.Char as C
import qualified Data.Maybe as Mb
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()

import Data.Task

spec :: Spec
spec = do
  describe "makePriority" $ makePriority `onlyTakes` capitalLetters
  describe "makeTag" $ makeTag `onlyTakes` singleWords
  describe "makeTagType" $ makeTagType `onlyTakes` singleWords
  describe "makeProject" $ makeProject `onlyTakes` singleWords
  describe "makeContext" $ makeContext `onlyTakes` singleWords
  describe "makeDescription" $ makeDescription `onlyTakes` singleLines


-- Arbitrary instances

applyGen :: (a -> Maybe b) -> Q.Gen a -> Q.Gen b
applyGen transform g = do
    mb <- transform <$> g
    case mb of
        Just b -> pure b
        Nothing -> applyGen transform g

instance Q.Arbitrary Priority where
    arbitrary = applyGen makePriority $ Q.choose ('A', 'Z')

instance Q.Arbitrary Project where
    arbitrary = applyGen makeProject $ Q.arbitrary

instance Q.Arbitrary Context where
    arbitrary = applyGen makeContext $ Q.arbitrary

instance Q.Arbitrary TagType where
    arbitrary = applyGen makeTagType $ Q.arbitrary

instance Q.Arbitrary Tag where
    arbitrary = applyGen makeTag $ Q.arbitrary

instance Q.Arbitrary Description where
    arbitrary = applyGen makeDescription $ Q.arbitrary

instance Q.Arbitrary Task where
    arbitrary = do
        task <- newTask <$> Q.arbitrary <*> Q.arbitrary
        -- Maybe add a priority
        shouldSetPriority <- Q.arbitrary
        task <- if shouldSetPriority
          then setPriority <$> Q.arbitrary <*> pure task
          else pure task
        -- Add some projects
        task <- S.foldl (flip addProject) task <$> Q.arbitrary
        -- Add some contexts
        task <- S.foldl (flip addContext) task <$> Q.arbitrary
        -- Add some tags
        task <- S.foldl (\tsk (tt, t) -> addTag tt t tsk) task <$> Q.arbitrary
        -- Maybe set due date
        shouldSetDueDate <- Q.arbitrary
        task <- if shouldSetDueDate
          then setDueDate <$> Q.arbitrary <*> pure task
          else pure task
        pure task

-- Utilities

(.&&.) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&.) = liftA2 (&&)

onlyTakes f (p, pred) =
    prop ("Only takes " ++ p) $ \x ->
      let fx = f x in if pred x then Mb.isJust fx else Mb.isNothing fx

someText = (not . T.null) .&&. T.all C.isPrint
capitalLetters = ("capital letters", C.isAscii .&&. C.isUpper)
singleWords = ("single words", someText .&&. (not . T.any C.isSpace))
singleLines = ("single lines", someText .&&. (not . T.any (== '\n')))
