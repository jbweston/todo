{-# LANGUAGE ScopedTypeVariables #-}

module Data.TaskSpec (spec) where

import Control.Applicative (liftA2)
import qualified Data.Char as C
import qualified Data.Maybe as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Gen
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
  describe "complete" $ idempotent2 complete
  describe "setPriority" $ idempotent2 setPriority
  describe "unsetPriority" $ idempotent unsetPriority
  describe "setDescription" $ idempotent2 setDescription
  describe "addProject" $ idempotent2 addProject
  describe "removeProject" $ idempotent2 removeProject
  describe "addContext" $ idempotent2 addContext
  describe "removeContext" $ idempotent2 removeContext
  describe "setDueDate" $ idempotent2 setDueDate
  describe "unsetDueDate" $ idempotent unsetDueDate
  describe "addTag" $ idempotent2 addTag
  describe "removeTag" $ idempotent2 removeTag


-- Arbitrary instances

instance Q.Arbitrary Priority where
    arbitrary = Q.choose ('A', 'Z') `suchThatMap` makePriority

instance Q.Arbitrary Project where
    arbitrary = Q.arbitrary `suchThatMap` makeProject

instance Q.Arbitrary Context where
    arbitrary = Q.arbitrary `suchThatMap` makeContext

instance Q.Arbitrary TagType where
    arbitrary = Q.arbitrary `suchThatMap` makeTagType

instance Q.Arbitrary Tag where
    arbitrary = Q.arbitrary `suchThatMap` makeTag

instance Q.Arbitrary Description where
    arbitrary = Q.arbitrary `suchThatMap` makeDescription

instance Q.Arbitrary Task where
    arbitrary =
        newTask <$> Q.arbitrary <*> Q.arbitrary
        >>= maybeApply complete
        >>= maybeApply setPriority
        >>= applyMany addProject
        >>= applyMany addContext
        >>= applyMany addTag
        >>= maybeApply setDueDate

-- Utilities

idempotent f = prop "Is idempotent" $ \x -> f x == (f . f) x

idempotent2 f = prop "Is idempotent" $ \x y -> f x y == (f x . f x) y

(.&&.) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&.) = liftA2 (&&)

(.||.) :: Applicative f => f Bool -> f Bool -> f Bool
(.||.) = liftA2 (||)

maybeApply :: Q.Arbitrary a => (a -> b -> b) -> b -> Q.Gen b
maybeApply setter task = do
    should <- Q.arbitrary
    if should
        then setter <$> Q.arbitrary <*> pure task
        else pure task

applyMany :: forall a b . Q.Arbitrary a => (a -> b -> b) -> b -> Q.Gen b
applyMany adder task = foldr adder task <$> arb
  where
    arb :: Q.Gen [a]
    arb = do
        n <- min 10 <$> getSize
        resize n Q.arbitrary

onlyTakes f (p, pred) =
    prop ("Only takes " ++ p) $ \x ->
      let fx = f x in if pred x then M.isJust fx else M.isNothing fx

someText = (not . T.null) .&&. T.all (C.isPrint .||. (== '\t'))
capitalLetters = ("capital letters", C.isAscii .&&. C.isUpper)
singleWords = ("single words", someText .&&. (not . T.any C.isSpace))
singleLines = ("single lines", someText .&&. (not . T.any (== '\n')))
