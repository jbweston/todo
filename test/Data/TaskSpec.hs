{-# LANGUAGE ScopedTypeVariables #-}

module Data.TaskSpec (spec) where

import Control.Applicative (liftA2)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Utf8 as U
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
  describe "complete" $ isIdempotent2 complete
  describe "setPriority" $ isIdempotent2 setPriority
  describe "unsetPriority" $ isIdempotent unsetPriority
  describe "setDescription" $ isIdempotent2 setDescription
  describe "addProject" $ isIdempotent2 addProject
  describe "removeProject" $ isIdempotent2 removeProject
  describe "addContext" $ isIdempotent2 addContext
  describe "removeContext" $ isIdempotent2 removeContext
  describe "setDueDate" $ isIdempotent2 setDueDate
  describe "unsetDueDate" $ isIdempotent unsetDueDate
  describe "addTag" $ isIdempotent2 addTag
  describe "removeTag" $ isIdempotent2 removeTag


-- Arbitrary instances

arbitraryWord :: Gen Text
arbitraryWord = T.pack <$> listOf1 wordChar
  where wordChar = U.genChar `suchThat` (C.isPrint .&&. (not . C.isSpace))

arbitraryWhitespace :: Gen Text
arbitraryWhitespace = elements $ map (T.singleton . C.chr) unicodeSpaces
    where
      -- https://en.wikipedia.org/wiki/Template:Whitespace_(Unicode)
      unicodeSpaces =
        [
            9, 32, 160, 5760, 8192, 8193, 8194, 8195, 8196,
            8197, 8198, 8199, 8200, 8201, 8202, 8239, 8287, 12288
        ]

arbitraryLine :: Gen Text
arbitraryLine = do
    n <- getSize
    k <- choose (1, n)
    let words = take k $ repeat arbitraryWord
        line = L.intersperse arbitraryWhitespace words
    T.concat <$> sequenceA line

instance Q.Arbitrary Priority where
    arbitrary = Q.choose ('A', 'Z') `suchThatMap` makePriority

instance Q.Arbitrary Project where
    arbitrary = arbitraryWord `suchThatMap` makeProject

instance Q.Arbitrary Context where
    arbitrary = arbitraryWord `suchThatMap` makeContext

instance Q.Arbitrary TagType where
    arbitrary = arbitraryWord `suchThatMap` makeTagType

instance Q.Arbitrary Tag where
    arbitrary = arbitraryWord `suchThatMap` makeTag

instance Q.Arbitrary Description where
    arbitrary = arbitraryLine `suchThatMap` makeDescription

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

isIdempotent f = prop "Is idempotent" $ \x -> f x == (f . f) x

isIdempotent2 f = prop "Is idempotent" $ \x y -> f x y == (f x . f x) y

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
