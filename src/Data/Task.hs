{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Task
  ( Task
  , Priority
  , Tag
  , TagType
  , Description
  , makePriority
  , makeTag
  , makeTagType
  , makeDescription
  , newTask
  , getNewTask
  )
where

import Data.Char (isSpace)
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time (Day)
import Data.Time.LocalTime (getZonedTime, ZonedTime(..), LocalTime(..))

import Control.Applicative

-- Datatypes

newtype Priority = Priority Char
newtype Tag = Tag Text
newtype TagType = TagType Text
newtype Description = Description Text
data Task = Task {
    completed :: Bool
  , priority :: Maybe Priority
  , completionDate :: Maybe Day
  , creationDate :: Maybe Day
  , description :: Description
  , projects :: [Tag]
  , contexts :: [Tag]
  , tags :: [Tag]
  , dueDate :: Maybe Day
  , extraTags :: [(TagType, Tag)]
}

-- Smart constructors

makePriority :: Char -> Maybe Priority
makePriority = Priority <$$> require (between 'A' 'Z')

makeTag :: Text -> Maybe Tag
makeTag = Tag <$$> require noSpaces

makeTagType :: Text -> Maybe TagType
makeTagType = TagType <$$> require noSpaces

makeDescription :: Text -> Maybe Description
makeDescription = Description <$$> require oneLine

-- | Make a new Task with the provided creation date and description
newTask :: Day -> Description -> Task
newTask c d = Task False Nothing Nothing (Just c) d [] [] [] Nothing []

-- | Make a new Task with the provided description and today as the creation date
getNewTask :: Description -> IO Task
getNewTask d = do
    c <- localDay . zonedTimeToLocalTime <$> getZonedTime
    pure $ newTask c d


-- Utilities

(.||.) = liftA2 (||)

(<$$>) = fmap . fmap

require :: (a -> Bool) -> a -> Maybe a
require pred a = if pred a then Just a else Nothing

between :: Ord a => a -> a -> a -> Bool
between a b x = x >= a && x <= b

noSpaces :: Text -> Bool
noSpaces = not . T.any isSpace

oneLine :: Text -> Bool
oneLine = not . T.any (== '\n')
