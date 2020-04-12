{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Control.Lens
import Data.Char (isSpace)
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time (Day)
import Data.Time.LocalTime (getZonedTime, ZonedTime(..), LocalTime(..))

import Control.Applicative

-- Datatypes

newtype Priority = Priority Char deriving Eq
newtype Tag = Tag Text deriving Eq
newtype TagType = TagType Text deriving Eq
newtype Description = Description Text deriving Eq
data Task = Task {
    _completed :: Bool
  , _priority :: Maybe Priority
  , _completionDate :: Maybe Day
  , _creationDate :: Maybe Day
  , _description :: Description
  , _projects :: [Tag]
  , _contexts :: [Tag]
  , _tags :: [Tag]
  , _dueDate :: Maybe Day
  , _extraTags :: [(TagType, Tag)]
} deriving Eq

makeLenses ''Task

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
    c <- today
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

today :: IO Day
today = localDay . zonedTimeToLocalTime <$> getZonedTime
