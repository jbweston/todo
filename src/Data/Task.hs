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
  , makeProject
  , makeContext
  , makeDescription
  , newTask
  , getNewTask
  )
where

import Data.Char (isSpace)
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time (Day)
import Data.Time.LocalTime (getZonedTime, ZonedTime(..), LocalTime(..))

import Control.Applicative

-- Datatypes

newtype Priority = Priority Char deriving (Eq)
newtype Project = Project Text deriving (Eq)
newtype Context = Context Text deriving (Eq)
newtype Tag = Tag Text deriving (Eq)
newtype TagType = TagType Text deriving (Eq)
newtype Description = Description Text deriving (Eq)
data Task = Task {
    completed :: Bool
  , priority :: Maybe Priority
  , completionDate :: Maybe Day
  , creationDate :: Maybe Day
  , description :: Description
  , projects :: Set Project
  , contexts :: Set Context
  , tags :: Set Tag
  , dueDate :: Maybe Day
  , extraTags :: Map TagType Tag
} deriving (Eq)

-- Smart constructors

makePriority :: Char -> Maybe Priority
makePriority = Priority <$$> require (between 'A' 'Z')

makeTag :: Text -> Maybe Tag
makeTag = Tag <$$> require noSpaces

makeProject :: Text -> Maybe Project
makeProject = Project <$$> require noSpaces

makeContext :: Text -> Maybe Context
makeContext = Context <$$> require noSpaces

makeTagType :: Text -> Maybe TagType
makeTagType = TagType <$$> require noSpaces

makeDescription :: Text -> Maybe Description
makeDescription = Description <$$> require oneLine

-- | Make a new Task with the provided creation date and description
newTask :: Day -> Description -> Task
newTask c d = Task False Nothing Nothing (Just c) d S.empty S.empty S.empty Nothing M.empty

-- | Make a new Task with the provided description and today as the creation date
getNewTask :: Description -> IO Task
getNewTask d = newTask <$> today <*> pure d


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
