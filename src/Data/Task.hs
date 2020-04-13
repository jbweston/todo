{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Task
  -- Types
  ( Task
  , Priority
  , Project
  , Context
  , Tag
  , TagType
  , Description
  -- Accessors
  , completed
  , priority
  , completionDate
  , creationDate
  , description
  , projects
  , contexts
  , tags
  , dueDate
  -- Smart constructors
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

import Prelude

import Control.Applicative (liftA2)
import Data.Char (isSpace)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time (Day)
import Data.Time.LocalTime (getZonedTime, ZonedTime(..), LocalTime(..))

-- Datatypes

newtype Priority = Priority Char deriving stock (Eq, Ord)
newtype Project = Project Text deriving stock (Eq, Ord)
newtype Context = Context Text deriving stock (Eq, Ord)
newtype Tag = Tag Text deriving stock (Eq, Ord)
newtype TagType = TagType Text deriving stock (Eq, Ord)
newtype Description = Description Text deriving stock (Eq)
data Task = Task {
    completed :: Bool
  , priority :: Maybe Priority
  , completionDate :: Maybe Day
  , creationDate :: Maybe Day
  , description :: Description
  , projects :: Set Project
  , contexts :: Set Context
  , tags :: Map TagType Tag
  , dueDate :: Maybe Day
} deriving stock (Eq)

-- Smart constructors

makePriority :: Char -> Maybe Priority
makePriority = Priority <$$> require (between 'A' 'Z')

makeProject :: Text -> Maybe Project
makeProject = Project <$$> require (notEmpty .&&. noSpaces)

makeContext :: Text -> Maybe Context
makeContext = Context <$$> require (notEmpty .&&. noSpaces)

makeTagType :: Text -> Maybe TagType
makeTagType = TagType <$$> require (notEmpty .&&. noSpaces)

makeDescription :: Text -> Maybe Description
makeDescription = Description <$$> require (notEmpty .&&. oneLine)

makeTag :: Text -> Maybe Tag
makeTag = Tag <$$> require (notEmpty .&&. noSpaces)


-- | Make a new Task with the provided creation date and description
newTask :: Day -> Description -> Task
newTask c d = Task False Nothing Nothing (Just c) d S.empty S.empty M.empty Nothing

-- | Make a new Task with the provided description and today as the creation date
getNewTask :: Description -> IO Task
getNewTask d = newTask <$> today <*> pure d


-- Utilities

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(.&&.) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&.) = liftA2 (&&)

require :: (a -> Bool) -> a -> Maybe a
require f a = if f a then Just a else Nothing

between :: Ord a => a -> a -> a -> Bool
between a b x = x >= a && x <= b

notEmpty :: Text -> Bool
notEmpty = not . T.null

noSpaces :: Text -> Bool
noSpaces = not . T.any isSpace

oneLine :: Text -> Bool
oneLine = not . T.any (== '\n')

today :: IO Day
today = localDay . zonedTimeToLocalTime <$> getZonedTime
