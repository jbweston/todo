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
  -- Manipulators
  , complete
  , completeToday
  , setPriority
  , unsetPriority
  , setDescription
  , addProject
  , removeProject
  , addContext
  , removeContext
  , setDueDate
  , unsetDueDate
  , addTag
  , removeTag
  )
where

import Prelude

import Control.Applicative (liftA2)
import Data.Char (isSpace, isPrint)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time (Day)
import Data.Time.LocalTime (getZonedTime, ZonedTime(..), LocalTime(..))

-- Datatypes

newtype Priority = Priority Char deriving stock (Eq, Ord, Show)
newtype Project = Project Text deriving stock (Eq, Ord, Show)
newtype Context = Context Text deriving stock (Eq, Ord, Show)
newtype Tag = Tag Text deriving stock (Eq, Ord, Show)
newtype TagType = TagType Text deriving stock (Eq, Ord, Show)
newtype Description = Description Text deriving stock (Eq, Show)
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
} deriving stock (Eq, Show)

-- Smart constructors

makePriority :: Char -> Maybe Priority
makePriority = Priority <$$> require (between 'A' 'Z')

makeProject :: Text -> Maybe Project
makeProject = Project <$$> require (someText .&&. noSpaces)

makeContext :: Text -> Maybe Context
makeContext = Context <$$> require (someText .&&. noSpaces)

makeTagType :: Text -> Maybe TagType
makeTagType = TagType <$$> require (someText .&&. noSpaces)

makeDescription :: Text -> Maybe Description
makeDescription = Description <$$> require (someText .&&. oneLine)

makeTag :: Text -> Maybe Tag
makeTag = Tag <$$> require (someText .&&. noSpaces)


-- | Make a new Task with the provided creation date and description
newTask :: Day -> Description -> Task
newTask c d = Task False Nothing Nothing (Just c) d S.empty S.empty M.empty Nothing

-- | Make a new Task with the provided description and today as the creation date
getNewTask :: Description -> IO Task
getNewTask d = newTask <$> today <*> pure d


-- Functions to manipulate tasks
-- These could be simplified with Lenses if they get more complicated

complete :: Day -> Task -> Task
complete d tsk =
    case creationDate tsk of
      Nothing -> tsk{completed=True}
      _ -> tsk{completed=True, completionDate=Just d}

completeToday :: Task -> IO Task
completeToday tsk = complete <$> today <*> pure tsk

setPriority :: Priority -> Task -> Task
setPriority p tsk = tsk{priority=Just p}

unsetPriority :: Task -> Task
unsetPriority tsk = tsk{priority=Nothing}

setDescription :: Description -> Task -> Task
setDescription d tsk = tsk{description=d}

addProject :: Project -> Task -> Task
addProject p tsk = tsk{projects=S.insert p (projects tsk)}

removeProject :: Project -> Task -> Task
removeProject p tsk = tsk{projects=S.delete p (projects tsk)}

addContext :: Context -> Task -> Task
addContext c tsk = tsk{contexts=S.insert c (contexts tsk)}

removeContext :: Context -> Task -> Task
removeContext c tsk = tsk{contexts=S.delete c (contexts tsk)}

setDueDate :: Day -> Task -> Task
setDueDate d tsk = tsk{dueDate=Just d}

unsetDueDate :: Task -> Task
unsetDueDate tsk = tsk{dueDate=Nothing}

addTag :: (TagType, Tag) -> Task -> Task
addTag (tt, t) tsk = tsk{tags=M.insert tt t (tags tsk)}

removeTag :: TagType -> Task -> Task
removeTag tt tsk = tsk{tags=M.delete tt (tags tsk)}


-- Utilities

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(.&&.) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&.) = liftA2 (&&)

require :: (a -> Bool) -> a -> Maybe a
require f a = if f a then Just a else Nothing

between :: Ord a => a -> a -> a -> Bool
between a b x = x >= a && x <= b

someText :: Text -> Bool
someText = (not . T.null) .&&. T.all isPrint

noSpaces :: Text -> Bool
noSpaces = not . T.any isSpace

oneLine :: Text -> Bool
oneLine = not . T.any (== '\n')

today :: IO Day
today = localDay . zonedTimeToLocalTime <$> getZonedTime
