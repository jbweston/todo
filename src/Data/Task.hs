{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
  -- serializers and parsers
  , serialize
  , parse
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
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char (isSpace, isPrint)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601Show)
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


-- ReaderT Task (WriterT Text Identity) ()
serialize :: Task -> Text
serialize task = let
    textDate = T.pack . iso8601Show
    items = M.foldrWithKey (\k v it -> (k,v):it) []
    maybeTell attr f =
        tell $ case attr task of
            Nothing -> []
            Just x -> [f x]
    serializer :: Writer [Text] ()
    serializer = do
        tell $ if completed task then ["x"] else []
        maybeTell priority $ \(Priority p) -> "(" <> T.singleton p <> ")"
        maybeTell completionDate textDate
        maybeTell creationDate textDate
        -- Description
        let Description descr = description task
        tell [descr]
        -- Projects
        forM_ (projects task) $ \(Project p) ->
            tell ["+" <> p]
        -- Contexts
        forM_ (contexts task) $ \(Context ct) ->
            tell ["@" <> ct]
        -- Tags
        forM_ (items $ tags task) $ \(TagType tt, Tag tg) ->
            tell [tt <> ":" <> tg]
    in
    T.intercalate " " (execWriter serializer)

parse :: Text -> Maybe Task
parse = undefined

-- Smart constructors

makePriority :: Char -> Maybe Priority
makePriority = Priority <$$> require (between 'A' 'Z')

makeProject :: Text -> Maybe Project
makeProject = Project <$$> require oneWord

makeContext :: Text -> Maybe Context
makeContext = Context <$$> require oneWord

makeTagType :: Text -> Maybe TagType
makeTagType = TagType <$$> require oneWord

makeDescription :: Text -> Maybe Description
makeDescription = Description <$$> require oneLine

makeTag :: Text -> Maybe Tag
makeTag = Tag <$$> require oneWord

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

(.||.) :: Applicative f => f Bool -> f Bool -> f Bool
(.||.) = liftA2 (||)

require :: (a -> Bool) -> a -> Maybe a
require f a = if f a then Just a else Nothing

between :: Ord a => a -> a -> a -> Bool
between a b x = x >= a && x <= b


oneWord :: Text -> Bool
oneWord = (not . T.null) .&&. T.all (isPrint .&&. (not . isSpace))

oneLine :: Text -> Bool
oneLine = (not . T.null) .&&. T.all (printable .&&. (/= '\n'))
  where printable = isPrint .||. (== '\t')

today :: IO Day
today = localDay . zonedTimeToLocalTime <$> getZonedTime
