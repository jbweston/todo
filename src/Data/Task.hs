{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

module Data.Task
  ( -- Types
    Task,
    Priority,
    Project,
    Context,
    Tag,
    TagType,
    Description,
    Parser,
    -- Accessors
    completed,
    priority,
    completionDate,
    creationDate,
    description,
    projects,
    contexts,
    tags,
    dueDate,
    -- Accessors that unwrap newtypes
    priorityChar,
    descriptionText,
    projectText,
    contextText,
    tagText,
    tagTypeText,
    -- serializers and parsers
    serialize,
    parse,
    parseMany,
    parser,
    -- Smart constructors
    makePriority,
    makeTag,
    makeTagType,
    makeProject,
    makeContext,
    makeDescription,
    newTask,
    getNewTask,
    today,
    -- Manipulators
    complete,
    completeToday,
    setPriority,
    unsetPriority,
    setDescription,
    addProject,
    removeProject,
    addContext,
    removeContext,
    setDueDate,
    unsetDueDate,
    addTag,
    removeTag,
  )
where

import Control.Applicative (liftA2)
import Control.Lens (over, set)
import Control.Lens.Tuple
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char (isPrint, isSpace)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (LocalTime (..), ZonedTime (..), getZonedTime)
import Data.Void
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Prelude

-- Datatypes

newtype Priority = Priority { priorityChar :: Char } deriving stock (Eq, Ord, Show)

newtype Project = Project { projectText :: Text } deriving stock (Eq, Ord, Show)

newtype Context = Context { contextText :: Text } deriving stock (Eq, Ord, Show)

newtype Tag = Tag { tagText :: Text } deriving stock (Eq, Ord, Show)

newtype TagType = TagType { tagTypeText :: Text } deriving stock (Eq, Ord, Show)

newtype Description = Description { descriptionText :: Text } deriving stock (Eq, Show)

data Task
  = Task
      { completed :: Bool,
        priority :: Maybe Priority,
        completionDate :: Maybe Day,
        creationDate :: Maybe Day,
        description :: Description,
        projects :: Set Project,
        contexts :: Set Context,
        tags :: Map TagType Tag,
        dueDate :: Maybe Day
      }
  deriving stock (Eq, Show)

serialize :: Task -> Text
serialize task =
  let textDate = T.pack . iso8601Show
      items = M.foldrWithKey (\k v it -> (k, v) : it) []
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
        -- Due date
        maybeTell dueDate (("due:" <>) . textDate)
        -- Tags
        forM_ (items $ tags task) $ \(TagType tt, Tag tg) ->
          tell [tt <> ":" <> tg]
   in T.intercalate " " (execWriter serializer)

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void

dateP :: Parser Day
dateP = do
  year <- decimal <* single '-'
  month <- decimal <* single '-'
  day <- decimal
  pure $ fromGregorian year month day

-- TODO: clean this up; this function is doing a lot
--       We can probably replace this with a word parser
parseDescriptionWords ::
  [Text] -> ([Text], S.Set Project, S.Set Context, M.Map TagType Tag, Maybe Day)
parseDescriptionWords = foldr select ([], S.empty, S.empty, M.empty, Nothing)
  where
    select word =
      if T.length word == 1
        then over _1 (word :)
        else case T.uncons word of
          Just ('+', p) -> over _2 (S.insert (Project p))
          Just ('@', c) -> over _3 (S.insert (Context c))
          _ -> case T.breakOn ":" word of
            -- 'word' is not empty, so this can never happen
            ("", "") -> undefined
            -- No ':' found; word is just a regular word
            (_, "") -> over _1 (word :)
            --
            (_, ":") -> over _1 (word :)
            -- ':' occurs at start
            ("", _) -> over _1 (word :)
            -- due date specifed
            ("due", date) -> case parseMaybe dateP (T.tail date) of
              -- If it's not a date treat it like a regular tag
              Nothing -> over _4 $ M.insert (TagType "due") (Tag $ T.tail date)
              -- If it's a date, set the due date
              jd -> set _5 jd
            -- any other tag
            (tt, tg) -> over _4 $ M.insert (TagType tt) (Tag $ T.tail tg)

-- TODO: Make this parser more robust so that it can handle tasks
--       not produced with 'serialize'
parser :: Parser Task
parser = do
  completed <- isJust <$> (optional . try $ single 'x' <* someSpace)
  priority <- Priority <$$> (optional . try $ (inParens . satisfy $ isUpperAlpha) <* someSpace)
  dates <- try twoDates <|> try oneDate <|> pure (Nothing, Nothing)
  let (creationDate, completionDate) = case dates of
        (d1, Nothing) -> (d1, Nothing)
        (d1, d2) -> (d2, d1)
  (descr, projects, contexts, tags, dueDate) <-
    parseDescriptionWords . T.words <$> takeWhile1P Nothing ((/= '\r') .&&. (/= '\n'))
  let description = Description $ T.unwords descr
  pure $ Task {..}
  where
    inParens :: Parser a -> Parser a
    inParens p = single '(' *> p <* single ')'
    someSpace :: Parser ()
    someSpace = void $ takeWhile1P (Just "white space") isLineSpace
    twoDates = (,) <$> (Just <$> dateP <* someSpace) <*> (Just <$> dateP <* someSpace)
    oneDate  = (, Nothing) <$> (Just <$> dateP <* someSpace)
    -- We don't want vertical whitespace or carriage returns
    isLineSpace = isSpace .&&. (isPrint .||. (== '\t'))

parser' :: Parser [Task]
parser' = parser `sepEndBy` eol

parse :: Text -> Either ParserError Task
parse = runParser parser ""

parseMany :: Text -> Either ParserError [Task]
parseMany = runParser parser' ""

-- Smart constructors

makePriority :: Char -> Maybe Priority
makePriority = Priority <$$> require isUpperAlpha

makeProject :: Text -> Maybe Project
makeProject = Project <$$> require oneWord

makeContext :: Text -> Maybe Context
makeContext = Context <$$> require oneWord

makeTagType :: Text -> Maybe TagType
makeTagType = TagType <$$> require oneWord

makeDescription :: Text -> Maybe Description
makeDescription = (Description . normalizeWhitespace) <$$> require oneLine

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
    Nothing -> tsk {completed = True}
    _ -> tsk {completed = True, completionDate = Just d}

completeToday :: Task -> IO Task
completeToday tsk = complete <$> today <*> pure tsk

setPriority :: Priority -> Task -> Task
setPriority p tsk = tsk {priority = Just p}

unsetPriority :: Task -> Task
unsetPriority tsk = tsk {priority = Nothing}

setDescription :: Description -> Task -> Task
setDescription d tsk = tsk {description = d}

addProject :: Project -> Task -> Task
addProject p tsk = tsk {projects = S.insert p (projects tsk)}

removeProject :: Project -> Task -> Task
removeProject p tsk = tsk {projects = S.delete p (projects tsk)}

addContext :: Context -> Task -> Task
addContext c tsk = tsk {contexts = S.insert c (contexts tsk)}

removeContext :: Context -> Task -> Task
removeContext c tsk = tsk {contexts = S.delete c (contexts tsk)}

setDueDate :: Day -> Task -> Task
setDueDate d tsk = tsk {dueDate = Just d}

unsetDueDate :: Task -> Task
unsetDueDate tsk = tsk {dueDate = Nothing}

addTag :: (TagType, Tag) -> Task -> Task
addTag (tt, t) tsk = tsk {tags = M.insert tt t (tags tsk)}

removeTag :: TagType -> Task -> Task
removeTag tt tsk = tsk {tags = M.delete tt (tags tsk)}

-- Utilities

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(.&&.) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&.) = liftA2 (&&)

(.||.) :: Applicative f => f Bool -> f Bool -> f Bool
(.||.) = liftA2 (||)

require :: (a -> Bool) -> a -> Maybe a
require f a = if f a then Just a else Nothing

isUpperAlpha :: Char -> Bool
isUpperAlpha = ('A' <=) .&&. (<= 'Z')

oneWord :: Text -> Bool
oneWord = (not . T.null) .&&. T.all (isPrint .&&. (not . isSpace))

oneLine :: Text -> Bool
oneLine = (not . T.null) .&&. T.all (printable .&&. (/= '\n'))
  where
    printable = isPrint .||. (== '\t')

normalizeWhitespace :: Text -> Text
normalizeWhitespace = T.unwords . T.words

today :: IO Day
today = localDay . zonedTimeToLocalTime <$> getZonedTime
