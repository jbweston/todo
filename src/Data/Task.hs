module Data.Task
  ()
where

import Data.Maybe
import Data.Text
import Data.Time (Day)
import Data.Word (Word)

newtype TaskPriority = TaskPriority Word

newtype Tag = Tag Text

data Task = Task {
    completed :: Bool
  , priority :: TaskPriority
  , completion_date :: Maybe Day
  , creation_date :: Maybe Day
  , description :: Text
  , projects :: [Tag]
  , contexts :: [Tag]
  , due_date :: Maybe Day
  , tags :: [(Tag, Tag)]
}
