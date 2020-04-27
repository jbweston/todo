{-# LANGUAGE DerivingStrategies #-}
module App.Types
( Res(..)
, W
, State(..)
, Ev(..)
)
where

import Brick.Types (Widget)
import Data.Task (Task)
import Data.Text (Text)

import Prelude

-- | Resources indexed by text
newtype Res = Res Text deriving stock (Eq, Ord, Show)

-- | Widgets
type W = Widget Res

-- | App state

data State = State { sTodoFile :: FilePath
                   , sTasks    :: [Task]
                   , sRow      :: Int
                   }

-- | Custom events
data Ev = TodoFileUpdated
