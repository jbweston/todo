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

import Prelude (FilePath, Eq, Ord, Show)

-- | Resources indexed by text
newtype Res = Res Text deriving stock (Eq, Ord, Show)

-- | Widgets
type W = Widget Res

-- | App state
data State = State FilePath [Task]

-- | Custom events
data Ev = TodoFileUpdated
