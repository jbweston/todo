{-# LANGUAGE DerivingStrategies #-}
module App.Types
( Res(..)
, W
, State(..)
, Ev(..)
)
where

import Brick.Types
import Data.Task
import Data.Text (Text)

-- | Resources indexed by text
newtype Res = Res Text deriving stock (Eq, Ord, Show)

-- | Widgets
type W = Widget Res

-- | App state
data State = State FilePath [Task]

-- | Custom events
data Ev = TodoFileUpdated
