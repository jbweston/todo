{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Todo
        (main)
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center

import Data.Functor
import Data.List
import Data.Text hiding (map, intersperse)
import qualified Graphics.Vty as V

import Prelude

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) = flip (.)

newtype Res = Res Text deriving stock (Eq, Ord, Show)

type W = Widget Res

type Task = Text

data State = State ()

app :: App State e Res
app =
  App { appDraw = const [ui]
      , appHandleEvent = resizeOrQuit
      , appStartEvent = pure
      , appAttrMap =  const $ attrMap V.defAttr []
      , appChooseCursor = neverShowCursor
      }

initialState :: State
initialState = State ()

ui :: W
ui = taskListView (Res "main") taskList

taskList :: [Task]
taskList =
  [ "one"
  , "two"
  , "three"
  ]

title :: Text -> W
title = txt |> hCenter |> (<=> hBorder)

taskView :: Task -> W
taskView = txt |> hCenter

taskListView :: Res -> [Text] -> W
taskListView r@(Res t) tasks =
  border $ title t <=> mainView
  where
    mainView =
      viewport r Vertical $
        withBorderStyle (borderStyleFromChar '-') $
          vBox $ intersperse hBorder taskViews
    taskViews = map taskView tasks

main :: IO ()
main = void $ defaultMain app initialState
