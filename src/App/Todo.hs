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
import Data.Text.IO hiding (putStrLn)
import qualified Graphics.Vty as V
import Text.Megaparsec (errorBundlePretty)
import System.Environment

import Data.Task

import Prelude hiding (readFile)

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) = flip (.)

newtype Res = Res Text deriving stock (Eq, Ord, Show)

type W = Widget Res

data State = State [Task]

app :: App State e Res
app =
  App { appDraw = fmap pure ui
      , appHandleEvent = resizeOrQuit
      , appStartEvent = pure
      , appAttrMap =  const $ attrMap V.defAttr []
      , appChooseCursor = neverShowCursor
      }

ui :: State -> W
ui (State tsks) = taskListView (Res "All Tasks") tsks

title :: Text -> W
title = txt |> hCenter |> (<=> hBorder)

taskView :: Task -> W
taskView = serialize |> txt |> hCenter

taskListView :: Res -> [Task] -> W
taskListView r@(Res t) tsks =
  border $ title t <=> mainView
  where
    mainView =
      viewport r Vertical $
        withBorderStyle (borderStyleFromChar '-') $
          vBox $ intersperse hBorder taskViews
    taskViews = map taskView tsks

main :: IO ()
main = do
  taskList <- readFile =<< (Prelude.head <$> getArgs)  -- obviously unsafe, but that's the point
  case parseMany taskList of
    Left e -> putStrLn $ errorBundlePretty e
    Right tsks -> void $ defaultMain app (State tsks)
