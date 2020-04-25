{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Todo
        (main)
where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center

import Control.Concurrent.Async

import Data.Functor
import Data.List
import Data.Text hiding (map, intersperse)
import Data.Text.IO hiding (putStrLn)
import qualified Graphics.Vty as V
import Text.Megaparsec (errorBundlePretty)
import System.Environment
import System.Exit

import Data.Task

import Prelude hiding (readFile)

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) = flip (.)

newtype Res = Res Text deriving stock (Eq, Ord, Show)

vpMain :: Res
vpMain = Res "All Tasks"

type W = Widget Res

data State = State [Task]

app :: App State e Res
app =
  App { appDraw = fmap pure ui
      , appHandleEvent = event
      , appStartEvent = pure
      , appAttrMap =  const $ attrMap V.defAttr []
      , appChooseCursor = neverShowCursor
      }

ui :: State -> W
ui (State tsks) = taskListView vpMain tsks

event :: State -> BrickEvent Res e -> EventM Res (Next State)
event s (VtyEvent (V.EvKey V.KDown []))  = vscroll vpMain 1 >> continue s
event s (VtyEvent (V.EvKey V.KUp []))    = vscroll vpMain (-1) >> continue s
event s (VtyEvent (V.EvKey V.KEsc [])) = halt s
event s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
event s _ = continue s

vscroll :: Res -> Int -> EventM Res ()
vscroll vp n = vScrollBy (viewportScroll vp) n

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
  fileName <- Prelude.head <$> getArgs  --obviously unsafe, but it will do for now
  taskList <- parseOrDie fileName
  fileEvents <- newBChan 100
  uiThread <- async $ uiMain fileEvents (State taskList)
  void $ wait uiThread
  where
    uiMain evChan state = do
      let b = V.mkVty V.defaultConfig
      v <- b
      customMain v b (Just evChan) app state
    parseOrDie fileName = do
      tl <- readFile fileName
      case parseMany tl of
        Left e -> die $ errorBundlePretty e
        Right tsks -> pure tsks
