{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Todo
        (main)
where

import Brick hiding (Context)
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center

import Control.Concurrent.Async
import Control.Monad.IO.Class

import Data.Functor
import Data.List hiding (unlines)
import qualified Data.Set as S
import Data.Text hiding (map, intersperse)
import Data.Text.IO hiding (putStrLn)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Text.Megaparsec (errorBundlePretty)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import qualified System.FSNotify as FS

import Data.Task

import Prelude hiding (readFile, writeFile, unlines)

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) = flip (.)

newtype Res = Res Text deriving stock (Eq, Ord, Show)

vpMain :: Res
vpMain = Res "All Tasks"

type W = Widget Res

data State = State FilePath [Task]

data Ev = TodoFileUpdated

app :: App State Ev Res
app =
  App { appDraw = fmap pure ui
      , appHandleEvent = event
      , appStartEvent = pure
      , appAttrMap =  const $ attrMap V.defAttr uiAttrs
      , appChooseCursor = neverShowCursor
      }


completedStyle :: AttrName
completedStyle = "completed"

projectStyle :: AttrName
projectStyle = "project"

contextStyle :: AttrName
contextStyle = "context"

uiAttrs :: [(AttrName, Attr)]
uiAttrs =
  [ (completedStyle, fg blue)
  , (projectStyle, black `on` blue)
  , (contextStyle, black `on` cyan)
  ]

ui :: State -> W
ui (State _ tsks) = taskListView vpMain tsks

event :: State -> BrickEvent Res Ev -> EventM Res (Next State)
-- Scrolling
event s (VtyEvent (V.EvKey V.KDown []))  = vscroll vpMain 1 >> continue s
event s (VtyEvent (V.EvKey V.KUp []))    = vscroll vpMain (-1) >> continue s
-- Quit
event s (VtyEvent (V.EvKey V.KEsc [])) = halt s
event s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
-- Saving
event s@(State fp tsks) (VtyEvent (V.EvKey (V.KChar 's') [])) = do
    liftIO $ writeFile fp (unlines $ map serialize tsks)
    continue s
-- File changed on disk
event (State fp tsks) (AppEvent TodoFileUpdated) = do
  t <- liftIO $ readFile fp
  let newTasks = either (const tsks) id (parseMany t)
  continue $ State fp newTasks
-- Default
event s _ = continue s

vscroll :: Res -> Int -> EventM Res ()
vscroll vp n = vScrollBy (viewportScroll vp) n

title :: Text -> W
title = txt |> hCenter |> (<=> hBorder)

taskView :: Task -> W
taskView t =
  vLimit 5 $
    descr <+> (padLeft Max $ ctxs <+> prjs)
  where
    ctxs = contextsView $ contexts t
    prjs = projectsView $ projects t
    descr = descrStyle . txt . descriptionText . description $ t
    descrStyle = if completed t then withAttr completedStyle else id

projectsView :: S.Set Project -> W
projectsView = S.toList |> map view |> map (padLeft $ Pad 1) |> hBox
  where
    view = projectText |> withSpace |> txt |> withAttr projectStyle

contextsView :: S.Set Context -> W
contextsView = S.toList |> map view |> map (padLeft $ Pad 1) |> hBox
  where
    view = contextText |> withSpace |> txt |> withAttr contextStyle

withSpace :: Text -> Text
withSpace t = " " <> t <> " "

taskListView :: Res -> [Task] -> W
taskListView r@(Res t) tsks =
  border $ title t <=> mainView
  where
    mainView =
      viewport r Vertical $
          vBox $ intersperse dashHBorder taskViews
    dashHBorder = withBorderStyle (borderStyleFromChar '-') hBorder
    taskViews = map taskView tsks

main :: IO ()
main = do
  fileName <- makeAbsolute =<< getFirstArg
  taskList <- parseOrDie fileName
  fileEvents <- newBChan 100
  uiThread <- async $ uiMain fileEvents (State fileName taskList)
  _ <- async $ fileWatcher fileName fileEvents uiThread
  void $ wait uiThread
  where
    getFirstArg = Prelude.head <$> getArgs  -- obviously unsafe, but will do for now
    uiMain evChan state = do
      let b = V.mkVty V.defaultConfig
      v <- b
      customMain v b (Just evChan) app state
    parseOrDie fileName = do
      tl <- readFile fileName
      case parseMany tl of
        Left e -> die $ errorBundlePretty e
        Right tsks -> pure tsks

fileWatcher :: FilePath -> BChan Ev -> Async a -> IO ()
fileWatcher f chan end = FS.withManager $ \m ->
  void $ FS.watchDir m dir fileChanged cb >> wait end
  where
    cb = const $ writeBChan chan TodoFileUpdated
    dir = takeDirectory f
    fileChanged (FS.Modified ef _ _) = (f == ef)
    fileChanged _ = False
