{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Todo
        (main)
where

import Brick hiding (Context)
import Brick.BChan

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Monad (forever)

import Data.Functor
import Data.Text.IO hiding (putStrLn)
import qualified Graphics.Vty as V
import Text.Megaparsec hiding (State)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import qualified System.FSNotify as FS

import Data.Task

import App.Events
import App.Resources
import App.Types
import App.Views

import Prelude hiding (readFile, writeFile, unlines)

app :: App State Ev Res
app =
  App { appDraw = fmap pure ui
      , appHandleEvent = event
      , appStartEvent = pure
      , appAttrMap =  const $ attrMap V.defAttr uiAttrs
      , appChooseCursor = neverShowCursor
      }

ui :: State -> W
ui (State _ tsks) = taskListView vpMain tsks

main :: IO ()
main = do
  fileName <- makeAbsolute =<< getFirstArg
  taskList <- parseOrDie fileName
  fileEvents <- newBChan 100
  uiThread <- async $ uiMain fileEvents (State fileName taskList)
  fwThread <- async $ fileWatcher fileName fileEvents
  void $ wait uiThread
  cancel fwThread
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

fileWatcher :: FilePath -> BChan Ev -> IO ()
fileWatcher f chan = FS.withManager $ \m ->
  void $ FS.watchDir m dir fileChanged cb >> spin
  where
    spin = forever $ threadDelay maxBound
    cb = const $ writeBChan chan TodoFileUpdated
    dir = takeDirectory f
    fileChanged (FS.Modified ef _ _) = (f == ef)
    fileChanged _ = False
