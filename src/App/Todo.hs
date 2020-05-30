{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Todo
        (main)
where

import Brick hiding (Context)
import Brick.Widgets.List (list)
import Brick.BChan

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Monad (forever)

import Data.Functor
import Data.Text.IO hiding (putStrLn)
import Data.Vector (fromList)
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
      , appHandleEvent = flip event
      , appStartEvent = pure
      , appAttrMap =  const $ attrMap V.defAttr uiAttrs
      , appChooseCursor = neverShowCursor
      }

ui :: State -> W
ui (State _ tasks) = taskListView vpMain tasks

initialState :: FilePath -> IO State
initialState fileName = do
  tl <- readFile fileName
  case parseMany tl of
    Left e -> die $ errorBundlePretty e
    Right tasks -> pure $ State fileName (list vpMain (fromList tasks) 3)

main :: IO ()
main = do
  fileName <- makeAbsolute =<< getFirstArg
  state <- initialState fileName
  fileEvents <- newBChan 100
  uiThread <- async $ uiMain fileEvents state
  fwThread <- async $ fileWatcher fileName fileEvents
  void $ wait uiThread
  cancel fwThread
  where
    getFirstArg = Prelude.head <$> getArgs  -- obviously unsafe, but will do for now
    uiMain evChan state = do
      let b = V.mkVty V.defaultConfig
      v <- b
      customMain v b (Just evChan) app state

fileWatcher :: FilePath -> BChan Ev -> IO ()
fileWatcher f chan = FS.withManager $ \m ->
  void $ FS.watchDir m dir fileChanged cb >> spin
  where
    spin = forever $ threadDelay maxBound
    cb = const $ writeBChan chan TodoFileUpdated
    dir = takeDirectory f
    fileChanged (FS.Modified ef _ _) = (f == ef)
    fileChanged _ = False
