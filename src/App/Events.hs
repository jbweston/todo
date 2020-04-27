{-# LANGUAGE OverloadedStrings #-}

module App.Events (event) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (unlines)
import Data.Text.IO (readFile, writeFile)
import Data.Vector (fromList)
import Flow
import Prelude hiding (readFile, writeFile, unlines)

import Graphics.Vty.Input
import Brick (vScrollBy, viewportScroll, continue, halt)
import Brick.Types (BrickEvent(..), EventM, Next)
import Brick.Widgets.List
  ( handleListEvent
  , listModify
  , listReplace
  , listSelectedElement
  )

import App.Resources
import App.Types

import Data.Task

event :: State -> BrickEvent Res Ev -> EventM Res (Next State)
-- Quit
event s (VtyEvent (EvKey KEsc [])) = halt s
event s (VtyEvent (EvKey (KChar 'q') [])) = halt s
-- File changed on disk
event (State fp tsks) (AppEvent TodoFileUpdated) = do
  t <- liftIO $ readFile fp
  let selectedId = fst <$> listSelectedElement tsks
      newTasks = either (const tsks) (\l -> listReplace (fromList l) (selectedId) tsks) (parseMany t)
  continue $ State fp newTasks
-- Task completion
event (State fp tsks) (VtyEvent (EvKey (KChar 'c') [])) = do
  now <- liftIO $ today
  continue $ State fp (listModify (complete now) tsks)
-- Scrolling
event s (VtyEvent ev) = do
  newTasks <- handleListEvent ev (sTasks s)
  continue s{sTasks=newTasks}
-- Default
event s _ = continue s

vscroll :: Res -> Int -> EventM Res ()
vscroll vp n = vScrollBy (viewportScroll vp) n

