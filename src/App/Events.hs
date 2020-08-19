{-# LANGUAGE OverloadedStrings #-}

module App.Events (event) where

import Control.Monad.IO.Class (liftIO)
import Data.Text.IO (readFile)
import Data.Vector (fromList)
import Prelude hiding (readFile)

import Graphics.Vty.Input
import Brick (continue, halt)
import Brick.Types (BrickEvent(..), EventM, Next)
import Brick.Widgets.List
  ( handleListEvent
  , listModify
  , listReplace
  , listSelectedElement
  )

import App.Types

import Data.Task

-- Mapping events to actions

event :: BrickEvent Res Ev -> State -> EventM Res (Next State)
-- Quit
event (VtyEvent (EvKey KEsc [])) = halt
event (VtyEvent (EvKey (KChar 'q') [])) = halt
-- File changed on disk
event (AppEvent TodoFileUpdated) = fileLoad
-- Task completion
event (VtyEvent (EvKey (KChar ' ') []))= toggleCompletion
-- Scrolling
event (VtyEvent ev) = scroll ev
-- Default
event _ = continue


-- Actions

type Action = State -> EventM Res (Next State)

fileLoad :: Action
fileLoad (State fp tsks) = do
  t <- liftIO $ readFile fp
  let selectedId = fst <$> listSelectedElement tsks
      newTasks = case parseMany t of
          Left _ -> tsks
          Right l -> listReplace (fromList l) selectedId tsks
  continue $ State fp newTasks

toggleCompletion :: Action
toggleCompletion (State fp tsks) = do
  now <- liftIO $ today
  continue $ State fp (listModify (toggleComplete now) tsks)

scroll :: Event -> Action
scroll ev s = do
  newTasks <- handleListEvent ev (sTasks s)
  continue s{sTasks=newTasks}
