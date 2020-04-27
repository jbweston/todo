{-# LANGUAGE OverloadedStrings #-}

module App.Events (event) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (unlines)
import Data.Text.IO (readFile, writeFile)
import Prelude hiding (readFile, writeFile, unlines)

import Graphics.Vty.Input
import Brick (vScrollBy, viewportScroll, continue, halt)
import Brick.Types (BrickEvent(..), EventM, Next)

import App.Resources
import App.Types

import Data.Task

event :: State -> BrickEvent Res Ev -> EventM Res (Next State)
-- Scrolling
event s@(State _ tsks row) (VtyEvent (EvKey KDown [])) =
  continue newState
  where
    newState = s{sRow=min (row + 1) (length tsks)}
event s@(State _ tsks row) (VtyEvent (EvKey KUp [])) =
  continue newState
  where
    newState = s{sRow=max (row - 1) 1}
-- Quit
event s (VtyEvent (EvKey KEsc [])) = halt s
event s (VtyEvent (EvKey (KChar 'q') [])) = halt s
-- Saving
event s@(State fp tsks _) (VtyEvent (EvKey (KChar 's') [])) = do
    liftIO $ writeFile fp (unlines $ map serialize tsks)
    continue s
-- File changed on disk
event (State fp tsks row) (AppEvent TodoFileUpdated) = do
  t <- liftIO $ readFile fp
  let newTasks = either (const tsks) id (parseMany t)
  continue $ State fp newTasks row
-- Default
event s _ = continue s

vscroll :: Res -> Int -> EventM Res ()
vscroll vp n = vScrollBy (viewportScroll vp) n

