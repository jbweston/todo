{-# LANGUAGE OverloadedStrings #-}

module App.Views
( taskListView
)
where

import Data.List (sortOn)
import Data.Text (Text, singleton)
import qualified Data.Set
import qualified Data.Map
import Data.Maybe (isNothing)

import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Types hiding (Context)

import Flow

import App.Resources
import App.Types
import Data.Task

import Prelude


taskListView :: Res -> [Task] -> Int -> W
taskListView r@(Res t) tsks row =
  border $ title t <=> mainView
  where
    mainView =
      viewport r Vertical $
          vBox $ map (<=> dashHBorder) taskViews
    dashHBorder = withBorderStyle (borderStyleFromChar '-') hBorder
    taskViews = tsks |> sortOn completed |> map taskView |> highlightSelected row

title :: Text -> W
title = txt .> hCenter .> (<=> hBorder)

taskView :: Task -> W
taskView t =
  vLimit 5 $
    padL 1 done <+> padLR 1 prio <+> padRight Max descr <+> labelView t
  where
    done  = t |> completed   |> (\c -> if c then "[x]" else "[ ]") |> txt
    prio  = t |> priority    |> priorityView
    descr = t |> description |> descriptionView

-- TOOD: Make this a box and reflow
labelView :: Task -> W
labelView t = hBox <| map (padL 1) <| prjs <> ctxs <> tgs
  where
    prjs  = t |> projects |> Data.Set.toList |> map projectView
    ctxs  = t |> contexts |> Data.Set.toList |> map contextView
    tgs   = t |> tags     |> Data.Map.toList |> map tagView


priorityView :: Maybe Priority -> W
priorityView p = p |> maybe ' ' priorityChar .> singleton .> txt .> style
  where
    style = if isNothing p then id else withAttr priorityStyle

projectView :: Project -> W
projectView = projectText .> ("+" <>) .> txt .> padLeftRight 1 .> withAttr projectStyle

contextView :: Context -> W
contextView = contextText .> ("@" <>) .> txt .> padLeftRight 1 .> withAttr contextStyle

tagView :: (TagType, Tag) -> W
tagView (tt, t) = (tagTypeText tt <> ":" <> tagText t) |> txt |> padLeftRight 1 |> withAttr tagStyle

descriptionView :: Description -> W
descriptionView = descriptionText .> txt

-- Utilities

highlightSelected :: Int -> [W] -> [W]
highlightSelected row widgets =
 zip [1..] widgets |> map style
 where
   style (i, w)
     | i == row  = w |> withAttr completedStyle
     | otherwise = w

padL :: Int -> W -> W
padL = padLeft . Pad

padLR :: Int -> W -> W
padLR = padLeftRight
