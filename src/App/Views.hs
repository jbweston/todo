{-# LANGUAGE OverloadedStrings #-}

module App.Views
( taskListView
)
where

import Data.Text (Text, singleton)
import Data.Set (toList)

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


taskListView :: Res -> [Task] -> W
taskListView r@(Res t) tsks =
  border $ title t <=> mainView
  where
    mainView =
      viewport r Vertical $
          vBox $ map (<=> dashHBorder) taskViews
    dashHBorder = withBorderStyle (borderStyleFromChar '-') hBorder
    taskViews = map taskView tsks

title :: Text -> W
title = txt .> hCenter .> (<=> hBorder)

taskView :: Task -> W
taskView t =
  vLimit 5 $
    prio <+> descr <+> (padLeft Max $ ctxs <+> prjs)
  where
    prio  = t |> priority    |> priorityView |> padLeft (Pad 1) |> padRight (Pad 2)
    prjs  = t |> projects    |> toList |> map projectView |> map (padLeft $ Pad 1) |> hBox
    ctxs  = t |> contexts    |> toList |> map contextView |> map (padLeft $ Pad 1) |> hBox
    descr = t |> description |> descriptionView |> descrStyle
    descrStyle = if completed t then withAttr completedStyle else id


priorityView :: Maybe Priority -> W
priorityView = maybe ' ' priorityChar .> singleton .> txt

projectView :: Project -> W
projectView = projectText .> ("+" <>) .> txt .> padLeftRight 1 .> withAttr projectStyle

contextView :: Context -> W
contextView = contextText .> ("@" <>) .> txt .> padLeftRight 1 .> withAttr contextStyle

descriptionView :: Description -> W
descriptionView = descriptionText .> txt
