{-# LANGUAGE OverloadedStrings #-}

module App.Views
( taskListView

)
where

import Data.List (intersperse)
import Data.Text (Text)
import Data.Set (Set, toList)

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

title :: Text -> W
title = txt .> hCenter .> (<=> hBorder)

taskView :: Task -> W
taskView t =
  vLimit 5 $
    descr <+> (padLeft Max $ ctxs <+> prjs)
  where
    ctxs = contextsView $ contexts t
    prjs = projectsView $ projects t
    descr = descrStyle . txt . descriptionText . description $ t
    descrStyle = if completed t then withAttr completedStyle else id

projectsView :: Set Project -> W
projectsView = toList .> map view .> map (padLeft $ Pad 1) .> hBox
  where
    view = projectText .> withSpace .> txt .> withAttr projectStyle

contextsView :: Set Context -> W
contextsView = toList .> map view .> map (padLeft $ Pad 1) .> hBox
  where
    view = contextText .> withSpace .> txt .> withAttr contextStyle

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

