{-# LANGUAGE OverloadedStrings #-}

module App.Resources
( vpMain
, completedStyle
, projectStyle
, contextStyle
, tagStyle
, priorityStyle
, uiAttrs
)
where

import Graphics.Vty (Attr)
import Graphics.Vty.Attributes (defAttr, withStyle, bold)
import Graphics.Vty.Attributes.Color
import Brick.AttrMap (AttrName)
import Brick.Util (on, fg)

import Prelude ()

import App.Types

-- Resources (viewports etc)

vpMain :: Res
vpMain = Res "All Tasks"


-- Styles

priorityStyle :: AttrName
priorityStyle = "priority"

completedStyle :: AttrName
completedStyle = "completed"

projectStyle :: AttrName
projectStyle = "project"

contextStyle :: AttrName
contextStyle = "context"

tagStyle :: AttrName
tagStyle = "tag"

uiAttrs :: [(AttrName, Attr)]
uiAttrs =
  [ (completedStyle, fg blue)
  , (projectStyle, black `on` blue)
  , (contextStyle, black `on` cyan)
  , (tagStyle, black `on` red)
  , (priorityStyle, defAttr `withStyle` bold)
  ]
