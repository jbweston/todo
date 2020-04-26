{-# LANGUAGE OverloadedStrings #-}

module App.Resources
( vpMain
, completedStyle
, projectStyle
, contextStyle
, uiAttrs
)
where

import Graphics.Vty (Attr)
import Graphics.Vty.Attributes.Color
import Brick.AttrMap (AttrName)
import Brick.Util (on, fg, bg)

import App.Types

-- Resources (viewports etc)

vpMain :: Res
vpMain = Res "All Tasks"


-- Styles

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
