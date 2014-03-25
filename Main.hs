module Main where

import Data.IORef
import Data.Time.Clock
import Graphics.UI.Gtk
import System.Directory

import Herguis.Entities
import Herguis.Setup

main = do
	initGUI
	-- sampleText <- readFile "example"
	-- updateTime <- getModificationTime "example"
	buildInterface defaultConfig
	mainGUI
