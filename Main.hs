module Main where

import Data.IORef
import Data.Time.Clock
import Graphics.UI.Gtk
import System.Directory

import Herguis.Entities
import Herguis.Loader
import Herguis.Machine
import Herguis.Setup

main = do
	initGUI
	config <- getConfig appName
	--print config
	result <- loadLastStatus (lastStatusFile config) defaultEditorStatus{sync = autoSync config} defaultAssemblerStatus
	case result of
		Right (editor,assembler) ->
			if filename editor /= "" then do
				newTime <- getModificationTime $ filename editor
				buildInterface config editor{lastUpdate = newTime} assembler
			else
				buildInterface config editor assembler
		Left msg -> buildInterface config{hasError = True, errorMsg = msg} defaultEditorStatus defaultAssemblerStatus
	mainGUI
