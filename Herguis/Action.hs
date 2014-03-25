module Herguis.Action where

import Control.Monad.Trans(liftIO)
import Data.IORef
import Data.Time.Clock
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import System.Directory

import Herguis.Entities

quit = do
	liftIO mainQuit

saveFile fileStatus = do
	dest <- readIORef $ filenameRef fileStatus
	let textBuffer = buffer fileStatus
	let updateTime = lastUpdateRef fileStatus
	text <- get textBuffer textBufferText
	writeFile dest text
	now <- getCurrentTime
	writeIORef updateTime now

-- | saves the file to its current location, or open the save as dialog if it has not been saved
save fileStatus = do
	dest <- readIORef $ filenameRef fileStatus
	if dest == "" then saveAs fileStatus else saveFile fileStatus

-- | opens a dialog, allowing the user to choose where to save the current text
-- | further changes will be saved to this new file, and syncing will be done with it as well
saveAs fileStatus = do
	let activeFile = filenameRef fileStatus
	currentName <- readIORef activeFile
	dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionSave [("Save",ResponseAccept)]
	set dialog [fileChooserDoOverwriteConfirmation := True]
	if currentName /= "" then do
		fileChooserSetFilename dialog currentName
		return ()
	else return ()

	answer <- dialogRun dialog

	if answer == ResponseAccept then do
		destination <- fileChooserGetFilename dialog
		case destination of
			Nothing -> putStrLn "Nope"
			Just dest -> do
				writeIORef activeFile dest
				saveFile fileStatus

	else putStrLn "Nothing"

	widgetDestroy dialog


reloadFile activeFile lastUpdate textBuffer sync = do
	fname <- liftIO $ readIORef activeFile
	oldTime <- liftIO $ readIORef lastUpdate
	newTime <- liftIO $ getModificationTime fname
	if oldTime < newTime then do
		text <- liftIO $ readFile fname
		liftIO $ set textBuffer [textBufferText := text]
		liftIO $ writeIORef lastUpdate newTime
		return False
	else
		return False
