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

-- | asks the user whether the current file should be saved or not
askToSave fileStatus = do
	modified <- readIORef (modifiedRef fileStatus)
	if not modified then return False
	else do
		dialog <- dialogNew
		upper <- dialogGetUpper dialog
		actionArea <- dialogGetActionArea dialog

		lbl <- labelNew $ Just "fucking work!"
		msg <- entryNew
		set msg [entryEditable := False, entryHasFrame := False, entryText := "Do you want to die?"]

		containerAdd upper msg
		containerAdd upper lbl

		dialogAddButton dialog "Yes" ResponseYes
		dialogAddButton dialog "No" ResponseNo
		dialogAddButton dialog "Cancel" ResponseCancel

		answer <- dialogRun dialog
		widgetDestroy dialog
		case answer of
			ResponseYes -> save fileStatus >> return False
			ResponseNo -> return False
			ResponseCancel -> return True

-- | creates a new file
-- | if changes were made to the current file, prompts the user if it should be saved first
new fileStatus = do
	cancelled <- askToSave fileStatus
	if not cancelled then do
		set (buffer fileStatus) [textBufferText := ""]
		writeIORef (filenameRef fileStatus) ""
		now <- getCurrentTime
		writeIORef (lastUpdateRef fileStatus) now
		writeIORef (modifiedRef fileStatus) False
	else return ()


-- | opens an existing file
-- | if changes were made to the current file, prompts the user if it should be saved first
open fileStatus = do
	let activeFile = filenameRef fileStatus
	currentName <- readIORef activeFile
	dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen [("Open",ResponseAccept), ("Cancel",ResponseCancel)]
	if currentName /= "" then do
		fileChooserSetFilename dialog currentName
		return ()
	else return ()

	answer <- dialogRun dialog

	if answer == ResponseAccept then do
		opened <- fileChooserGetFilename dialog
		case opened of
			Nothing -> return ()
			Just opn -> do
				load opn fileStatus
				return ()
	else return ()

	widgetDestroy dialog


saveFile fileStatus = do
	dest <- readIORef $ filenameRef fileStatus
	let textBuffer = buffer fileStatus
	let updateTime = lastUpdateRef fileStatus
	text <- get textBuffer textBufferText
	writeFile dest text
	now <- getCurrentTime
	writeIORef updateTime now
	writeIORef (modifiedRef fileStatus) False

-- | saves the file to its current location, or open the save as dialog if it has not been saved
save fileStatus = do
	dest <- readIORef $ filenameRef fileStatus
	if dest == "" then saveAs fileStatus else saveFile fileStatus

-- | opens a dialog, allowing the user to choose where to save the current text
-- | further changes will be saved to this new file, and syncing will be done with it as well
saveAs fileStatus = do
	let activeFile = filenameRef fileStatus
	currentName <- readIORef activeFile
	dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionSave [("Save",ResponseAccept), ("Cancel",ResponseReject)]
	set dialog [fileChooserDoOverwriteConfirmation := True]
	if currentName /= "" then do
		fileChooserSetFilename dialog currentName
		return ()
	else return ()

	answer <- dialogRun dialog

	if answer == ResponseAccept then do
		destination <- fileChooserGetFilename dialog
		case destination of
			Nothing -> return ()
			Just dest -> do
				writeIORef activeFile dest
				saveFile fileStatus
				return ()
	else return ()

	widgetDestroy dialog

-- | loads a new file
load fname fileStatus = do
	writeIORef (filenameRef fileStatus) fname
	text <- readFile fname
	let textBuffer = buffer fileStatus
	set textBuffer [textBufferText := text]
	newTime <- getModificationTime fname
	writeIORef (lastUpdateRef fileStatus) newTime
	writeIORef (modifiedRef fileStatus) False

-- | reloads the current file if it has changed since the last time and if we are supposed to sync with it
reloadFile fileStatus = do
	let textBuffer = buffer fileStatus
	let lastUpdate = lastUpdateRef fileStatus
	sync <- readIORef (syncRef fileStatus)
	fname <- readIORef (filenameRef fileStatus)
	if not sync || fname == "" then return False
	else do
		oldTime <- readIORef lastUpdate
		newTime <- getModificationTime fname
		if oldTime < newTime then do
			text <- readFile fname
			set textBuffer [textBufferText := text]
			writeIORef lastUpdate newTime
			writeIORef (modifiedRef fileStatus) False
			return False
		else
			return False
