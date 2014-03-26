module Herguis.Action where

import Control.Monad.Trans(liftIO)
import Data.IORef
import Data.Time.Clock
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import System.Directory

import Herguis.Entities

quit fileStatus window = do
	modified <- liftIO $ readIORef (modifiedRef fileStatus)
	if modified then do
		cancelled <- liftIO $ askToSave fileStatus window
		if cancelled then return True
		else liftIO mainQuit >> return False
	else
		liftIO mainQuit >> return False

-- | asks the user whether the current file should be saved or not
-- | returns whether the user cancelled the action or not
askToSave fileStatus window = do
	modified <- readIORef (modifiedRef fileStatus)
	if not modified then return False
	else do
		dialog <- dialogNew
		windowSetPosition dialog WinPosCenterOnParent
		set dialog [windowWindowPosition := WinPosCenterOnParent, windowTransientFor := window]
		upper <- dialogGetUpper dialog
		actionArea <- dialogGetActionArea dialog

		msg <- labelNew $ Just "Do you want to save the file before closing?"
		set msg [widgetVisible := True]

		containerAdd upper msg

		dialogAddButton dialog "Yes" ResponseYes
		dialogAddButton dialog "No" ResponseNo
		dialogAddButton dialog "Cancel" ResponseCancel

		answer <- dialogRun dialog
		widgetDestroy dialog
		case answer of
			ResponseYes -> save fileStatus window
			ResponseNo -> return False
			ResponseCancel -> return True

-- | creates a new file
-- | if changes were made to the current file, prompts the user if it should be saved first
new fileStatus window = do
	cancelled <- askToSave fileStatus window
	if not cancelled then do
		set (buffer fileStatus) [textBufferText := ""]
		writeIORef (filenameRef fileStatus) ""
		now <- getCurrentTime
		writeIORef (lastUpdateRef fileStatus) now
		writeIORef (modifiedRef fileStatus) False
	else return ()

-- | opens an existing file
-- | if changes were made to the current file, prompts the user if it should be saved first
open fileStatus window = do
	let activeFile = filenameRef fileStatus
	currentName <- readIORef activeFile
	dialog <- fileChooserDialogNew Nothing (Just window) FileChooserActionOpen [("Open",ResponseAccept), ("Cancel",ResponseCancel)]
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
-- | returns whether the user cancelled the action or not
save fileStatus window = do
	dest <- readIORef $ filenameRef fileStatus
	if dest == "" then saveAs fileStatus window else saveFile fileStatus >> return False

-- | opens a dialog, allowing the user to choose where to save the current text
-- | further changes will be saved to this new file, and syncing will be done with it as well
-- | returns whether the user cancelled the action or not
saveAs fileStatus window = do
	let activeFile = filenameRef fileStatus
	currentName <- readIORef activeFile
	dialog <- fileChooserDialogNew Nothing (Just window) FileChooserActionSave [("Save",ResponseOk), ("Cancel",ResponseCancel)]
	set dialog [fileChooserDoOverwriteConfirmation := True]
	if currentName /= "" then do
		fileChooserSetFilename dialog currentName
		return ()
	else return ()

	answer <- dialogRun dialog
	widgetDestroy dialog

	if answer == ResponseOk then do
		destination <- fileChooserGetFilename dialog
		case destination of
			Nothing -> return False
			Just dest -> do
				writeIORef activeFile dest
				saveFile fileStatus
				return False
	else return True

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
