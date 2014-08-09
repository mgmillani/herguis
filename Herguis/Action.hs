module Herguis.Action where

import Control.Monad
import Control.Monad.Trans(liftIO)
import Data.DescriLo
import Data.IORef
import Data.Time.Clock
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import System.Directory
import System.FilePath
import System.IO
import System.Exit
-- import System.Process

import Herguis.Entities
import qualified Herguis.Work as Work

quit editorStatusRef assemblerStatusRef lastStatusFile window = do
	editorStatus <- readIORef editorStatusRef
	continue <- if (modified editorStatus) then do
		cancelled <- askToSave editorStatusRef assemblerStatusRef window "Save the file before quitting?"
		if cancelled then return True
		else mainQuit >> return False
	else
		mainQuit >> return False
	when (not continue) $ do
		assemblerStatus <- readIORef assemblerStatusRef
		saveStatus editorStatus assemblerStatus lastStatusFile
	return continue

-- | writes error and warnings from the assembler to the given ListStore
addAssemblerOutput listStore stdout stderr parseError parseWarning = do
	listStoreClear listStore
	let warnings = lines stdout
	let errors   = lines stderr

	mapM_ (listStoreAppend listStore) $ zip warnings $ repeat "war"
	mapM_ (listStoreAppend listStore) $ zip errors $ repeat "err"


-- | sends the source file to the assembler
-- | if the source hasn't been saved, prompts the user
assemble editorStatusRef assemblerStatusRef assembler messages outputList window = do
	cancelled <- askToSave editorStatusRef assemblerStatusRef window "You have to save the file before assembling. Do you want to do it now?"
	when (not cancelled) $ do
		editorStatus <- readIORef editorStatusRef
		assemblerStatus <- readIORef assemblerStatusRef
		(sout, serr, eCode) <- Work.assemble editorStatus assemblerStatus assembler messages
		addAssemblerOutput outputList sout serr
		case eCode of
			ExitSuccess -> return ()
			ExitFailure f -> do
				if null sout && null serr then
					popupError ("Assembling failed:\n" ++ assembler ++ " finished with the error code: " ++ show f) window
				else
					return ()


-- | asks the user whether the current file should be saved or not
-- | returns whether the user cancelled the action or not
askToSave editorStatusRef assemblerStatusRef window msg = do
	editorStatus <- readIORef editorStatusRef
	if not (modified editorStatus) then return False
	else do
		dialog <- dialogNew
		windowSetPosition dialog WinPosCenterOnParent
		set dialog [windowWindowPosition := WinPosCenterOnParent, windowTransientFor := window]
		upper <- dialogGetUpper dialog
		actionArea <- dialogGetActionArea dialog

		msg <- labelNew $ Just msg
		set msg [widgetVisible := True]

		containerAdd upper msg

		dialogAddButton dialog "gtk-save" ResponseYes
		dialogAddButton dialog "gtk-no" ResponseNo
		dialogAddButton dialog "gtk-cancel" ResponseCancel

		answer <- dialogRun dialog
		widgetDestroy dialog
		case answer of
			ResponseYes -> save editorStatusRef assemblerStatusRef window
			ResponseNo -> return False
			ResponseCancel -> return True

-- | creates a new file
-- | if changes were made to the current file, prompts the user if it should be saved first
new editorStatusRef assemblerStatusRef window = do
	cancelled <- askToSave editorStatusRef assemblerStatusRef window "Save current code before creating a new one?"
	if not cancelled then do
		editorStatus <- readIORef editorStatusRef
		set (buffer editorStatus) [textBufferText := ""]
		now <- getCurrentTime
		writeIORef editorStatusRef editorStatus{lastUpdate = now, modified = False, filename = ""}
	else return ()

-- | opens an existing file
-- if changes were made to the current file, prompts the user if it should be saved first
open editorStatusRef assemblerStatusRef window = do
	editorStatus <- readIORef editorStatusRef
	let currentName = filename editorStatus
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
				load opn editorStatusRef
				return ()
	else return ()

	widgetDestroy dialog

-- | Saves the current working status in order to load it back next time
saveStatus editorStatus assemblerStatus file =
	writeFile file $ show status
	where
		status = Description "State" [ ("source", filename editorStatus)
		                             , ("machine", machineFile assemblerStatus)
		                             , ("output file", outputFile assemblerStatus)
		                             , ("output type", outputType assemblerStatus)
		                             , ("symbols", symbolFile assemblerStatus)
		                             ]

saveFile editorStatusRef assemblerStatusRef dest = do
	editorStatus <- readIORef editorStatusRef
	assemblerStatus <- readIORef assemblerStatusRef
	let textBuffer = buffer editorStatus
	let updateTime = lastUpdate editorStatus
	text <- get textBuffer textBufferText
	writeFile dest text
	now <- getCurrentTime
	writeIORef editorStatusRef editorStatus{modified = False, filename = dest, lastUpdate = now}
	writeIORef assemblerStatusRef assemblerStatus{outputFile = replaceExtension dest "mem", symbolFile = replaceExtension dest "sym"}

-- | saves the file to its current location, or open the save as dialog if it has not been saved
-- returns whether the user cancelled the action or not
save editorStatusRef assemblerStatusRef window = do
	editorStatus <- readIORef editorStatusRef
	if (filename editorStatus) == "" then saveAs editorStatusRef assemblerStatusRef window else saveFile editorStatusRef assemblerStatusRef (filename editorStatus) >> return False

-- | opens a dialog, allowing the user to choose where to save the current text
-- further changes will be saved to this new file, and syncing will be done with it as well
-- returns whether the user cancelled the action or not
saveAs editorStatusRef assemblerStatusRef window = do
	editorStatus <- readIORef editorStatusRef
	let currentName = filename editorStatus
	dialog <- fileChooserDialogNew Nothing (Just window) FileChooserActionSave [("Save",ResponseOk), ("Cancel",ResponseCancel)]
	set dialog [fileChooserDoOverwriteConfirmation := True]
	if currentName /= "" then do
		fileChooserSetFilename dialog currentName
		return ()
	else return ()

	answer <- dialogRun dialog

	v <- if answer == ResponseOk then do
		destination <- fileChooserGetFilename dialog
		case destination of
			Nothing -> return False
			Just dest -> do
				saveFile editorStatusRef assemblerStatusRef dest
				return False
	else return True

	widgetDestroy dialog
	return v

-- | loads a new file
load "" editorStatusRef = return ()
load fname editorStatusRef = do
	editorStatus <- readIORef editorStatusRef
	text <- readFile fname
	let textBuffer = buffer editorStatus
	set textBuffer [textBufferText := text]
	newTime <- getModificationTime fname
	writeIORef editorStatusRef editorStatus{filename = fname, lastUpdate = newTime, modified = False}

-- | reloads the current file if it has changed since the last time and if we are supposed to sync with it
reloadFile editorStatusRef = do
	editorStatus <- readIORef editorStatusRef
	let textBuffer = buffer editorStatus
	let fname = filename editorStatus
	if not (sync editorStatus) || fname == "" then return False
	else do
		newTime <- getModificationTime fname
		if (lastUpdate editorStatus) < newTime then do
			text <- readFile fname
			set textBuffer [textBufferText := text]
			writeIORef editorStatusRef editorStatus{modified = False, lastUpdate = newTime}
			return False
		else
			return False

popupError msg window = do
	dialog <- dialogNew
	windowSetPosition dialog WinPosCenterOnParent
	set dialog [windowWindowPosition := WinPosCenterOnParent, windowTransientFor := window]
	upper <- dialogGetUpper dialog

	msg <- labelNew $ Just msg
	set msg [widgetVisible := True]

	containerAdd upper msg

	dialogAddButton dialog "_Ok" ResponseOk
	answer <- dialogRun dialog
	widgetDestroy dialog

