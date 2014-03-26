module Herguis.Setup(buildInterface) where

import Control.Monad.Trans(liftIO)
import Data.IORef
import Data.Time.Clock
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView

import qualified Herguis.Action as Action
import Herguis.Entities

setupMenuItemFile fileStatus window = do
	file <- menuItemNewWithMnemonic "_File"
	menu <- menuNew

	s0 <- separatorMenuItemNew
	s1 <- separatorMenuItemNew

	new <- menuItemNewWithMnemonic "_New"
	new `on` menuItemActivate $ Action.new fileStatus window

	open <- menuItemNewWithMnemonic "_Open"
	open `on` menuItemActivate $ Action.open fileStatus window

	save <- menuItemNewWithMnemonic "_Save"
	save `on` menuItemActivate $ Action.save fileStatus window >> return ()

	saveas <- menuItemNewWithMnemonic "Save _as"
	saveas `on` menuItemActivate $ Action.saveAs fileStatus window >> return ()

	exit <- menuItemNewWithMnemonic "_Exit"
	exit `on` menuItemActivate $ Action.quit fileStatus window >>= \x -> return ()

	containerAdd menu new
	containerAdd menu s0
	containerAdd menu open
	containerAdd menu save
	containerAdd menu saveas
	containerAdd menu s1
	containerAdd menu exit

	set file [menuItemSubmenu := menu]

	return file

setupMenuItemEdit = menuItemNewWithMnemonic "_Edit"
setupMenuItemHelp = menuItemNewWithMnemonic "_Help"

setupMenuBar fileStatus window = do
	bar <- menuBarNew
	miFile <- setupMenuItemFile fileStatus window
	miEdit <- setupMenuItemEdit
	miHelp <- setupMenuItemHelp

	containerAdd bar miFile
	containerAdd bar miEdit
	containerAdd bar miHelp

	return bar

setupTextView = do
	tTextView <- sourceViewNew
	set tTextView [textViewWrapMode := WrapWord, sourceViewShowLineNumbers := True]
	tTextBuffer <- get tTextView textViewBuffer

	return (tTextView,tTextBuffer)

buildInterface config = do

	activeFile <- newIORef $ filename config
	now <- getCurrentTime
	updateTime <- newIORef $
		case lastUpdate config of
			Nothing -> now
			Just t -> t

	syncFile <- newIORef $ syncWithFile config
	modified <- newIORef False

	window  <- windowNew
	mainBox <- vBoxNew False 0
	textWindow <- scrolledWindowNew Nothing Nothing
	set textWindow [scrolledWindowHscrollbarPolicy := PolicyAutomatic, scrolledWindowVscrollbarPolicy := PolicyAutomatic]

	(tTextView,tTextBuffer) <- setupTextView
	textBufferInsertAtCursor tTextBuffer (text config)
	tTextBuffer `on` bufferChanged $ liftIO $ writeIORef modified True

	let fileStatus = FileStatus{filenameRef = activeFile,lastUpdateRef = updateTime, buffer = tTextBuffer, syncRef = syncFile, modifiedRef = modified}
	mMenuBar <- setupMenuBar fileStatus window

	set window [windowDefaultWidth := 640, windowDefaultHeight := 480, containerChild := mainBox, containerBorderWidth := 1]
	boxPackStart mainBox mMenuBar PackNatural 0
	boxPackStart mainBox textWindow PackGrow 0
	containerAdd textWindow tTextView

	window `on` deleteEvent $ Action.quit fileStatus window
	window `on` focusInEvent $ liftIO $ Action.reloadFile fileStatus

	widgetShowAll window
