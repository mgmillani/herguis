module Herguis.Setup(buildInterface) where

import Control.Monad
import Control.Monad.Trans(liftIO)
import Data.IORef
import Data.List
import Data.Tree
import Data.Time.Clock
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import Text.Printf

import qualified Herguis.Action as Action
import Herguis.Entities

setupMenuItemFile editorStatusRef assemblerStatusRef config window = do
	file <- menuItemNewWithMnemonic "_File"
	menu <- menuNew

	s0 <- separatorMenuItemNew
	s1 <- separatorMenuItemNew

	new <- menuItemNewWithMnemonic "_New"
	new `on` menuItemActivate $ Action.new editorStatusRef assemblerStatusRef window

	open <- menuItemNewWithMnemonic "_Open"
	open `on` menuItemActivate $ Action.open editorStatusRef assemblerStatusRef window

	save <- menuItemNewWithMnemonic "_Save"
	save `on` menuItemActivate $ Action.save editorStatusRef assemblerStatusRef window >> return ()

	saveas <- menuItemNewWithMnemonic "Save _as"
	saveas `on` menuItemActivate $ Action.saveAs editorStatusRef assemblerStatusRef window >> return ()

	exit <- menuItemNewWithMnemonic "_Exit"
	exit `on` menuItemActivate $ Action.quit editorStatusRef assemblerStatusRef (lastStatusFile config) window >>= \x -> return ()

	containerAdd menu new
	containerAdd menu s0
	containerAdd menu open
	containerAdd menu save
	containerAdd menu saveas
	containerAdd menu s1
	containerAdd menu exit

	set file [menuItemSubmenu := menu]

	return file

setupMenuItemPreferences = menuItemNewWithMnemonic "_Preferences"
setupMenuItemHelp = menuItemNewWithMnemonic "_Help"

setupMenuBar editorStatusRef assemblerStatusRef config window = do
	bar <- menuBarNew
	miFile <- setupMenuItemFile editorStatusRef assemblerStatusRef config window
	miPref <- setupMenuItemPreferences
	miHelp <- setupMenuItemHelp

	containerAdd bar miFile
	containerAdd bar miPref
	containerAdd bar miHelp

	return bar

setupTextView config = do
	tTextView <- sourceViewNew
	set tTextView [textViewWrapMode := wrapMode config, sourceViewShowLineNumbers := lineNumbering config]
	tTextBuffer <- get tTextView textViewBuffer

	return (tTextView,tTextBuffer)

setupActionBar machines editorStatusRef assemblerStatusRef assemblerName msgFile outputList window = do
	bar <- hBoxNew False 0
	machineBox <- comboBoxNewText
	set machineBox [comboBoxWrapWidth := 1]
	mapM_ (\(x,y) -> comboBoxAppendText machineBox x) machines
	assemblerStatus <- readIORef assemblerStatusRef
	when (machineFile assemblerStatus /= "") $
		let index = find (\((_,y),_) -> y == (machineFile assemblerStatus)) $ zip machines [0,1..] in
		case index of
			Just (_,index) -> set machineBox [comboBoxActive := index]
			Nothing -> return ()

	sep <- vSeparatorNew

	refreshImg <- imageNewFromStock stockRefresh IconSizeButton
	assembleImg <- imageNewFromStock stockExecute IconSizeButton
	refreshBtn <- buttonNew
	assembleBtn <- buttonNew
	set refreshBtn [buttonImage := refreshImg, widgetTooltipText := Just "Refresh"]
	set assembleBtn [buttonImage := assembleImg, widgetTooltipText := Just "Assemble"]

	boxPackStart bar machineBox PackNatural 0
	boxPackStart bar sep PackNatural 10
	boxPackStart bar refreshBtn PackNatural 0
	boxPackStart bar assembleBtn PackNatural 0

	machineBox `on` changed $ do
		active <- get machineBox comboBoxActive
		let (name,location) = machines !! active
		assemblerStatus <- readIORef assemblerStatusRef
		writeIORef assemblerStatusRef assemblerStatus{machineFile = location}

	refreshBtn `on` buttonActivated $ do
		editorStatus <- readIORef editorStatusRef
		Action.load (filename editorStatus) editorStatusRef

	assembleBtn `on` buttonActivated $ Action.assemble editorStatusRef assemblerStatusRef assemblerName msgFile outputList window

	return bar

setupOutputBar = do
	treeModel <- listStoreNew []
	treeView <- treeViewNewWithModel treeModel
	vAdjust <- get treeView treeViewVAdjustment
	case vAdjust of
		Just ad -> set ad [adjustmentUpper := 0.3, adjustmentValue := 0.1]
		Nothing -> return ()

	typeCol <- treeViewColumnNew
	lineCol <- treeViewColumnNew
	msgCol <- treeViewColumnNew
	treeViewColumnSetTitle typeCol "Type"
	treeViewColumnSetTitle lineCol "Line"
	treeViewColumnSetTitle msgCol "Message"
	typeRenderer <- cellRendererTextNew
	lineRenderer <- cellRendererTextNew
	msgRenderer <- cellRendererTextNew
	cellLayoutPackStart typeCol typeRenderer False
	cellLayoutPackStart lineCol lineRenderer False
	cellLayoutPackStart msgCol msgRenderer False
	cellLayoutSetAttributes typeCol typeRenderer treeModel
		$ \(a,b,c) -> [cellText := a]
	cellLayoutSetAttributes typeCol lineRenderer treeModel
		$ \(a,b,c) -> [cellText := b]
	cellLayoutSetAttributes msgCol msgRenderer treeModel
		$ \(a,b,c) -> [cellText := c]
	treeViewAppendColumn treeView typeCol
	treeViewAppendColumn treeView lineCol
	treeViewAppendColumn treeView msgCol

	treeViewSetHeadersVisible treeView True

	vadj <- get treeView treeViewVAdjustment
	hadj <- get treeView treeViewHAdjustment

	swin <- scrolledWindowNew hadj vadj
	containerAdd swin treeView

	return (treeModel,swin)


buildInterface config editorStatus assemblerStatus = do

	window  <- windowNew
	mainBox <- vBoxNew False 0
	textWindow <- scrolledWindowNew Nothing Nothing
	set textWindow [scrolledWindowHscrollbarPolicy := PolicyAutomatic, scrolledWindowVscrollbarPolicy := PolicyAutomatic]

	-- sets up editor's text
	(tTextView,tTextBuffer) <- setupTextView config
	editorStatusRef <- newIORef editorStatus{buffer = tTextBuffer}
	Action.load (filename editorStatus) editorStatusRef
	tTextBuffer `on` bufferChanged $ liftIO $ do
		eStatus <- readIORef editorStatusRef
		writeIORef editorStatusRef eStatus{modified = True}

	assemblerStatusRef <- newIORef assemblerStatus

	(outputList, outputView) <- setupOutputBar

	mMenuBar <- setupMenuBar editorStatusRef assemblerStatusRef config window

	actionBar <- setupActionBar (machines config) editorStatusRef assemblerStatusRef (assembler config) (messageFile config) outputList window

	set window [windowDefaultWidth := 640, windowDefaultHeight := 480, containerChild := mainBox, containerBorderWidth := 1]

	let cols = 4
	sourceBox <- tableNew cols 1 True
	boxPackStart mainBox mMenuBar PackNatural 0
	boxPackStart mainBox actionBar PackNatural 0
	boxPackStart mainBox sourceBox PackGrow 0
	tableAttachDefaults sourceBox textWindow 0 1 0 (cols -1)
	tableAttachDefaults sourceBox outputView 0 1 (cols -1) cols
	containerAdd textWindow tTextView

	window `on` deleteEvent $ liftIO $ Action.quit editorStatusRef assemblerStatusRef (lastStatusFile config) window
	window `on` focusInEvent $ liftIO $ Action.reloadFile editorStatusRef

	widgetShowAll window

	when (hasError config) $ Action.popupError (errorMsg config) window
