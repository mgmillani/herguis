module Herguis.Entities where

import Data.IORef
import Data.Time.Clock
import Graphics.UI.Gtk

data GuiConfig =
	GuiConfig
	{
		  text        ::String
		, filename    ::String
		, lastUpdate  ::Maybe UTCTime
		, syncWithFile::Bool
	}

data FileStatus =
	FileStatus
	{
		  filenameRef  ::IORef String
		, lastUpdateRef::IORef UTCTime
		, buffer       ::TextBuffer
	}

defaultConfig = GuiConfig{text = "", filename = "", lastUpdate = Nothing, syncWithFile = False}
