module Herguis.Entities where

import Data.IORef
import Data.Time.Clock
import Data.Time.Calendar
import Graphics.UI.Gtk

import Data.DescriLo

data MessageType = ErrorMsg | WarningMsg

data Config =
	Config
	{
	  wrapMode      ::WrapMode
	, lineNumbering ::Bool
	, autoSync      ::Bool
	, machines      ::[(String,String)]
	, messageFile   ::String
	, lastStatusFile::String
	, assembler     ::String
	, simulator     ::String
	, hasError      ::Bool
	, errorMsg      ::String
	}

defaultConfig =
	Config
	{
	  wrapMode = WrapWord
	, lineNumbering = True
	, machines = []
	, messageFile = ""
	, lastStatusFile = "last"
	, autoSync = True
	, assembler = ""
	, simulator = ""
	, hasError = False
	, errorMsg = ""
	}

showConfig c =
	   "# GUI description. Possible attributes:\n"
	++ "# warp mode - how word wrapping will be done (none, char, WORD, word char)\n"
	++ "# line numbers - whether line numbers will appear or not (TRUE,false) \n"
	++ "# auto sync - whether the program should automatically reload the file when it has changed (TRUE,false) \n"
	++ "# there are no other values for GUI\n"
	++ (show $ Description "GUI" guiValues)
	++ "\n# Machines description. An infinite amount of attributes is possible. The lvalue is the name of the machine, and rvalue, its location.\n"
	++ (show $ Description "Machines" $ machines c)
	++ "\n# Programs description. Possible attributs:\n"
	++ "# assembler - program to be called as the assembler.\n"
	++ "# simulator - program to be called as the simulator.\n"
	++ (show $ Description "Programs" programs)
	++ "\n# Miscellaneous stuff. Possible attributes:\n"
	++ "# assembler messages - file which contains error and warning messages for the assembler.\n"
	++ "# last status - file which contains the last used configuration, like which file was open and what machine was used.\n"
	++ (show $ Description "Misc" misc)
	where
		guiValues =
			[
				("wrap mode", wrapStr)
			, ("line numbering", show $ lineNumbering c)
			, ("auto sync", show $ autoSync c)
			]
		wrapStr = case wrapMode c of
			WrapNone     -> "none"
			WrapWord     -> "word"
			WrapChar     -> "char"
			WrapWordChar -> "word char"
		programs = [("assembler",assembler c), ("simulator",simulator c)]
		misc = [("assembler messages", messageFile c) , ("last status", lastStatusFile c)]

instance Show Config where
	show = showConfig

data EditorStatus =
	EditorStatus
	{
	  filename  ::String
	, lastUpdate::UTCTime
	, buffer    ::TextBuffer
	, sync      ::Bool
	, modified  ::Bool
	}

defaultEditorStatus =
	EditorStatus
	{
	  filename = ""
	, sync = True
	, modified = False
	}

data AssemblerStatus =
	AssemblerStatus
	{
	  machineFile::String
	, outputFile ::String
	, outputType ::String
	, symbolFile ::String
	}

defaultAssemblerStatus =
	AssemblerStatus
	{
	  machineFile = ""
	, outputFile  = ""
	,	outputType  = "Mecaria0"
	, symbolFile  = ""
	}
appName = "herguis"


