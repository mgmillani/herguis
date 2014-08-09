module Herguis.Loader where

import Control.Exception
import Data.Char
import Data.DescriLo
import Graphics.UI.Gtk
import System.Directory
import System.Environment
import System.FilePath
import System.IO

import Herguis.Entities

-- header to be used on the sample config file
configHeader =
	"# This is the configuration file for " ++ appName ++ "\n" ++
	"# For a full description of its syntax, see the haddock documentation of Data.DescriLo\n" ++
	"# All locations may be absolute or relative to the config file (this file).\n\n"

-- | gets the directory where configuration files should be placed
--
-- | First, checks if XDG_CONFIG_HOME exists, producing $XDG_CONFIG_HOME/appName if it does
-- | if it does not, the checks if HOME does, producing $HOME/.config/appName if it does
-- | if it still fails, returns getAppUserDataDirectory appName
getConfigDirectory appName =
	let failXDG e = do
		dir <- getEnv "HOME"
		return $ dir ++ [pathSeparator] ++ ".config" ++ [pathSeparator] ++ appName
	in
	let failHOME e = getAppUserDataDirectory appName in
	do
	handle
		(failHOME::SomeException -> IO FilePath) $
		handle (failXDG::SomeException -> IO FilePath) $ do
			dir <- getEnv "XDG_CONFIG_HOME"
			return $ dir ++ [pathSeparator] ++ appName

-- | creates a new config file
installConfig cFile config = do
	-- creates the base config file
	hcFile <- openFile cFile WriteMode
	hPutStr hcFile configHeader
	hPutStr hcFile $ show config
	hClose hcFile

readLocation cDir l = if isRelative l then cDir ++ [pathSeparator] ++ l else l

loadConfig cFile cDir = do
	descriptions <- loadDescriptionFile cFile "Machines"
	let result = parse descriptions defaultConfig
	case result of
		Right c -> return c
		Left c -> return defaultConfig{hasError = True, errorMsg = c}
	where
		parse :: [Description] -> Config -> Either String Config
		parse [] cfg = Right cfg
		parse ((Description title values):r) cfg = do
			c <- case (map toLower title) of
				"gui" -> loadGui values cfg
				"machines" -> loadMachines values cfg
				"programs" -> loadPrograms values cfg
				"misc" -> loadMisc values cfg
				_ -> Left $ "Invalid section: " ++ title
			parse r c
			where
				loadGui :: [(String,String)] -> Config -> Either String Config
				loadGui [] cfg = Right cfg
				loadGui ((l,r):t) cfg =
					case map toLower l of
						"wrap mode" ->
							readWrapMode r >>= (\wrap -> Right cfg{wrapMode = wrap})
						"line numbering" ->
							readBool r >>=	(\v -> Right cfg{lineNumbering = v})
						"auto sync" ->
							readBool r >>= (\v -> Right cfg{autoSync = v})
						_ -> Left $ "Invalid attribute: " ++ l
					>>= (loadGui t)
					where
						readWrapMode a = case map toLower a of
							"none" -> Right WrapNone
							"char" -> Right WrapChar
							"word" -> Right WrapWord
							"word char" -> Right WrapWord
							_ -> Left $ a ++ " is not a valid wrap mode."
						readBool b = case map toLower b of
							"true" -> Right True
							"false" -> Right False
							_ -> Left $ b ++ " is not a valid boolean value."
				loadMachines [] cfg = Right cfg
				loadMachines ((l,r):t) cfg = do
					rest <- loadMachines t cfg
					Right rest{machines = (l,readLocation cDir r):(machines rest)}
				loadPrograms [] cfg = Right cfg
				loadPrograms ((l,r):t) cfg =
					case map toLower l of
						"assembler" -> Right cfg{assembler = r}
						"simulator" -> Right cfg{simulator = r}
						_           -> Left $ "Invalid attribute: " ++ l
					>>=
					loadPrograms t
				loadMisc [] cfg = Right cfg
				loadMisc ((l,r):t) cfg =
					case map toLower l of
						"assembler messages" -> Right cfg{messageFile = readLocation cDir r}
						"last status" -> Right cfg{lastStatusFile = readLocation cDir r}
						_ -> Left $ "Invalid attribute: " ++ l
					>>=
					loadMisc t


getConfig appName = do
	cDir <- getConfigDirectory appName
	-- if the configuration directory does not exists, sets it up
	let cFile = cDir ++ "/config"
	confExists <- doesFileExist cFile
	if (not confExists) then do
		createDirectoryIfMissing True cDir
		installConfig cFile defaultConfig
		return defaultConfig
	else
		loadConfig cFile cDir

-- | loads the previous state from the file
loadLastStatus :: String -> EditorStatus -> AssemblerStatus -> IO (Either String (EditorStatus,AssemblerStatus))
loadLastStatus fname editorStatus assemblerStatus = do
	cDir <- getConfigDirectory appName
	lastExists <- doesFileExist fname
	if not lastExists then return $ Right (editorStatus,assemblerStatus)
	else do
	  descriptions <- loadDescriptionFile fname "State"
	  let result = parse (values $ head descriptions) cDir (editorStatus,assemblerStatus)
	  return result
	where
		parse :: [(String,String)] -> FilePath -> (EditorStatus,AssemblerStatus) -> Either String (EditorStatus,AssemblerStatus)
		parse [] _ cfg = Right cfg
		parse ((l,r):t) cDir (editor,assembler) =
			case map toLower l of
				"source" -> Right (editor{filename = readLocation cDir r},assembler)
				"machine" -> Right (editor,assembler{machineFile = readLocation cDir r})
				"output file" -> Right (editor,assembler{outputFile = readLocation cDir r})
				"output type" -> Right (editor,assembler{outputType = r})
				"symbols" -> Right (editor,assembler{symbolFile = readLocation cDir r})
				_ -> Left $ fname ++ " -\nInvalid attribute: " ++ l
			>>=
			parse t cDir


