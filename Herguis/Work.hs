module Herguis.Work where

import Text.ParserCombinators.Parsec
import System.IO
import System.Process

import Herguis.Entities

escape =
	    try string "\\n"
	<|> try string "\\t"
	<|> try string "\\r"

message error warning = do
	mtype <- (try (error >> return ErrorMsg) <|> warning >> return WarningMsg)
	number <- many1 digit
	msg <- escape
	return (mtype, number, msg)

-- | separates the type and the message of a warning or error
readAssemblerMessage msg error warning = parse (message error warning) "" msg

-- | sends the source file to the assembler
-- if the source hasn't been saved, prompts the user
assemble editorStatus assemblerStatus assembler messages = do
	let cmd = proc assembler assemblerCmd
	(_, Just hout, Just herr, pHandle) <- createProcess cmd{std_out = CreatePipe, std_err = CreatePipe}
	mapM_ (\x -> putStr x >> putStr " ") assemblerCmd
	eCode <- waitForProcess pHandle
	serr <- hGetContents herr
	sout <- hGetContents hout
	{-putStrLn "Stdout:"
	putStrLn sout
	putStrLn "\nStderr:"
	putStrLn serr
	-}
	return (sout, serr, eCode)

	where
		assemblerCmd =
				["--input"   , filename editorStatus
				,"--machine" , machineFile assemblerStatus
				,"--messages", messages
				,"--output"  , outputFile assemblerStatus
				,"--type"    , outputType assemblerStatus
				,"--symbols" , symbolFile assemblerStatus
				,"--non-human"
				]
