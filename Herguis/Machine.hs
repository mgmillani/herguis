module Herguis.Machine where

data MachineProperty =
	Instruction
	{
	  name       ::String
	, size       ::Int
	, operands   ::String
	, addressings::[String]
	, registers  ::[String]
	, binaryCode ::String
	}
	|
	Register
	{
	  name::String
	, size::Int
	}
	|
	Addressing
	{
	  name    ::String
	, relative::Bool
	, code    ::Int
	, otype   ::Char
	}

	deriving (Show)
