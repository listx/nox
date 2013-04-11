{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module DC.Option where

import Data.List (intercalate)
import System.Console.CmdArgs.Implicit

import DC.Data
import DC.Meta
import DC.Util

data Opts = Opts
	{ lang :: Language
	, multi :: Bool
	, uncomment :: Bool
	} deriving (Data, Typeable, Show, Eq)

progOpts :: Opts
progOpts = Opts
	{ lang = Shell &= typ "LANGUAGE" &= help ("language type; values are: "
		++ names
		++ "; default is `sh' for '#' comments")
	, multi = False &= help "multi-line comment style (default is single); if \
\the target language lacks multiline symbols, then the single-line symbol is used"
	, uncomment = False &= help "uncomment the text; you only need to specify the \
\particular language --- dc will take care of both the language's single and multiline symbols"
	}
	where
	names :: String
	names = intercalate ", "
		$ zipWith (\a b -> a ++ " " ++ b)
			(map (squote . cmtLangExt) langs)
			(map (paren . show) langs)

getOpts :: IO Opts
getOpts = cmdArgs $ progOpts
	&= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
	&= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
	&= help "comment/uncomment out blocks of code"
	&= helpArg [explicit, name "help", name "h"]
	&= program _PROGRAM_NAME
	&= details
		[ "Examples:"
		, ""
		, indent "echo \"a\\nab\\nabc\" | dc -l c"
		, ""
		,  "//a"
		,  "//ab"
		,  "//abc"
		, ""
		, indent "echo \"a\\nab\\nabc\" | dc -l c -m"
		, ""
		,  "/*"
		,  "a"
		,  "ab"
		,  "abc"
		,  "*/"
		]
	where
	indent = ("  "++)

cmdsCheck :: Opts -> IO (Opts, Int)
cmdsCheck opts = do
	errVal <- cmdsCheck' opts
	return (opts, errVal)

cmdsCheck' :: Opts -> IO (Int)
cmdsCheck' Opts{..}
	| not (elem lang langs) = errMsg "unsupported language" >> return 1
	| otherwise = return 0
