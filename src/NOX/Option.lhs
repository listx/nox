\section{NOX/Option.lhs}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module NOX.Option where

import Data.List (intercalate)
import qualified Data.Text.Lazy as T
import System.Console.CmdArgs.Implicit

import NOX.Language
import NOX.Meta
import NOX.Util

data Opts = Opts
	{ lang :: Language
	, multi :: Bool
	, uncomment :: Bool
	, sline :: String
	, after_lw :: Bool
	} deriving (Data, Typeable, Show, Eq)

progOpts :: Opts
progOpts = Opts
	{ lang = Shell &= typ "LANGUAGE" &= help ("language type; values are: "
		++ names
		++ "; default is `sh' for '#' comments")
	, multi = False
		&= help "multi-line comment style (default is single); if the target language\
			\ lacks multiline symbols, then the single-line symbol is used"
	, uncomment = False
		&= help "uncomment the text; you only need to specify the particular language\
			\ --- nox will take care of both the language's single and multiline\
			\ symbols"
	, sline = [] &= typ "STRING"
		&= help "custom single-line comment string; overrides `-l' option"
	, after_lw = False &= help "for single-line comments, should we prepend it at the beginning of the line, or add it just after any leading whitespace?"
	}
	where
	names :: String
	names = intercalate ", "
		$ zipWith (\a b -> a ++ " " ++ b)
			(map (enclose sQuotes . ldExt) langs)
			(map (enclose parens . show) langs)

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
		, "  echo \"a\\nab\\nabc\" | nox -l c"
		, ""
		,  "//a"
		,  "//ab"
		,  "//abc"
		, ""
		, "  echo \"a\\nab\\nabc\" | nox -l c -m"
		, ""
		,  "/*"
		,  "a"
		,  "ab"
		,  "abc"
		,  "*/"
		]
\end{code}

The options are defined by the \ct{Opts} data type, and we write our version of it with \ct{progOpts}.
We add some more customizations to how \ct{nox} will behave (e.g., \ct{-h} and \ct{-v} flags) with \ct{getOpts}.
This is standard practice for writing command line options with the \ct{CmdArgs} library.

\begin{code}
argsCheck :: Opts -> IO (Opts, Int)
argsCheck opts = return . (,) opts =<< argsCheck' opts
	where
	argsCheck' :: Opts -> IO (Int)
	argsCheck' Opts{..}
		| not (elem lang langs) = errMsg "unsupported language" >> return 1
		| multi && noMulti && noSingle
			= errMsg "--multi requested, but language supports neither multiline nor\
				\ single-line comments"
			>> return 1
		| otherwise = return 0
		where
		noMulti = T.null . uncurry T.append $ ldCmtM lang
		noSingle = T.null $ ldCmtS lang
\end{code}

Check for errors, and return an error code (a positive number) if any errors are found.
