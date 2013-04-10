{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (when)
import Data.List (intercalate)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Console.CmdArgs.Implicit
import System.Exit
import System.IO

import DC.Util

cmtSingle :: Language -> T.Text
cmtSingle l = case l of
	C -> "//"
	EmacsLisp -> ";"
	Haskell -> "--"
	HTML -> ""
	Lilypond -> "%"
	Shell -> "#"
	TeX -> "%"

cmtMulti :: Language -> (T.Text, T.Text)
cmtMulti l = case l of
	C -> ("/*", "*/")
	EmacsLisp -> ("/*", "*/")
	Haskell -> ("{-", "-}")
	HTML -> ("<!--", "-->")
	Lilypond -> ("%{", "%}")
	Shell -> ("", "")
	TeX -> ("", "")

cmtLangExt :: Language -> String
cmtLangExt l = case l of
	C -> "c"
	EmacsLisp -> "el"
	Haskell -> "hs"
	HTML -> "html"
	Lilypond -> "ly"
	Shell -> "sh"
	TeX -> "tex"

data Language
	= C
	| EmacsLisp
	| Haskell
	| HTML
	| Lilypond
	| Shell
	| TeX
	deriving (Data, Eq, Enum, Show, Typeable)

langs :: [Language]
langs = enumFrom C

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

_PROGRAM_NAME, _PROGRAM_VERSION, _PROGRAM_INFO, _COPYRIGHT :: String
_PROGRAM_NAME = "dc"
_PROGRAM_VERSION = "0.0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_COPYRIGHT = "(C) Linus Arver 2013"

main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	hSetBuffering stderr NoBuffering
	opts <- getOpts
	main' opts

main' :: Opts -> IO ()
main' opts@Opts{..} = do
	(opts', cmdsErrNo) <- cmdsCheck opts
	when (cmdsErrNo > 0) $ exitWith $ ExitFailure cmdsErrNo
	text <- T.hGetContents stdin
	when (T.null text) $ abort ("nothing from STDIN", 1)
	T.putStr $ (if uncomment then remCmt else makeCmt) opts' text

cmdsCheck :: Opts -> IO (Opts, Int)
cmdsCheck opts = do
	errVal <- cmdsCheck' opts
	return (opts, errVal)

cmdsCheck' :: Opts -> IO (Int)
cmdsCheck' Opts{..}
	| not (elem lang langs) = errMsg "unsupported language" >> return 1
	| otherwise = return 0

makeCmt :: Opts -> T.Text -> T.Text
makeCmt Opts{..} str
	| multi = flip T.append mcbn $ T.append mcan str
	| otherwise = T.unlines . map (T.append (cmtSingle lang)) $ T.lines str
	where
	(mca, mcb) = cmtMulti lang
	mcan = T.append mca "\n"
	mcbn = T.append mcb "\n"

remCmt :: Opts -> T.Text -> T.Text
remCmt Opts{..} str
	| multi = T.unlines . init . tail $ T.lines str
	| otherwise = T.unlines . map (T.drop n) $ T.lines str
	where
	n = T.length $ cmtSingle lang
