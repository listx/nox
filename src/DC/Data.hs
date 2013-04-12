{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module DC.Data where

import Data.Data
import qualified Data.Text.Lazy as T

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
	EmacsLisp -> ("", "")
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
