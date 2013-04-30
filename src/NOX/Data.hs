{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module NOX.Data where

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
	HTML -> x
	Lilypond -> "%"
	Shell -> "#"
	TeX -> "%"
	where
	x = T.empty

cmtMulti :: Language -> (T.Text, T.Text)
cmtMulti l = case l of
	C -> ("/*", "*/")
	EmacsLisp -> x
	Haskell -> ("{-", "-}")
	HTML -> ("<!--", "-->")
	Lilypond -> ("%{", "%}")
	Shell -> x
	TeX -> x
	where
	x = (T.empty, T.empty)

cmtLangExt :: Language -> String
cmtLangExt l = case l of
	C -> "c"
	EmacsLisp -> "el"
	Haskell -> "hs"
	HTML -> "html"
	Lilypond -> "ly"
	Shell -> "sh"
	TeX -> "tex"
