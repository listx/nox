\begin{code}
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

class LangDesc a where
	ldCmtS :: a -> T.Text
	ldCmtM :: a -> (T.Text, T.Text)
	ldExt :: a -> String

instance LangDesc Language where
	ldCmtS l = case l of
		C -> "//"
		EmacsLisp -> ";"
		Haskell -> "--"
		HTML -> x
		Lilypond -> "%"
		Shell -> "#"
		TeX -> "%"
		where
		x = T.empty
	ldCmtM l = case l of
		C -> ("/*", "*/")
		EmacsLisp -> x
		Haskell -> ("{-", "-}")
		HTML -> ("<!--", "-->")
		Lilypond -> ("%{", "%}")
		Shell -> x
		TeX -> x
		where
		x = (T.empty, T.empty)
	ldExt l = case l of
		C -> "c"
		EmacsLisp -> "el"
		Haskell -> "hs"
		HTML -> "html"
		Lilypond -> "ly"
		Shell -> "sh"
		TeX -> "tex"
\end{code}
