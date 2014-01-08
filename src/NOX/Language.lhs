\section{NOX/Language.lhs}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module NOX.Language where

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
\end{code}

\ct{Language} defines the language types recognized by \ct{nox}.

\begin{code}
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

The \ct{LangDesc} class defines comment strings and also the extension type of a particular language.
