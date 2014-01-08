\section{NOX/Parse.lhs}

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NOX.Parse where

import qualified Data.Text.Lazy as T
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text.Lazy

slineDetect :: String -> Parser String
slineDetect x = do
	leadSpace <- many $ oneOf " \t"
	_ <- string x
	rest <- manyTill anyChar eof
	return $ leadSpace ++ rest
\end{code}

\ct{slineDetect} tries to find the given needle string \ct{x}.
We make sure to skip any leading whitespace, and then return the line without the (leading) comment characters.

\begin{code}
mlineDetect :: (String, String) -> Parser String
mlineDetect (a, b) = do
	beg <- manyTill anyChar . lookAhead $ string a
	mid <- between (string a) (string b) (manyTill anyChar . lookAhead . try $ string b)
	end <- manyTill anyChar eof
	return $ beg ++ mid ++ end
\end{code}

This is just like \ct{slineDetect}, but for multiline comment strings.

\begin{code}
slineCommentExists :: T.Text -> T.Text -> (Bool, T.Text)
slineCommentExists src slineCmtStr
	| T.null slineCmtStr = (False, T.empty)
	| otherwise = case parse (slineDetect slineCmtStr') [] src of
		Left _ -> (False, T.empty)
		Right str -> (True, T.pack str)
	where
	slineCmtStr' = T.unpack slineCmtStr
\end{code}

Here we check if a single line comment exists; if so, we return the uncommented version of that line.

\begin{code}
mlineCommentExists :: T.Text -> (T.Text, T.Text) -> (Bool, T.Text)
mlineCommentExists src (a, b)
	| T.null a && T.null b = (False, T.empty)
	| otherwise = case parse (mlineDetect (a', b')) [] src of
		Left _ -> (False, T.empty)
		Right str -> (True, T.pack str)
	where
	(a', b') = (T.unpack a, T.unpack b)
\end{code}

This is the multi-line counterpart of \ct{slineCommentExists}.
