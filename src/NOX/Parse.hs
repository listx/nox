{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NOX.Parse where

import qualified Data.Text.Lazy as T
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text.Lazy

-- | Find x in the string; skip any leading whitespace. Return the line without
-- the (leading) comment chars.
slineDetect :: String -> Parser String
slineDetect x = do
	leadSpace <- many $ oneOf " \t"
	_ <- string x
	rest <- manyTill anyChar eof
	return $ leadSpace ++ rest

mlineDetect :: (String, String) -> Parser String
mlineDetect (a, b) = do
	beg <- manyTill anyChar . lookAhead $ string a
	mid <- between (string a) (string b) (manyTill anyChar . lookAhead . try $ string b)
	end <- manyTill anyChar eof
	return $ beg ++ mid ++ end

-- | Check if a single line comment exists; if so, return the uncommented
-- version of that line.
slineCommentExists :: T.Text -> T.Text -> (Bool, T.Text)
slineCommentExists src slineCmtStr
	| T.null slineCmtStr = (False, T.empty)
	| otherwise = case parse (slineDetect slineCmtStr') [] src of
		Left _ -> (False, T.empty)
		Right str -> (True, T.pack str)
	where
	slineCmtStr' = T.unpack slineCmtStr

mlineCommentExists :: T.Text -> (T.Text, T.Text) -> (Bool, T.Text)
mlineCommentExists src (a, b)
	| T.null a && T.null b = (False, T.empty)
	| otherwise = case parse (mlineDetect (a', b')) [] src of
		Left _ -> (False, T.empty)
		Right str -> (True, T.pack str)
	where
	(a', b') = (T.unpack a, T.unpack b)
