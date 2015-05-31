\section{NOX/Core.lhs}

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NOX.Core where

import qualified Data.Text.Lazy as T

import NOX.Language
import NOX.Option
import NOX.Parse

makeCmt :: Opts -> T.Text -> T.Text
makeCmt Opts{..}
	| multi = flip T.append mcbn . T.append mcan
	| otherwise = T.unlines . map (makeSingleComment sline) . T.lines
	where
	(mca, mcb) = ldCmtM lang
	mcan = T.append mca "\n"
	mcbn = T.append mcb "\n"
	makeSingleComment sline' l
		| T.null l = l
		| not (null sline') = if after_lw
			then (\(a, b) -> T.concat [a, T.pack sline', b])
				$ T.span (flip elem (" \t" :: String)) l
			else T.append (T.pack sline') l
		| otherwise = T.append (ldCmtS lang) l
\end{code}

\ct{makeCmt} comments out the given chunk of text.
For single-line comments, simply prepend each non-empty line with the comment string.
For multiline comments, we prepend and append lines containing just the multiline comment string pairs.

\begin{code}
remCmt :: Opts -> T.Text -> T.Text
remCmt Opts{..} src
	| multi = case mlineCommentExists src $ ldCmtM lang of
		(True, removed) -> removed
		(False, _) -> src
	| otherwise = T.unlines . map (tryRemSlineComment sline) $ T.lines src
	where
	tryRemSlineComment :: String -> T.Text -> T.Text
	tryRemSlineComment sline' rawline = if scmtExists
		then removed
		else rawline
		where
		(scmtExists, removed) = slineCommentExists rawline $
			if null sline'
				then ldCmtS lang
				else T.pack sline'
\end{code}

\ct{remCmt} uncomments a given chunk of (probably commented) text.
For single-line mode, remove all single-line comment characters found on each line independently of other lines.
For multiline comments, we find and remove the mulitline comment string pairs.
