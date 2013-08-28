{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NOX.Command where

import qualified Data.Text.Lazy as T

import NOX.Data
import NOX.Option
import NOX.Parse

-- | Comment text. For single-line comments, simply prepend each non-empty line
-- with the comment string. For multiline comments, prepend and append lines
-- containing just the multiline comment string pairs.
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
		| not (null sline') = T.append (T.pack sline') l
		| otherwise = T.append (ldCmtS lang) l

-- | Uncomment text. For single-line mode, remove all single-line comment
-- characters found on each line independently of other lines. Continuous,
-- trailing characters that are the same as the last single-line comment
-- character are also removed (e.g., "//" is removed but so is "///" or
-- "///////". For multiline comments, simply find and remove the mulitline
-- comment string pairs.
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
