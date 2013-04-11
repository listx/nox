{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DC.Command where

import qualified Data.Text.Lazy as T

import DC.Data
import DC.Option
import DC.Parse

-- | Comment text. For single-line comments, simply prepend each line with the
-- comment string. For multiline comments, prepend and append lines containing
-- just the multiline comment string pairs.
makeCmt :: Opts -> T.Text -> T.Text
makeCmt Opts{..} src
	| multi = flip T.append mcbn $ T.append mcan src
	| otherwise = T.unlines . map (T.append (cmtSingle lang)) $ T.lines src
	where
	(mca, mcb) = cmtMulti lang
	mcan = T.append mca "\n"
	mcbn = T.append mcb "\n"

-- | Uncomment text. For single-line mode, remove all single-line comment
-- characters found on each line independently of other lines. Continuous,
-- trailing characters that are the same as the last single-line comment
-- character are also removed (e.g., "//" is removed but so is "///" or
-- "///////". For multiline comments, simply find and remove the mulitline
-- comment string pairs.
remCmt :: Opts -> T.Text -> T.Text
remCmt Opts{..} src
	| multi = case mlineCommentExists src $ cmtMulti lang of
		(True, removed) -> removed
		(False, _) -> src
	| otherwise = T.unlines . map tryRemSlineComment $ T.lines src
	where
	tryRemSlineComment :: T.Text -> T.Text
	tryRemSlineComment rawline = if scmtExists
		then removed
		else rawline
		where
		(scmtExists, removed) = slineCommentExists rawline $ cmtSingle lang
