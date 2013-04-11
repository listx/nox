{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DC.Command where

import qualified Data.Text.Lazy as T

import DC.Data
import DC.Option

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
