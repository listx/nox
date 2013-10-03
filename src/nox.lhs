\section{nox.lhs}

\begin{code}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Exit
import System.IO

import NOX.Command
import NOX.Option
import NOX.Util

main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	hSetBuffering stderr NoBuffering
	opts <- getOpts
	main' opts

main' :: Opts -> IO ()
main' opts@Opts{..} = do
	(opts', cmdsErrNo) <- cmdsCheck opts
	when (cmdsErrNo > 0) $ exitWith $ ExitFailure cmdsErrNo
	text <- T.hGetContents stdin
	when (T.null text) $ abort ("nothing from STDIN", 1)
	T.putStr $ (if uncomment then remCmt else makeCmt) opts' text
\end{code}
