\section{nox.lhs}

\begin{code}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Exit
import System.IO

import NOX.Core
import NOX.Option
import NOX.Util

main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	hSetBuffering stderr NoBuffering
	opts <- getOpts
	(opts', argsErrNo) <- argsCheck opts
	when (argsErrNo > 0) . exitWith $ ExitFailure argsErrNo
	text <- T.hGetContents stdin
	when (T.null text) . abort $ (,) "nothing from STDIN" 1
	T.putStr $ (if uncomment opts' then remCmt else makeCmt) opts' text
\end{code}
