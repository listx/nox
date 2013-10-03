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
\end{code}

The above are some basic imports.
The only interesting import is the \ct{when} function, which allows us to write some succinct one-liners in \ct{main}.

\begin{code}
main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	hSetBuffering stderr NoBuffering
	opts <- getOpts
	(opts', argsErrNo) <- argsCheck opts
	when (argsErrNo > 0) $ do
		errMsg $ "code " ++ show argsErrNo
		exitWith $ ExitFailure argsErrNo
	text <- T.hGetContents stdin
	when (T.null text) . abort $ (,) "nothing from STDIN" 1
	T.putStr $ (if uncomment opts' then remCmt else makeCmt) opts' text
\end{code}

This is the main program section and handles all arguments passed to \ct{nox}.
The given options are first checked for sanity before \ct{nox} does anything; if there is an error encountered, we exit prematurely.
