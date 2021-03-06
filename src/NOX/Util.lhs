\section{NOX/Util.lhs}

\begin{code}
module NOX.Util where

import System.Exit
import System.IO

enclose :: (String, String) -> String -> String
enclose (a, b) w = a ++ w ++ b

enclose' :: (String, String) -> String -> String
enclose' pair = enclose sSpaces . enclose pair

dQuotes
	, sQuotes
	, parens
	, bracks
	, braces
	, sSpaces :: (String, String)
dQuotes = ("\"", "\"")
sQuotes = ("`", "'")
parens = ("(", ")")
bracks = ("[", "]")
braces = ("{", "}")
sSpaces = (" ", " ")
\end{code}

These two functions make it easier to surround text with a given pair of strings.

\begin{code}
abort :: (String, Int) -> IO ()
abort (msg, eid) = do
    errMsg msg
    hPutStrLn stderr "operation aborted"
    exitWith $ ExitFailure eid

errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg
\end{code}

These are some basic error-logging/exiting functions.
