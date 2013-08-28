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

abort :: (String, Int) -> IO ()
abort (msg, eid) = do
    errMsg msg
    hPutStrLn stderr "operation aborted"
    exitWith $ ExitFailure eid

errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg
