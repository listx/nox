module DC.Util where

import System.Exit
import System.IO

dquote :: String -> String
dquote s = " \"" ++ s ++ "\" "

squote :: String -> String
squote s = " `" ++ s ++ "' "

paren :: String -> String
paren s = " (" ++ s ++ ") "

abort :: (String, Int) -> IO ()
abort (msg, eid) = do
    errMsg msg
    hPutStrLn stderr "operation aborted"
    exitWith $ ExitFailure eid

errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg
