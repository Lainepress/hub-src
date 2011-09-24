module Hub.System
    ( setEnv
    , exec
    , fileExists
    ) where

#if mingw32_HOST_OS==1
setEnv :: String -> String -> Bool -> IO ()
setEnv = undefined

exec :: String -> Bool -> [String] -> Maybe [(String,String)] -> IO ()
exec = undefined

fileExists :: FilePath -> Bool
fileExists = undefined
#else
import System.Posix.Env
import System.Posix.Process
--import System.Posix.Files

exec :: String -> Bool -> [String] -> Maybe [(String,String)] -> IO ()
exec = executeFile

fileExists :: FilePath -> IO Bool
fileExists = undefined
#endif
