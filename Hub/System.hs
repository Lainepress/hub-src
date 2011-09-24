module Hub.System
    ( setEnv
    , executeFile
    , fileExists
    ) where

#if mingw32_HOST_OS==1
setEnv :: String -> String -> Bool -> IO ()
setEnv = undefined

executeFile :: String -> Bool -> [String] -> Maybe [(String,String)] -> IO ()
executeFile = undefined

fileExists :: FilePath -> Bool
fileExists = undefined
#else
import System.Posix.Env
import System.Posix.Process
import System.Posix.Files

fileExists :: FilePath -> IO Bool
fileExists = undefined
#endif
