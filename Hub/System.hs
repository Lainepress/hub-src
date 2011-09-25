module Hub.System
    ( setEnv
    , exec
    , fileExists
    ) where

#if mingw32_HOST_OS==1
setEnv :: String -> String -> Bool -> IO ()
setEnv = winstub

exec :: String -> Bool -> [String] -> Maybe [(String,String)] -> IO ()
exec = winstub

fileExists :: FilePath -> IO Bool
fileExists = winstub

winstub :: a
winstub = error "winstub"

#else
import System.Posix.Env
import System.Posix.Process
import System.Posix.Files

exec :: String -> Bool -> [String] -> Maybe [(String,String)] -> IO ()
exec = executeFile

fileExists :: FilePath -> IO Bool
fileExists fp = flip catch (\_->return False)
     do fst <- getFileStatus fp
        return $ isRegularFile fst
#endif
