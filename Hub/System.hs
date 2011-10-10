module Hub.System
    ( setEnv
    , exec
    , fileExists
    , removeRF
    , cpFileDir
    , mvFileDir
    , readAFile
    ) where

import qualified Data.ByteString as B
import Hub.Oops


#if mingw32_HOST_OS==1

setEnv :: String -> String -> Bool -> IO ()
setEnv = winstub

exec :: String -> Bool -> [String] -> Maybe [(String,String)] -> IO ()
exec = winstub

fileExists :: FilePath -> IO Bool
fileExists = winstub

removeRF :: FilePath -> IO ()
removeRF = winstub

cpFileDir :: FilePath -> FilePath -> IO ()
cpFileDir = winstub

mvFileDir :: FilePath -> FilePath -> IO ()
mvFileDir = winstub

--fileAvailable :: FilePath -> IO Bool
--fileAvailable = winstub

winstub :: a
winstub = error "winstub"

#else

import System.Cmd
import System.Exit
import System.Posix.Env
import System.Posix.Process
import System.Posix.Files
import Text.Printf

exec :: String -> Bool -> [String] -> Maybe [(String,String)] -> IO ()
exec = executeFile

fileExists :: FilePath -> IO Bool
fileExists fp = flip catch (\_->return False) $
     do st <- getFileStatus fp
        return $ isRegularFile st

removeRF :: FilePath -> IO ()
removeRF fp =
     do ec <- rawSystem "rm" ["-rf",fp]
        case ec of
          ExitSuccess   -> return ()
          ExitFailure n -> oops SysO $
                                printf "rm failure (return code=%d)" n  

cpFileDir :: FilePath -> FilePath -> IO ()
cpFileDir fp fp' =
     do ec <- rawSystem "cp" ["-a",fp,fp']
        case ec of
          ExitSuccess   -> return ()
          ExitFailure n -> oops SysO $
                                printf "cp failure (return code=%d)" n  

mvFileDir :: FilePath -> FilePath -> IO ()
mvFileDir fp fp' =
     do ec <- rawSystem "mv" [fp,fp']
        case ec of
          ExitSuccess   -> return ()
          ExitFailure n -> oops SysO $
                                printf "mv failure (return code=%d)" n  

#endif

readAFile :: FilePath -> IO String
readAFile fp =
     do bs <- B.readFile fp
        return $ map (toEnum.fromEnum) $ B.unpack bs 
