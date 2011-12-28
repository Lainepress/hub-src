module Hub.System
    ( tmpFile
    , setEnv
    , fileExists
    , fileDirExists
    , removeRF
    , cpFileDir
    , mvFileDir
    , symLink
    , inc
    , tidyDir
    , exec
    , go
    , ExecCtrl(..)
    , RedirectStream(..)
    , execProg
    , readAFile
    , writeAFile
    ) where

import qualified Data.ByteString as B
import Hub.Oops
import System.Process


#if mingw32_HOST_OS==1

dev_null :: FilePath
dev_null = winstub

tmpFile :: IO Int
tmpFile = winstub 

setEnv :: String -> String -> Bool -> IO ()
setEnv = winstub

fileExists :: FilePath -> IO Bool
fileExists = winstub

fileDirExists :: FilePath -> IO Bool
fileDirExists = winstub

removeRF :: FilePath -> IO ()
removeRF = winstub

cpFileDir :: FilePath -> FilePath -> IO ()
cpFileDir = winstub

mvFileDir :: FilePath -> FilePath -> IO ()
mvFileDir = winstub

symLink :: FilePath -> FilePath -> IO ()
symLink = winstub

inc :: FilePath -> IO Int
inc = winstub 

go :: [String] -> FilePath -> IO a
go = winstub

exec :: [String] -> FilePath -> IO ExitCode
exec = winstub

tidyDir :: FilePath -> IO ()
tidyDir = winstub

--fileAvailable :: FilePath -> IO Bool
--fileAvailable = winstub

winstub :: a
winstub = error "winstub"

#else

import           Control.Monad
import qualified Control.Exception      as E
import           System.Directory
import System.IO
import System.Exit
import System.Posix.Env
import System.Posix.Files
import System.Posix.Process
import System.Posix.IO
import Text.Printf


dev_null :: FilePath
dev_null = "/dev/null"

-- allocate a temporary file

tmpFile :: FilePath -> IO FilePath
tmpFile fn =
     do pid <- getProcessID
        return $ printf "/tmp/hub-%d-%s" (fromIntegral pid :: Int) fn

fileExists :: FilePath -> IO Bool
fileExists fp = flip E.catch (hdl_ioe False) $
     do st <- getFileStatus fp
        return $ isRegularFile st

fileDirExists :: FilePath -> IO Bool
fileDirExists fp = flip E.catch (hdl_ioe False) $
     do st <- getFileStatus fp
        return $ isRegularFile st || isDirectory st
        
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


symLink :: FilePath -> FilePath -> IO ()
symLink = createSymbolicLink


go :: [String] -> FilePath -> IO ()
go as exe   = executeFile exe True as Nothing

exec :: [String] -> FilePath -> IO ExitCode
exec as exe = rawSystem exe as

inc :: FilePath -> IO Int
inc fp = 
     do fd <- openFd fp ReadWrite (Just stdFileMode) defaultFileFlags
        -- putStrLn "acquiring lock"
        waitToSetLock fd (WriteLock,AbsoluteSeek,0,0)
        --putStrLn "lock acquired"
        i <- rd_i fd
        _ <- fdSeek fd AbsoluteSeek 0
        _ <- fdWrite fd $ show $ i+1
        closeFd fd
        return i
      where
        rd_i fd =
             do (s,_) <- E.catch (fdRead fd 64) (hdl_ioe ("",0))
                return $ maybe 0 id $ readMB s

tidyDir :: FilePath -> IO ()
tidyDir dp = flip E.catch (hdl_ioe ()) $
     do e <- all dots `fmap` getDirectoryContents dp 
        when e $ removeDirectory dp
      where
        dots "."  = True
        dots ".." = True
        dots _    = False
        
readMB :: Read a => String -> Maybe a
readMB str =
    case [ x | (x,t)<-reads str, ("","")<-lex t ] of
      [x] -> Just x
      _   -> Nothing

hdl_ioe :: a -> IOError -> IO a
hdl_ioe x _ = return x

#endif

data ExecCtrl = EC {
    redirctOutEC :: RedirectStream,
    redirctErrEC :: RedirectStream
    }                                                           deriving (Show)

data RedirectStream
    = InheritRS
    | DiscardRS
    | RedirctRS FilePath
                                                                deriving (Show)

execProg :: ExecCtrl -> FilePath -> [String] -> IO ExitCode
execProg ec pr as =
     do so <- get_ss $ redirctOutEC ec
        se <- get_ss $ redirctErrEC ec
        let cp = (proc pr as) { std_out = so, std_err = se }
        (_,_,_,ph) <- createProcess cp
        ex <- waitForProcess ph
        clse so
        clse se
        return ex
      where
        get_ss  InheritRS     = return Inherit
        get_ss  DiscardRS     = get_ss (RedirctRS dev_null)
        get_ss (RedirctRS fp) = UseHandle `fmap` openFile fp WriteMode

        clse (UseHandle h) = hClose h
        clse _             = return ()

readAFile :: FilePath -> IO String
readAFile fp =
     do bs <- B.readFile fp
        return $ map (toEnum.fromEnum) $ B.unpack bs 

writeAFile :: FilePath -> String -> IO ()
writeAFile = writeFile
-- writeAFile fp cts = B.writeFile fp $ B.pack $ map (toEnum.fromEnum) cts
