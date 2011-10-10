module Hub.Commands
    ( defaultGlobalHub
    , _name
    , _path
    , _xml
    , _init
    , _cp
    , _mv
    , _rm
    ) where

import		 Char
import qualified Data.ByteString          as B
import           System.Cmd
import           System.Exit
import           System.FilePath
import           System.Directory
import           Text.Printf
import           Hub.System
import           Hub.Hub
import           Hub.Parse


defaultGlobalHub :: IO Hub
defaultGlobalHub =
     do hn <- trim `fmap` readAFile defaultHubPath
        checkHubName GlbHT hn
        hf <- case isGlobal hn of
                True  -> return $ globalHubPath hn
                False -> userHubPath hn
        parse hn hf

_name :: Hub -> IO ()
_name hub = putStrLn $ name__HUB hub

_path :: Hub -> IO ()
_path hub = putStrLn $ path__HUB hub

_xml :: Hub -> IO ()
_xml hub =
     do bs <- B.readFile $ path__HUB hub
        putStr $ map (toEnum.fromEnum) $ B.unpack bs

_init :: Hub -> HubName -> IO ()
_init hub0 hn = 
     do userHubAvailable hn
        createHubDirs
        (h_fp,p_fp) <- userHubPaths hn
        pkg_init hub0 p_fp
        let hub = hub0 { name__HUB=hn, path__HUB=h_fp, usr_dbHUB=Just p_fp }
        dump hub 

_cp :: Hub -> HubName -> IO ()
_cp hub hn =
     do db0 <- not_global hub
        createHubDirs
        (h_fp,db) <- userHubPaths hn
        cpFileDir db0 db
        let hub' = hub { name__HUB=hn, path__HUB=h_fp, usr_dbHUB=Just db }
        pkg_recache hub' db
        dump hub'

_mv :: Hub -> HubName -> IO ()
_mv hub hn =
     do db0 <- not_global hub
        (h_fp,db) <- userHubPaths hn
        mvFileDir db0 db
        removeFile $ path__HUB hub
        let hub' = hub { name__HUB=hn, path__HUB=h_fp, usr_dbHUB=Just db }
        dump hub'

_rm :: Hub -> IO ()
_rm hub =
     do _ <- not_global hub
        (h_fp,db) <- userHubPaths $ name__HUB hub
        removeFile h_fp
        removeRF   db

pkg_init :: Hub -> FilePath -> IO ()
pkg_init hub fp =
     do ec <- rawSystem ghc_pkg ["init",fp]
        case ec of
          ExitSuccess   -> return ()
          ExitFailure n -> ioError $ userError $
                                printf "ghc-pkg failure (return code=%d)" n  
      where
        ghc_pkg = hc_binHUB hub </> "ghc-pkg"

pkg_recache :: Hub -> FilePath -> IO ()
pkg_recache hub fp = 
     do ec <- rawSystem ghc_pkg ["recache","-f",fp]
        case ec of
          ExitSuccess   -> return ()
          ExitFailure n -> ioError $ userError $
                                printf "ghc-pkg failure (return code=%d)" n  
      where
        ghc_pkg = hc_binHUB hub </> "ghc-pkg"

not_global :: Hub -> IO FilePath
not_global hub =
        case usr_dbHUB hub of
          Nothing -> ioError $ userError $
                                printf "%s: is a global hub" $name__HUB hub
          Just db -> return db

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
