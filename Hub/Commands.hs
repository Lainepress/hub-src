module Hub.Commands
    ( defGlobalHub
    , _name
    , _path
    , _xml
    , _init
    , _cp
    , _mv
    , _rm
    ) where

import System.Cmd
import System.Exit
import System.FilePath
import Text.Printf
import Hub.Hub
import Hub.Parse


defGlobalHub :: IO Hub
defGlobalHub = undefined


_name :: Hub -> IO ()
_name = undefined

_path :: Hub -> IO ()
_path = undefined

_xml :: Hub -> IO ()
_xml = undefined


_init :: Hub -> HubName -> IO ()
_init hub0 hn = 
     do userHubAvailable hn
        createHubDirs
        (h_fp,p_fp) <- userHubPaths hn
        pkg_init hub0 p_fp
        let hub = hub0 { name__HUB=hn, path__HUB=h_fp, usr_dbHUB=Just p_fp }
        dump hub 

_cp :: Hub -> HubName -> IO ()
_cp = undefined

_mv :: Hub -> HubName -> IO ()
_mv = undefined

_rm :: Hub -> IO ()
_rm  = undefined


pkg_init :: Hub -> FilePath -> IO ()
pkg_init hub fp =
     do ec <- rawSystem ghc_pkg ["init",fp]
        case ec of
          ExitSuccess   -> return ()
          ExitFailure n -> ioError $ userError $
                                printf "ghc-pkg failure (return code=%d)" n  

      where
        ghc_pkg = hc_binHUB hub </> "ghc-pkg"
