module Hub.Prog (_prog) where

import Control.Monad
import System.Exit
import System.FilePath
import System.Environment
import Text.Printf
import Hub.System
import Hub.Hub
import Hub.CommandLine


_prog :: Hub -> Prog -> [String] -> IO ()
_prog hub prog as =
     do set_hub_pkg_path hub
        case typPROG prog of
          HcPT    -> go       as $ hc_binHUB hub </> nmePROG prog
          CiPT mb -> ci_go mb as $ tl_binHUB hub </> nmePROG prog
          TlPT    -> go       as $ tl_binHUB hub </> nmePROG prog

set_hub_pkg_path :: Hub -> IO ()
set_hub_pkg_path hub = 
     do pth <- mk_pth `fmap` getEnv "PATH" 
        setEnv'                              "HUB"              hnm True
        when (not $ isGlobalHub hub) $
             do setEnv'                      "GHC_PACKAGE_PATH" ppt True
                setEnv'                      "PATH"             pth True
      where
        hnm        =                    hubName hub
        ppt        = maybe glb mk_ppt $ usr_dbHUB hub
        
        mk_pth pt0 = printf "%s:%s:%s" hubGccBin hubBinutilsBin pt0
        mk_ppt usr = printf "%s:%s"    usr                      glb

        glb        = glb_dbHUB hub

ci_go :: Maybe FilePath -> [String] -> FilePath -> IO ()
ci_go Nothing   as exe = go   as exe
ci_go (Just fp) as exe = exec as exe >>= \ec -> tidyDir fp >> exitWith ec

dbg :: Bool
dbg = False

setEnv' :: String -> String -> Bool -> IO ()
setEnv' var val ovw =
     do when dbg $
            putStrLn $ printf ">> %s=%s\n" var val
        setEnv var val ovw
