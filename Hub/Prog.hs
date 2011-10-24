module Hub.Prog (_prog) where

import Control.Monad
import System.Exit
import System.FilePath
import System.Environment
import Text.Printf
import Hub.Oops
import Hub.System
import Hub.Hub
import Hub.CommandLine


_prog :: Hub -> Prog -> [String] -> IO ()
_prog hub prog as =
     do set_hub_pkg_path hub
        case typPROG prog of
          HcPT    -> go                     as $ hc_binHUB hub </> nmePROG prog
          CiPT mb -> ci_go mb               as $ ci_binHUB hub </> nmePROG prog
          HpPT    -> chk_hp $ \hp_bin -> go as $ hp_bin        </> nmePROG prog
      where
        chk_hp f = maybe nhp_err f $ hp_binHUB hub
        
        nhp_err  = oops PrgO $
                        printf "Hub %s does not hava a Haskell Platform"
                                                            (name__HUB hub) 

set_hub_pkg_path :: Hub -> IO ()
set_hub_pkg_path hub = 
     do pth <- mk_pth `fmap` getEnv "PATH" 
        setEnv                              "HUB"              hnm True
        when (not $ isGlobalHub hub) $
             do setEnv                      "GHC_PACKAGE_PATH" ppt True
                setEnv                      "PATH"             pth True
      where
        hnm        =                    hubName hub
        ppt        = maybe glb mk_ppt $ usr_dbHUB hub
        
        mk_pth pt0 = printf "%s:%s:%s" hubGccBin hubBinutilsBin pt0
        mk_ppt usr = printf "%s:%s"    usr                      glb

        glb        = glb_dbHUB hub

ci_go :: Maybe FilePath -> [String] -> FilePath -> IO ()
ci_go Nothing   as exe = go   as exe
ci_go (Just fp) as exe = exec as exe >>= \ec -> tidyDir fp >> exitWith ec
