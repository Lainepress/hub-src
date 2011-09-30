module Hub.Prog (_prog) where

import System.FilePath
import Text.Printf
import Hub.System
import Hub.Hub
import Hub.CommandLine


_prog :: Hub -> Prog -> [String] -> IO ()
_prog hub prg as =
     do set_pkg_path hub
        pth <- prog_path hub prg
        exec pth False as Nothing


set_pkg_path :: Hub -> IO ()
set_pkg_path hub = setEnv "GHC_PACKAGE_PATH" pth True
      where
        pth        = maybe glb mk_pth $ usr_dbHUB hub
        
        mk_pth usr = printf "%s:%s" usr glb

        glb        = glb_dbHUB hub

prog_path :: Hub -> Prog -> IO FilePath
prog_path hub prog =
        case typPROG prog of
          HcPT -> return $ hc_binHUB hub </> nmePROG prog
          CiPT -> return $ ci_binHUB hub </> nmePROG prog
          HpPT -> chk_hp $ \hp_bin -> return $ hp_bin </> nmePROG prog
      where
        chk_hp f = maybe nhp_err f $ hp_binHUB hub
        
        nhp_err  = ioError $ userError $
                        printf "Hub %s does not hava a Haskell Platform"
                                                            (name__HUB hub) 
