module Hub.Prog (_prog) where

import System.Directory
import System.FilePath
import Text.Printf
import Hub.Oops
import Hub.System
import Hub.Hub
import Hub.CommandLine


_prog :: Hub -> Prog -> [String] -> IO ()
_prog hub prog as =
     do set_pkg_path hub
        case typPROG prog of
          HcPT -> go as $ hc_binHUB hub </> nmePROG prog
          CiPT -> ci_go hub as $ ci_binHUB hub </> nmePROG prog
          HpPT -> chk_hp $ \hp_bin -> go as $ hp_bin </> nmePROG prog
      where
        chk_hp f = maybe nhp_err f $ hp_binHUB hub
        
        nhp_err  = oops PrgO $
                        printf "Hub %s does not hava a Haskell Platform"
                                                            (name__HUB hub) 

set_pkg_path :: Hub -> IO ()
set_pkg_path hub = setEnv "GHC_PACKAGE_PATH" pth True
      where
        pth        = maybe glb mk_pth $ usr_dbHUB hub
        
        mk_pth usr = printf "%s:%s" usr glb

        glb        = glb_dbHUB hub

ci_go :: Hub -> [String] -> FilePath -> IO ()
ci_go hub as exe =
     do (lkf,sdb,ddb) <- ghc_paths hub
        lock lkf $
         do removeFile  sdb
            symLink sdb ddb
            go as exe            

ghc_paths :: Hub -> IO (FilePath,FilePath,FilePath)
ghc_paths hub =
        case bin2toolchain $ hc_binHUB hub of
          Nothing  -> oops PrgO "GHC bin path not recognised"
          Just ghc -> return $ ghc_paths' ghc
          
ghc_paths' :: String -> (FilePath,FilePath,FilePath)
ghc_paths' ghc = (lkf,sdb,ddb)
      where
        lkf = lib </> "hub-cabal-lock.txt"
        sdb = lib </> mkd "package.conf"
        ddb = "/usr/hs/db" </>  mkd ghc
        lib = printf "/usr/hs/ghc/%s/lib/ghc-%s" ghc ghc

        mkd = if old then (++".d") else id 

        old = case ghc of
                '6':'.':'1':'0':'.':_ -> True
                _                     -> False

go :: [String] -> FilePath -> IO ()
go as exe = exec exe False as Nothing
