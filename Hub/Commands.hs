module Hub.Commands
    ( _default
    , _default_hub
    , _ls
    , _name
    , _info
    , _path
    , _xml
    , _init
    , _cp
    , _mv
    , _rm
    , _swap
    ) where

import           System.Cmd
import           System.Exit
import           System.FilePath
import           System.Directory
import           Text.Printf
import           Hub.Oops
import           Hub.System
import           Hub.Hub
import           Hub.Parse


_default :: IO ()
_default = defaultGlobalHubName >>= putStrLn

_default_hub :: Maybe Hub -> IO ()
_default_hub Nothing   = removeRF defaultHubPath
_default_hub (Just hb) = is_global hb >>= writeAFile defaultHubPath

_ls :: IO ()
_ls = do hns <- lsHubs AnyHT; putStr $ unlines hns

_name :: Hub -> IO ()
_name hub = putStrLn $ name__HUB hub

_info :: Hub -> IO ()
_info hub = putStr $ unlines $
    [ printf "%s (%s hub)"                              name ht                                                             ] ++
    [ printf "   Toolchain       : GHC %s"              hc      |                         Just hc <- [bin2toolchain hc_bin] ] ++
    [ printf "   Tools           : %s"                  hc_bin                                                              ] ++
    [ printf "   Platform        : Haskell Platform %s" hp      | Just hp_bin <- [mb_hp], Just hp <- [bin2platform  hp_bin] ] ++ 
    [ printf "   Platform Tools  : %s"                  hp_bin  | Just hp_bin <- [mb_hp]                                    ] ++ 
    [ printf "   Cabal           : %s/cabal"            ci_bin
    ,        "   Package DBs"
    , printf "      global       : %s"                  glb_db                                                              ] ++
    [ printf "      user         : %s"                  usr_db  | Just usr_db <- [mb_ud]                                    ]
  where
    ht     = if isGlobal name then "global" else "user"
    name   = name__HUB hub
    hc_bin = hc_binHUB hub
    ci_bin = ci_binHUB hub
    mb_hp  = hp_binHUB hub
    glb_db = glb_dbHUB hub
    mb_ud  = usr_dbHUB hub

_path :: Hub -> IO ()
_path hub = putStrLn $ path__HUB hub

_xml :: Hub -> IO ()
_xml hub = readAFile (path__HUB hub) >>= putStr

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

_swap :: Hub -> HubName -> IO ()
_swap hub1 hn2 =
     do hub2 <- readHub hn2
        _    <- not_global hub1
        _    <- not_global hub2
        (h_fp1,db1) <- userHubPaths $ name__HUB hub1
        (h_fp2,db2) <- userHubPaths $ name__HUB hub2
        swap_files  h_fp1 h_fp2 
        swap_files' db1   db2   (swap_files h_fp1 h_fp2)

pkg_init :: Hub -> FilePath -> IO ()
pkg_init hub fp =
     do ec <- rawSystem ghc_pkg ["init",fp]
        case ec of
          ExitSuccess   -> return ()
          ExitFailure n -> oops HubO $
                                printf "ghc-pkg failure (return code=%d)" n  
      where
        ghc_pkg = hc_binHUB hub </> "ghc-pkg"

pkg_recache :: Hub -> FilePath -> IO ()
pkg_recache hub fp = 
     do ec <- rawSystem ghc_pkg ["recache","-f",fp]
        case ec of
          ExitSuccess   -> return ()
          ExitFailure n -> oops HubO $
                                printf "ghc-pkg failure (return code=%d)" n  
      where
        ghc_pkg = hc_binHUB hub </> "ghc-pkg"

not_global :: Hub -> IO FilePath
not_global hub =
        case usr_dbHUB hub of
          Nothing -> oops HubO $ printf "%s: is a global hub" $name__HUB hub
          Just db -> return db

is_global :: Hub -> IO HubName
is_global hub =
        case usr_dbHUB hub of
          Nothing -> return                                    $ name__HUB hub
          Just  _ -> oops HubO $ printf "%s: not a global hub" $ name__HUB hub

swap_files :: FilePath -> FilePath -> IO ()
swap_files fp fp' = swap_files'' fp fp' $ oops SysO

swap_files' :: FilePath -> FilePath -> IO () -> IO ()
swap_files' fp fp' tdy = swap_files'' fp fp' $ \err -> tdy >> oops SysO err

swap_files'' :: FilePath -> FilePath -> (String->IO ()) -> IO ()
swap_files'' fp fp' h = catch (sw_files fp fp') $ \_ -> h err
      where
        err = printf "Failed to swap %s and %s (permissions?)" fp fp' 

sw_files :: FilePath -> FilePath -> IO ()
sw_files fp_1 fp_2 =
     do fp_t <- mk_tmp 0 fp_1
        mvFileDir fp_1 fp_t
        mvFileDir fp_2 fp_1
        mvFileDir fp_t fp_2

mk_tmp :: Int -> FilePath -> IO FilePath
mk_tmp i fp =
     do yup <- fileDirExists fp'
        case yup of
          True  -> mk_tmp (i+1) fp
          False -> return fp' 
      where
        fp' = printf "%s-%d" fp i
