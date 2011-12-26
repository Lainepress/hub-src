module Hub.Commands
    ( _default
    , _default_hub
    , _ls
    , _get
    , _set
    , _unset
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

import           Control.Monad
import           System.Directory
import           Text.Printf
import           Hub.FilePaths
import           Hub.Oops
import           Hub.System
import           Hub.Hub
import           Hub.Directory
import           Hub.Discover


_default :: IO ()
_default = defaultGlobalHubName >>= putStrLn

_default_hub :: Maybe Hub -> IO ()
_default_hub Nothing   = fileExists defaultHubPath >>= \ex -> when ex $ removeFile defaultHubPath
_default_hub (Just hub) = is_global hub >> writeAFile defaultHubPath (name__HUB hub)

is_global :: Hub -> IO ()
is_global hub = when (kind__HUB hub/=GlbHK) $
                    oops HubO $ printf "%s: not a global hub" $name__HUB hub


_ls :: IO ()
_ls = do hns <- lsHubs [UsrHK,GlbHK]; putStr $ unlines hns

_get :: IO ()
_get =
     do yup <- fileExists ".hub"
        case yup of
          False -> putStrLn "No hub set for this directory"
          True  -> 
             do hn <- trim `fmap` readAFile ".hub"
                _ <- checkHubName [UsrHK,GlbHK] hn
                putStrLn hn
                
_set :: Hub -> IO ()
_set hub = writeAFile ".hub" $ name__HUB hub ++ "\n"

_unset ::   IO ()
_unset =
     do yup <- fileExists ".hub"
        case yup of
          False -> putStrLn "No hub set for this directory"
          True  -> removeFile ".hub"

_name :: Hub -> IO ()
_name hub = putStrLn $ name__HUB hub

_info :: Hub -> IO ()
_info hub = putStr $ unlines $
    [ printf "%s (%s hub)"                     name ht                                                             ] ++
    [ printf "   GHC              : %s" hc             |                         Just hc <- [bin2toolchain hc_bin] ] ++
    [ printf "   Haskell Platform : %s" hp             | Just usr_db <- [mb_ud], Just hp <- [db2platform   usr_db] ] ++ 
    [ printf "   Tools            : %s"        hc_bin                                                              ] ++
    [        "   Package DBs"                          |                         hk/=GlbHK                         ] ++
    [ printf "      global        : %s"        glb_db  |                         hk/=GlbHK                         ] ++
    [ printf "      user          : %s"        usr_db  | Just usr_db <- [mb_ud], hk/=GlbHK                         ]
  where
    ht     = if hk==GlbHK then "global" else "user"
    name   = name__HUB hub
    hk     = kind__HUB hub
    hc_bin = hc_binHUB hub
    glb_db = glb_dbHUB hub
    mb_ud  = usr_dbHUB hub

_path :: Hub -> IO ()
_path hub = putStrLn $ path__HUB hub

_xml :: Hub -> IO ()
_xml hub = readAFile (path__HUB hub) >>= putStr

_init :: Hub -> HubName -> IO ()
_init hub0 hn = initDirectory >> createHub False hub0 hn 

_cp :: Hub -> HubName -> IO ()
_cp hub hn = initDirectory >> createHub True hub hn

{-     
     
     do initHub
     
     initDirectory
        _    <- not_global hub
        lib0 <- fst `fmap` hubUserLib hub
        (h_fp,lib,db) <- userHubPaths hn
        cpFileDir lib0 lib
        let hub' = hub { name__HUB=hn, path__HUB=h_fp, usr_dbHUB=Just db }
        dump hub'
-}

_mv :: Hub -> HubName -> IO ()
_mv hub hn = initDirectory >> renameHub hub hn

{-
     do initDirectory
        _    <- not_global hub
        lib0 <- fst `fmap` hubUserLib hub
        (h_fp,lib,db) <- userHubPaths hn
        mvFileDir lib0 lib
        removeFile $ path__HUB hub
        let hub' = hub { name__HUB=hn, path__HUB=h_fp, usr_dbHUB=Just db }
        dump hub'
-}

_rm :: Hub -> IO ()
_rm = deleteHub
{-
     do _ <- not_global hub
        (h_fp,lib,_) <- userHubPaths $ name__HUB hub
        removeFile h_fp
        removeRF   lib
-}

_swap :: Hub -> HubName -> IO ()
_swap hub1 hn2 =
     do hub2 <- discover $ Just hn2
        swapHub hub1 hub2
     
{-
        _    <- not_global hub1
        _    <- not_global hub2
        lib1 <- x_lib `fmap` userHubPaths (name__HUB hub1)
        lib2 <- x_lib `fmap` userHubPaths (name__HUB hub2)
        let hub1' = hub1 {name__HUB=name__HUB hub2,path__HUB=path__HUB hub2,usr_dbHUB=usr_dbHUB hub2}
            hub2' = hub2 {name__HUB=name__HUB hub1,path__HUB=path__HUB hub1,usr_dbHUB=usr_dbHUB hub1}
        dump hub1'
        dump hub2'
        swap_files lib1 lib2
      where
        x_lib (_,lib,_) = lib
-}

