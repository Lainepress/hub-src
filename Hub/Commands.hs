module Hub.Commands
    ( _prog
    , _default
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
    , _comment
    , _cp
    , _mv
    , _rm
    , _swap
    , _gc
    , _list
    , _check
    , _load
    , _save
    , _verify
    , _install
    , _erase
    ) where

import           Data.Char
import           Control.Monad
import           System.Directory
import           Text.Printf
--import qualified Data.Map           as Map
import           Hub.FilePaths
import           Hub.Oops
import           Hub.System
import           Hub.Prog
import           Hub.Hub
import           Hub.PackageDB
import           Hub.Directory
import           Hub.Parse
import           Hub.Discover
import           Hub.SaveLoad



_prog :: Hub -> Prog -> [String] -> IO ()
_prog = execProg HubO (EE InheritRS InheritRS []) FullMDE


_default :: IO ()
_default = defaultGlobalHubName >>= putStrLn

_default_hub :: Maybe Hub -> IO ()
_default_hub Nothing   = fileExists defaultHubPath >>= \ex -> when ex $ removeFile defaultHubPath
_default_hub (Just hub) = is_global hub >> writeAFile defaultHubPath (name__HUB hub)

is_global :: Hub -> IO ()
is_global hub = when (kind__HUB hub/=GlbHK) $
                    oops HubO $ printf "%s: not a global hub" $name__HUB hub


_ls :: Bool -> IO ()
_ls af =
     do hns  <- lsHubs [UsrHK,GlbHK]
        hubs <- mapM (discover . Just) $ filter f hns
        putStr $ unlines $ [ printf "%-20s -- %s" nm co | hub<-hubs, 
                                let nm = name__HUB hub, let co = commntHUB hub ]
      where
        f ('_':'_':_) = af
        f _           = True

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
    [ printf "%s %s %s"                        name hs cmt                                 ] ++
    [ printf "   GHC              : %s" hc             | Just hc <- [bin2toolchain hc_bin] ] ++
    [ printf "   Haskell Platform : %s" hp             | Just hp <- [db2platform   glb_db] ] ++ 
    [ printf "   Tools            : %s"        hc_bin                                      ] ++
    [        "   Package DBs"                          |                         True      ] ++
    [ printf "      global        : %s"        glb_db  |                         True      ] ++
    [ printf "      user          : %s"        usr_db  | Just usr_db <- [mb_ud], True      ]
  where
    hs     = case sourceHUB hub of
               ClHS -> ""
               EvHS -> "[ENV]"
               DrHS -> "[DIR]"
               DuHS -> "[HME]"
               DsHS -> "[SYS]"
    cmt    = if null cmt0 then "" else "-- " ++ cmt0
    name   = name__HUB hub
    cmt0   = commntHUB hub
    hc_bin = hc_binHUB hub
    glb_db = glb_dbHUB hub
    mb_ud  = usr_dbHUB hub

_path :: Hub -> IO ()
_path hub = putStrLn $ path__HUB hub

_xml :: Hub -> IO ()
_xml hub = readAFile (path__HUB hub) >>= putStr

_init :: Hub -> HubName -> Bool -> IO ()
_init hub0 hn sf =
     do initDirectory
        hub <- createHub' False hub0 hn sf
        when sf $ _set hub

_comment :: Hub -> String -> IO ()
_comment hub cmt = dump $ hub { commntHUB = cmt }

_cp :: Hub -> HubName -> IO ()
_cp hub hn = initDirectory >> createHub True hub hn

_mv :: Hub -> HubName -> IO ()
_mv hub hn = initDirectory >> renameHub hub hn

_rm :: Hub -> IO ()
_rm = deleteHub

_swap :: Hub -> HubName -> IO ()
_swap hub1 hn2 =
     do hub2 <- discover $ Just hn2
        swapHub hub1 hub2

_gc :: IO () 
_gc = gcDefaultDirectory discover VerboseGCM

_list :: Hub -> IO ()
_list hub = execP HubO (EE InheritRS InheritRS []) FullMDE hub Ghc_pkgP ["list"]

_check :: Hub -> IO ()
_check hub = execP HubO (EE InheritRS InheritRS []) FullMDE hub Ghc_pkgP ["check"]

_save :: Hub -> FilePath -> IO ()
_save = save

_load :: HubName -> FilePath -> Bool -> IO ()
_load hn fp ef = 
     do hub <- undefined hn
        (eps,ips) <- load hub fp
        case (ef,eps) of
          (True,_:_) -> _erase hub eps
          _          -> return ()
        case ips of
          []  -> return ()
          _:_ -> _install hub ips

_verify :: Hub -> FilePath -> Bool -> IO ()
_verify hub fp sf =
     do (eps,ips) <- load hub fp
        case (sf,eps) of
          (True,_:_) -> oops_e eps ips
          _          -> return ()
        case ips of
          []  -> return ()
          _:_ -> oops_i ips
      where
        oops_e _ _ = oops HubO "verify: failed: excess/missing packages" -- undefined
        oops_i _   = oops HubO "verify: failed: missing packages" -- undefined

_install :: Hub -> [PkgNick] -> IO ()
_install hub pkns = execP HubO (EE InheritRS InheritRS []) FullMDE hub CabalP
                                                ("install":map prettyPkgNick pkns)

_erase :: Hub -> [PkgNick] -> IO ()
_erase hub pkns0 = 
     do (pkns,d_pkns) <- eraseClosure hub pkns0
        putStr "Packages requested to be deleted:\n"
        putStr $ unlines $ map fmt pkns
        putStr "Dependent packages also to be deleted:\n"
        putStr $ unlines $ map fmt d_pkns
        putStr "Would you like to delete these packages? [n]\n"
        yn <- getLine
        case map toLower yn `elem` ["y","yes"] of
          True  ->  
             do mapM_ unreg $ pkns ++ d_pkns
                putStr "package(s) deleted.\n"
          False -> putStrLn "No modules deleted."
      where
        fmt   pkn = "  " ++ prettyPkgNick pkn

        unreg pkn = execP HubO (EE InheritRS DiscardRS []) UserMDE hub
                                    Ghc_pkgP ["unregister",prettyPkgNick pkn]
