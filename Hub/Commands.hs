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
    , _lock
    , _unlock
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
import           Data.List
import qualified Data.Map           as Map
import           Control.Monad
import           System.FilePath
import           System.Directory
import           Text.Printf
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
        putStr $ unlines $ [ printf "%-20s %s -- %s" nm lk co | hub<-hubs, 
                                let nm = name__HUB hub, 
                                let co = commntHUB hub,
                                let lk = if lockedHUB hub then "L" else " " ]
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
    [ printf "%s %s %s %s"                      name hs lk cmt                              ] ++
    [ printf "   GHC              : %s" hc                | Just hc <- [bin2toolchain hc_bin] ] ++
    [ printf "   Haskell Platform : %s" hp                | Just hp <- [db2platform   glb_db] ] ++ 
    [ printf "   Tools            : %s"         hc_bin                                      ] ++
    [        "   Package DBs"                             |                         True      ] ++
    [ printf "      global        : %-50s (%s)" glb_db gh |                         True      ] ++
    [ printf "      user          : %-50s (%s)" usr_db uh | Just usr_db <- [mb_ud], True      ]
  where
    hs     = case sourceHUB hub of
               ClHS -> ""
               EvHS -> "[ENV]"
               DrHS -> "[DIR]"
               DuHS -> "[HME]"
               DsHS -> "[SYS]"
    lk     = case lockedHUB hub of
               True  -> "[LOCKED]"
               False -> "        "
    gh     = case usr_ghHUB hub of
               Nothing -> name
               Just hn -> hn
    uh     = name
    cmt    = if null cmt0 then "" else "-- " ++ cmt0
    name   = name__HUB hub
    cmt0   = commntHUB hub
    hc_bin = hc_binHUB hub
    glb_db = glb_dbHUB hub
    mb_ud  = usr_dbHUB hub

_lock :: Hub -> IO ()
_lock hub = lock True hub

_unlock :: Hub -> IO ()
_unlock hub = lock False hub

lock :: Bool -> Hub -> IO ()
lock lck hub =
        case usr_dbHUB hub of
          Nothing -> oops HubO msg
          Just dr -> 
             do cs <- filter isc `fmap` getDirectoryContents dr
                lockFileDir True lck dr
                mapM_ (lockFileDir False lck) [dr</>c|c<-cs]
                dump $ hub { lockedHUB = lck }
      where
        isc fp = case reverse fp of
                   'f':'n':'o':'c':'.':_ -> True
                   'e':'h':'c':'a':'c':_ -> True
                   _                     -> False

        msg    = printf "%s: cannot (un)lock a global hub" $ name__HUB hub

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

_save :: Hub -> IO ()
_save = save

_load :: HubName -> IO ()
_load hn = 
     do _   <- checkHubName [UsrHK] hn
        thr <- doesHubExist         hn
        mb  <- case thr of
                 True  -> Just `fmap` discover (Just hn)
                 False -> return Nothing
        pd  <- prep_load hn mb True 
        let hub = hubPD pd
            sps = surPD pd
            mps = msgPD pd
            aps = allPD pd
        case sps of
          []  -> return ()
          _:_ -> _erase hub sps True
        case sps++mps of
          []  -> return ()
          _:_ -> _install hub aps

_verify :: Hub -> Bool -> IO ()
_verify hub sf =
     do _  <- checkHubName [UsrHK] $ name__HUB hub
        pd <- prep_load (name__HUB hub) (Just hub) False
        let sps = surPD pd
            mps = msgPD pd
        case (sf,sps) of
          (True,_:_) -> oops_s sps
          _          -> return ()
        case mps of
          []  -> return ()
          _:_ -> oops_m mps
      where
        oops_s _ = oops HubO "verify: failed: surplus/missing packages"           -- TODO: diagnostics
        oops_m _ = oops HubO "verify: failed: missing packages"                   -- TODO: diagnostics

data PkgDiffs = PD { hubPD :: Hub, surPD, msgPD, allPD :: [PkgNick] }
                                                                deriving (Show)

prep_load :: HubName -> Maybe Hub -> Bool -> IO PkgDiffs
prep_load hn mb_hub0 lo =
     do mb_prs   <- load
        (gh,nks) <- case mb_prs of
                       Nothing -> oops HubO $ "parse error"
                       Just pr -> return pr
        mb_hub <-
            case mb_hub0 of
              Nothing  -> return Nothing 
              Just hub -> case usr_ghHUB hub of
                            Just gh' | gh==gh' -> return $ Just hub
                                     | lo      -> deleteHub hub >> return Nothing   -- TODO: diagnostics
                            _                  -> oops HubO "global hub mismatch"   -- TODO: diagnostics
        g_hub <- discover $ Just gh
        hub   <- case mb_hub of
                   Nothing  -> createHub' False g_hub hn False
                   Just hub -> return hub
        nks0 <- (map iden2nick . Map.keys) `fmap` packageDB hub
        return $ PD hub (nks0\\nks) (nks\\nks0) nks
        
_install :: Hub -> [PkgNick] -> IO ()
_install hub pkns =
     do notLocked hub
        execP HubO (EE InheritRS InheritRS []) FullMDE hub CabalP
                                            ("install":map prettyPkgNick pkns)

_erase :: Hub -> [PkgNick] -> Bool -> IO ()
_erase hub pkns0 ff = 
     do notLocked hub
        (pkns,d_pkns) <- eraseClosure hub pkns0
        putStr "Packages requested to be deleted:\n"
        putStr $ unlines $ map fmt pkns
        putStr "Dependent packages also to be deleted:\n"
        putStr $ unlines $ map fmt d_pkns
        go <-   case ff of
                  True  ->
                        return True
                  False ->
                     do putStr "Would you like to delete these packages? [n]\n"
                        yn <- getLine
                        return $ map toLower yn `elem` ["y","yes"]
        case go of
          True  ->  
             do mapM_ unreg $ pkns ++ d_pkns
                putStr "package(s) deleted.\n"
          False -> putStrLn "No modules deleted."
      where
        fmt   pkn = "  " ++ prettyPkgNick pkn

        unreg pkn = execP HubO (EE InheritRS DiscardRS []) UserMDE hub
                                    Ghc_pkgP ["unregister",prettyPkgNick pkn]
