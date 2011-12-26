module Hub.Directory
    ( initDirectory
    , directoryPath
    , HubKind(..)
    , userHubExists
    , userHubAvailable
    , hubExists
    , isUserHub
    , userHubPath
    , globalHubPath
    , allocate
    
    , defaultGlobalHubName
    , lsHubs
    , bin2toolchain
    , db2platform

    , createHub
    , renameHub
    , deleteHub
    , swapHub
    
    ) where


import qualified Control.Exception      as E
import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.Cmd
import           System.Exit
import           System.FilePath
import           System.Directory
import           System.Environment
import           Text.Printf
import           Text.Regex
import           Hub.FilePaths
import           Hub.Oops
import           Hub.System
import           Hub.Hub
import           Hub.Parse


initDirectory :: IO ()
initDirectory =
     do (hub,lib) <- user_hub_dirs 
        createDirectoryIfMissing True hub
        createDirectoryIfMissing True lib

directoryPath :: IO FilePath
directoryPath = home >>= \hme -> return $ printf "%s/.hubrc" hme

-- check that a hub name denotes a named user hub that either exists or
-- or has not been created

userHubExists, userHubAvailable :: HubName -> IO ()

userHubAvailable hn = 
     do _ <- checkHubName [UsrHK] hn
        iuh <- isUserHub hn
        case iuh of
          True  -> oops SysO $ printf "%s: hub already in use" hn
          False -> return ()

userHubExists hn = 
     do _ <- checkHubName [UsrHK] hn
        hubExists   hn

hubExists :: HubName -> IO ()
hubExists hn =
     do hf <- case isHubName hn of
                Just GlbHK -> return $ globalHubPath hn
                Just UsrHK -> userHubPath hn
                Nothing    -> oops SysO $ printf "%s: bad hub name"
        ok <- fileExists hf
        case ok of
          True  -> return ()
          False -> oops SysO $ printf "%s: no such hub" hn



isUserHub :: HubName -> IO Bool
isUserHub hn = userHubPath hn >>= fileExists

userHubPath :: HubName -> IO FilePath
userHubPath hn = (\(h_fp,_,_)->h_fp) `fmap` user_hub_paths hn

globalHubPath :: HubName -> FilePath
globalHubPath hn = globalHubDir </> hn2fp hn





defaultGlobalHubName :: IO HubName
defaultGlobalHubName = sel
        [ usr_df     -- user-spec default
        , sys_df     -- system    default (may not be installed)
        , lhp_df     -- looks like the latest H.P. (guess)
        , lst_df     -- lexigographical maximum 
        ]
      where
        sel []        = oops SysO "no global hubs!"
        sel (p:ps)    =
                 do ei <- tryIO $ trim `fmap` p
                    case ei of
                      Left  _  -> sel ps
                      Right hn -> 
                         do ex <- fileExists $ globalHubPath hn
                            case ex of
                              True  -> return hn
                              False -> sel ps

        usr_df        = readAFile defaultHubPath

        sys_df        = readAFile sysDefaultHubPath

        lhp_df        = mx_fr $ gmatch hp_hub_re
        
        lst_df        = mx_fr $ const True

        mx_fr  p      = (filter p `fmap` lsHubs [GlbHK]) >>= mx 

        mx     []     = ioError $ userError "Hub.Hub: internal error"
        mx     (x:xs) = return  $ foldl max x xs
                
        hp_hub_re     = mk_re "20[0-9][0-9]\\.[0-9]\\.[0-9]\\.[0-9]"

tryIO :: IO a -> IO (Either IOError a)
tryIO = E.try

lsHubs :: [HubKind] -> IO [HubName]
lsHubs hks = concat `fmap` mapM ls (sort hks)
      where
        ls GlbHK = ls_glb_hubs
        ls UsrHK = ls_usr_hubs

ls_glb_hubs :: IO [HubName]
ls_glb_hubs = (sort . chk) `fmap` getDirectoryContents globalHubDir
      where
        chk fps = [ hn | fp<-fps, Just hn<-[fp2hn fp], isHubName hn==Just GlbHK ]

ls_usr_hubs :: IO [HubName]
ls_usr_hubs = 
     do dp <- fst `fmap` user_hub_dirs
        (sort . chk) `fmap` getDirectoryContents dp
      where
        chk fps = [ hn | fp<-fps, Just hn<-[fp2hn fp], isHubName hn==Just UsrHK ]



bin2toolchain, db2platform :: FilePath -> Maybe String
bin2toolchain = match $ mk_re hcBinREs
db2platform   = match $ mk_re hpDbREs




createHub :: Bool -> Hub -> HubName -> IO ()
createHub cp hub0 hn =
     do userHubAvailable hn
        (h_fp,lib,db) <- user_hub_paths hn
        createDirectoryIfMissing True lib
        (_,db0) <- hub_user_lib hub0
        case cp of
          True  -> cpFileDir db0 db
          False -> pkg_init hub0 db
        let hub = hub0 { name__HUB=hn, path__HUB=h_fp, usr_dbHUB=Just db }
        dump hub 

renameHub :: Hub -> HubName -> IO ()
renameHub hub0 hn =
     do not_global hub0
        (lib0,_)      <- hub_user_lib hub0
        (h_fp,lib,db) <- user_hub_paths hn
        mvFileDir lib0 lib
        removeFile $ path__HUB hub0
        let hub = hub0 { name__HUB=hn, path__HUB=h_fp, usr_dbHUB=Just db }
        dump hub

deleteHub :: Hub -> IO ()
deleteHub hub =
     do not_global hub
        (h_fp,lib,_) <- user_hub_paths $ name__HUB hub
        removeFile h_fp
        removeRF   lib

swapHub :: Hub -> Hub -> IO ()
swapHub hub1 hub2 =
     do _    <- not_global hub1
        _    <- not_global hub2
        (_,lib1,_) <- user_hub_paths (name__HUB hub1)
        (_,lib2,_) <- user_hub_paths (name__HUB hub2)
        let hub1' = hub1 {name__HUB=name__HUB hub2,path__HUB=path__HUB hub2,usr_dbHUB=usr_dbHUB hub2}
            hub2' = hub2 {name__HUB=name__HUB hub1,path__HUB=path__HUB hub1,usr_dbHUB=usr_dbHUB hub1}
        dump hub1'
        dump hub2'
        swap_files lib1 lib2







pkg_init :: Hub -> FilePath -> IO ()
pkg_init hub fp =
     do ec <- rawSystem ghc_pkg ["init",fp]
        case ec of
          ExitSuccess   -> return ()
          ExitFailure n -> oops HubO $
                                printf "ghc-pkg failure (return code=%d)" n  
      where
        ghc_pkg = hc_binHUB hub </> "ghc-pkg"

not_global :: Hub -> IO ()
not_global hub = when (kind__HUB hub==GlbHK) $
                    oops HubO $ printf "%s: is a global hub"  $name__HUB hub

{-
is_global :: Hub -> IO ()
is_global hub = when (kind__HUB hub/=GlbHK) $
                    oops HubO $ printf "%s: not a global hub" $name__HUB hub
-}




package_config :: FilePath
package_config = "package.config"




allocate :: IO FilePath
allocate =
     do hme <- home
        createDirectoryIfMissing True $ printf "%s/.hubrc/heap"             hme
        i <- inc                      $ printf "%s/.hubrc/heap/counter.txt" hme
        let pth =                       printf "%s/.hubrc/heap/%d"          hme i
        createDirectoryIfMissing True   pth
        return pth

user_lib :: FilePath -> HubName -> FilePath
user_lib hme hn = printf "%s/.hubrc/lib/%s" hme hn

db_re :: String -> Regex
db_re hme = mk_re $ printf "%s/.hubrc/lib/([^/]*)/%s/?" hme package_config

hn2fp :: HubName -> FilePath
hn2fp = (++ ".xml")

fp2hn :: FilePath -> Maybe HubName
fp2hn = match xml_fn_re

xml_fn_re :: Regex
xml_fn_re = mk_re "(.*)\\.xml"




user_hub_paths :: HubName -> IO (FilePath,FilePath,FilePath)
user_hub_paths hn = 
     do (hub,lib) <- user_hub_dirs
        let h_l = lib </> hn
        return (hub </> hn2fp hn, h_l, h_l </> package_config)

user_hub_dirs :: IO (FilePath,FilePath)
user_hub_dirs = 
     do hme <- home
        let hub = printf "%s/.hubrc/hub" hme
            lib = printf "%s/.hubrc/lib" hme
        return (hub,lib)


hub_user_lib :: Hub -> IO (FilePath,FilePath)
hub_user_lib hub = 
     do hme <- home
        case usr_dbHUB hub of
          Nothing -> oops SysO "no user DB speceified for this hub"
          Just db -> case match (db_re hme) db of
                       Just hn | hn==name__HUB hub
                            -> return $ (user_lib hme hn,db)
                       _    -> oops SysO "hub has non-standard user-database path"
        
        
swap_files :: FilePath -> FilePath -> IO ()
swap_files fp fp' = swap_files'' fp fp' $ oops SysO

--swap_files' :: FilePath -> FilePath -> IO () -> IO ()
--swap_files' fp fp' tdy = swap_files'' fp fp' $ \err -> tdy >> oops SysO err

swap_files'' :: FilePath -> FilePath -> (String->IO ()) -> IO ()
swap_files'' fp fp' h = catchIO (sw_files fp fp') $ \_ -> h err
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

home :: IO FilePath
home = catchIO (getEnv "HOME") $ \_ -> return "/"


catchIO :: IO a -> (IOError->IO a) -> IO a
catchIO = E.catch




mk_re :: String -> Regex
mk_re re_str = mkRegexWithOpts (printf "^%s$" re_str) False True

gmatch :: Regex -> String -> Bool
gmatch re st = isJust $ matchRegex re st

match :: Regex -> String -> Maybe String
match re st = case matchRegex re st of
                Just (se:_) -> Just se
                _           -> Nothing



        