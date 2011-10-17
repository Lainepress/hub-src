module Hub.Hub
    ( HubName
    , Hub(..)
    , HubType(..)
    , homeHub
    , hubLib
    , sysVersion
    , sysDefaultHubPath
    , defaultHubPath
    , globalHubDir
    , hubBin
    , defaultCabalBin
    , userHubDirs
    , hubUserLib
    , defaultGlobalHubName
    , hubExists
    , lsHubs
    , isGlobal
    , globalHubPath
    , checkHubName
    , userHubAvailable
    , userHubExists
    , userHubName
    , isUserHub
    , createHubDirs
    , userHubPath
    , userHubPaths
    , bin2toolchain
    , bin2platform
    ) where

import           IO
import           Char
import           List
import           Data.Maybe
import           System
import           System.Directory
import           System.FilePath
import           Text.Printf
import           Text.Regex
import           Hub.Oops
import           Hub.System


type HubName = String


data Hub = HUB {
    name__HUB :: HubName,
    path__HUB :: FilePath,
    hc_binHUB :: FilePath,
    ci_binHUB :: FilePath,
    hp_binHUB :: Maybe FilePath,
    glb_dbHUB :: FilePath,
    usr_dbHUB :: Maybe FilePath
    }                                                           deriving (Show)


data HubType
    = AnyHT
    | GlbHT
    | UsrHT
                                                                deriving (Show)

homeHub :: FilePath
homeHub = "home"

package_config :: FilePath
package_config = "package.config"

hubLib, sysVersion, sysDefaultHubPath, defaultHubPath, 
        globalHubDir, hubBin, defaultCabalBin :: FilePath
hc_bin_res, hp_bin_res :: String
hubLib            = "/usr/hs/lib"
sysVersion        = "/usr/hs/lib/version.txt"
sysDefaultHubPath = "/usr/hs/lib/sys-default.hub"
defaultHubPath    = "/usr/hs/lib/the-default.hub"
globalHubDir      = "/usr/hs/hub"
hubBin            = "/usr/hs/bin"
defaultCabalBin   = "/usr/hs/cabal" 
hc_bin_res        = "/usr/hs/ghc/([a-z0-9.-_]+)/bin"
hp_bin_res        = "/usr/hs/hp/([a-z0-9.-_]+)/bin"

userHubDirs :: IO (FilePath,FilePath)
userHubDirs = 
     do hme <- home
        let hub = printf "%s/.hubrc/hub" hme
            lib = printf "%s/.hubrc/lib" hme
        return (hub,lib)

user_lib :: FilePath -> HubName -> FilePath
user_lib hme hn = printf "%s/.hubrc/lib/%s" hme hn

db_re :: String -> Regex
db_re hme = mk_re $ printf "%s/.hubrc/lib/([^/]*)/%s/?" hme package_config

hn2fp :: HubName -> FilePath
hn2fp = (++ ".xml")

fp2hn :: FilePath -> Maybe HubName
fp2hn = match xml_fn_re

hubUserLib :: Hub -> IO (FilePath,FilePath)
hubUserLib hub = 
     do hme <- home
        case usr_dbHUB hub of
          Nothing -> oops SysO "no user DB speceified for this hub"
          Just db -> case match (db_re hme) db of
                       Just hn | hn==name__HUB hub
                            -> return $ (user_lib hme hn,db)
                       _    -> oops SysO "hub has non-standard user-database path"

xml_fn_re :: Regex
xml_fn_re = mk_re "(.*)\\.xml"



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
                 do ei <- try $ trim `fmap` p
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

        mx_fr  p      = (filter p `fmap` lsHubs GlbHT) >>= mx 

        mx     []     = ioError $ userError "Hub.Hub: internal error"
        mx     (x:xs) = return  $ foldl max x xs
                
        hp_hub_re     = mk_re "20[0-9][0-9]\\.[0-9]\\.[0-9]\\.[0-9]"

hubExists :: HubName -> IO ()
hubExists hn =
     do hf <- case isGlobal hn of
                True  -> return $ globalHubPath hn
                False -> userHubPath hn
        ok <- fileExists hf
        case ok of
          True  -> return ()
          False -> oops SysO $ printf "%s: no such hub" hn

lsHubs :: HubType -> IO [HubName]
lsHubs GlbHT = ls_glb_hubs
lsHubs UsrHT = ls_usr_hubs
lsHubs AnyHT =
     do g_hns <- ls_glb_hubs
        u_hns <- ls_usr_hubs 
        return $ g_hns ++ u_hns

ls_glb_hubs :: IO [HubName]
ls_glb_hubs = (sort . chk) `fmap` getDirectoryContents globalHubDir
      where
        chk fps = [ hn | fp<-fps, Just hn<-[fp2hn fp], is_hub_name GlbHT hn ]

ls_usr_hubs :: IO [HubName]
ls_usr_hubs = 
     do dp <- fst `fmap` userHubDirs
        (sort . chk) `fmap` getDirectoryContents dp
      where
        chk fps = [ hn | fp<-fps, Just hn<-[fp2hn fp], is_hub_name UsrHT hn ]





isGlobal :: HubName -> Bool
isGlobal (c:_) = isDigit c
isGlobal _     = False

globalHubPath :: HubName -> FilePath
globalHubPath hn = globalHubDir </> hn2fp hn


-- tests for presence of Hub

checkHubName :: HubType -> HubName -> IO ()
checkHubName ht hn
    | is_hub_name ht hn = return ()
    | otherwise         = oops SysO $ printf "%s: invalid %shub name" hn ht_s
      where
        ht_s =  case ht of
                  AnyHT -> ""
                  GlbHT -> "global "
                  UsrHT -> "user "

is_hub_name :: HubType -> HubName -> Bool
is_hub_name ht hn =
        case hn of
          c:cs -> fst_hubname_c ht c && all (hubname_c ht) cs
          _    -> False

userHubAvailable :: HubName -> IO ()
userHubAvailable hn = 
     do userHubName hn
        iuh <- isUserHub hn
        case iuh of
          True  -> oops SysO $ printf "%s: hub already in use" hn
          False -> return ()

userHubExists :: HubName -> IO ()
userHubExists hn = 
     do userHubName hn
        hubExists   hn

userHubName :: HubName -> IO ()
userHubName hn =
        case isGlobal hn of
          False -> return ()
          True  -> oops SysO $ printf "%s: not a user hub" hn



isUserHub :: HubName -> IO Bool
isUserHub hn = userHubPath hn >>= fileExists


createHubDirs :: IO ()
createHubDirs =
     do (hub,lib) <- userHubDirs 
        createDirectoryIfMissing True hub
        createDirectoryIfMissing True lib

userHubPath :: HubName -> IO FilePath
userHubPath hn = (\(h_fp,_,_)->h_fp) `fmap` userHubPaths hn

userHubPaths :: HubName -> IO (FilePath,FilePath,FilePath)
userHubPaths hn = 
     do (hub,lib) <- userHubDirs
        let h_l = lib </> hn
        return (hub </> hn2fp hn, h_l, h_l </> package_config)


fst_hubname_c, hubname_c :: HubType -> Char -> Bool
fst_hubname_c AnyHT c = glb_first_hub_name_c c || usr_first_hub_name_c c
fst_hubname_c GlbHT c = glb_first_hub_name_c c
fst_hubname_c UsrHT c = usr_first_hub_name_c c
hubname_c     _     c = c `elem` "_-." || isAlpha c || isDigit c

glb_first_hub_name_c, usr_first_hub_name_c :: Char -> Bool
glb_first_hub_name_c c = isDigit c
usr_first_hub_name_c c = c `elem` "_." || isAlpha c



bin2toolchain, bin2platform :: FilePath -> Maybe String
bin2toolchain = match $ mk_re hc_bin_res
bin2platform  = match $ mk_re hp_bin_res



mk_re :: String -> Regex
mk_re re_str = mkRegexWithOpts (printf "^%s$" re_str) False True

gmatch :: Regex -> String -> Bool
gmatch re st = isJust $ matchRegex re st

match :: Regex -> String -> Maybe String
match re st = case matchRegex re st of
                Just [se] -> Just se
                _         -> Nothing

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace


home :: IO FilePath
home = catch (getEnv "HOME") $ \_ -> return "/"

