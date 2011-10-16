module Hub.Hub
    ( HubName
    , Hub(..)
    , HubType(..)
    , homeHub
    , hubBin
    , defaultCabalBin
    , defaultHubPath
    , globalHubDir
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

import           Data.Maybe
import           Data.List
import           Char
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

globalHubDir, hubBin, defaultCabalBin, defaultHubPath :: FilePath
hc_bin_res, hp_bin_res :: String
globalHubDir     = "/usr/hs/hub"
hubBin           = "/usr/hs/bin"
defaultCabalBin  = "/usr/hs/cabal" 
defaultHubPath   = "/usr/hs/default.hub"
hc_bin_res       = "/usr/hs/ghc/([a-z0-9.-_]+)/bin"
hp_bin_res       = "/usr/hs/hp/([a-z0-9.-_]+)/bin"

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
defaultGlobalHubName = 
     do ds <- fileExists defaultHubPath
        case ds of
          True  ->
             do hn <- trim `fmap` readAFile defaultHubPath
                checkHubName GlbHT hn
                return hn
          False ->
             do hns <- lsHubs GlbHT
                case default_hub hns of
                  Nothing -> oops SysO "no global hubs!"
                  Just hn -> return hn
      where
        trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

hubExists :: HubName -> IO ()
hubExists hn =
     do hf <- case isGlobal hn of
                True  -> return $ globalHubPath hn
                False -> userHubPath hn
        ok <- fileExists hf
        case ok of
          True  -> return ()
          False -> oops SysO $ printf "%s: no such hub" hn

default_hub :: [HubName] -> Maybe HubName
default_hub hns0 =
        case filter hp_hub hns of
          [] -> case hns of
                  []   -> Nothing
                  hn:_ -> Just hn
          hn:_ -> Just hn
      where
        hns     = sortBy dec hns0
        
        dec x y = case compare x y of
                    GT -> LT
                    EQ -> EQ
                    LT -> GT

        hp_hub = isJust . matchRegex hp_hub_re 
        
hp_hub_re :: Regex
hp_hub_re = mk_re "20[0-9][0-9]\\.[0-9]\\.[0-9]\\.[0-9]"


lsHubs :: HubType -> IO [HubName]
lsHubs GlbHT = ls_glb_hubs
lsHubs UsrHT = ls_usr_hubs
lsHubs AnyHT =
     do g_hns <- ls_glb_hubs
        u_hns <- ls_usr_hubs 
        return $ g_hns ++ u_hns

ls_glb_hubs :: IO [HubName]
ls_glb_hubs = chk `fmap` getDirectoryContents globalHubDir
      where
        chk fps = [ hn | fp<-fps, Just hn<-[fp2hn fp], is_hub_name GlbHT hn ]

ls_usr_hubs :: IO [HubName]
ls_usr_hubs = 
     do dp <- fst `fmap` userHubDirs
        chk `fmap` getDirectoryContents dp
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
hubname_c     _     c = c `elem` "_." || isAlpha c || isDigit c

glb_first_hub_name_c, usr_first_hub_name_c :: Char -> Bool
glb_first_hub_name_c c = isDigit c
usr_first_hub_name_c c = c `elem` "_." || isAlpha c



bin2toolchain, bin2platform :: FilePath -> Maybe String
bin2toolchain = match $ mk_re hc_bin_res
bin2platform  = match $ mk_re hp_bin_res



mk_re :: String -> Regex
mk_re re_str = mkRegexWithOpts (printf "^%s$" re_str) False True

match :: Regex -> String -> Maybe String
match re st = case matchRegex re st of
                Just [se] -> Just se
                _         -> Nothing 


home :: IO FilePath
home = catch (getEnv "HOME") $ \_ -> return "/"

