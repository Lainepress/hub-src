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
    , defaultGlobalHubName
    , lsHubs
    , isGlobal
    , globalHubPath
    , checkHubName
    , userHubAvailable
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
            db  = printf "%s/.hubrc/db"  hme
        return (hub,db)



hn2fp :: HubName -> FilePath
hn2fp = (++ ".xml")

hn2db :: HubName -> FilePath
hn2db = id

fp2hn :: FilePath -> Maybe HubName
fp2hn = match xml_fn_re

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
     do iuh <- isUserHub hn
        case iuh of
          True  -> oops SysO "%s: hub already in use"
          False -> return ()


isUserHub :: HubName -> IO Bool
isUserHub hn = userHubPath hn >>= fileExists


createHubDirs :: IO ()
createHubDirs =
     do (hub,db) <- userHubDirs 
        createDirectoryIfMissing True  hub
        createDirectoryIfMissing False db

userHubPath :: HubName -> IO FilePath
userHubPath hn = fst `fmap` userHubPaths hn

userHubPaths :: HubName -> IO (FilePath,FilePath)
userHubPaths hn = 
     do (hub,db) <- userHubDirs 
        return (hub </> hn2fp hn,db</>hn2db hn)


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

