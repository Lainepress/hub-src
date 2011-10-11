module Hub.Hub
    ( HubName
    , Hub(..)
    , homeHub
    , hubBin
    , defaultHubPath
    , globalHubDir
    , userHubDirs
    , lsHubs
    , defaultGlobalHubName
    , globalHubPath
    , isGlobal
    , HubType(..)
    , checkHubName
    , userHubAvailable
    , isUserHub
    , createHubDirs
    , userHubPath
    , userHubPaths
    ) where


import           Char
import           System
import           System.Directory
import           Text.Printf
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


homeHub :: FilePath
homeHub = "home"

hubBin, defaultHubPath, globalHubDir :: FilePath
globalHubDir     = "/usr/hs/hub/%s.xml"
hubBin           = "/usr/hs/bin"
defaultHubPath   = "/usr/hs/default.hub"

userHubDirs :: IO (FilePath,FilePath)
userHubDirs = 
     do hme <- home
        let hub = printf "%s/.hub/hub" hme
            db  = printf "%s/.hub/db"  hme
        return (hub,db)


lsHubs :: IO [FilePath]
lsHubs = undefined


defaultGlobalHubName :: IO HubName
defaultGlobalHubName = undefined

{-
     do hn <- trim `fmap` readAFile defaultHubPath
        checkHubName GlbHT hn
        return hn
      where
        trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
-}

isGlobal :: HubName -> Bool
isGlobal (c:_) = isDigit c
isGlobal _     = False

globalHubPath :: HubName -> FilePath
globalHubPath hn = printf "%s/%s.xml" globalHubDir hn


data HubType
    = AnyHT
    | GlbHT
    | UsrHT
                                                                deriving (Show)

-- tests for presence of Hub

checkHubName :: HubType -> HubName -> IO ()
checkHubName ht hn =
        case hn of
          c:cs | fst_hubname_c ht c && all (hubname_c ht) cs
            -> return ()
          _ -> oops SysO $ printf "%s: invalid %shub name" hn ht_s
      where
        ht_s =  case ht of
                  AnyHT -> ""
                  GlbHT -> "global "
                  UsrHT -> "user "



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
        return (printf "%/%s.xml" hub hn,printf "%s/%s" db hn)




fst_hubname_c, hubname_c :: HubType -> Char -> Bool
fst_hubname_c AnyHT c = glb_first_hub_name_c c || usr_first_hub_name_c c
fst_hubname_c GlbHT c = glb_first_hub_name_c c
fst_hubname_c UsrHT c = usr_first_hub_name_c c
hubname_c     _     c = c `elem` "_." || isAlpha c || isDigit c

glb_first_hub_name_c, usr_first_hub_name_c :: Char -> Bool
glb_first_hub_name_c c = isDigit c
usr_first_hub_name_c c = c `elem` "_." || isAlpha c


home :: IO FilePath
home = catch (getEnv "HOME") $ \_ -> return "/"

