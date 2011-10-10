module Hub.Hub
    ( HubName
    , Hub(..)
    , homeHub
    , hubBin
    , defaultHubPath
    , globalHubPath
    , HubType(..)
    , checkHubName
    , userHubAvailable
    , isGlobal
    , isUserHub
    , userHubPath
    , userHubPaths
    , createHubDirs
    ) where


import           Char
import           System
import           System.Directory
import           Text.Printf
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


hubBin, defaultHubPath ::            FilePath
globalHubPath          :: HubName -> FilePath

hubBin           =        "/usr/hs/bin"
defaultHubPath   =        "/usr/hs/default.hub"
globalHubPath hn = printf "/usr/hs/hub/%s.xml"  hn





isGlobal :: HubName -> Bool
isGlobal (c:_) = isDigit c
isGlobal _     = False




data HubType
    = AnyHT
    | GlbHT
    | UsrHT
                                                                deriving (Show)

-- tests for presence of (user) Hub

checkHubName :: HubType -> HubName -> IO ()
checkHubName ht hn =
        case hn of
          c:cs | fst_hubname_c ht c && all (hubname_c ht) cs
            -> return ()
          _ -> ioError $ userError $ printf "%s: invalid hub name" hn   

userHubAvailable :: HubName -> IO ()
userHubAvailable hn = 
     do iuh <- isUserHub hn
        case iuh of
          True  -> ioError $ userError "%s: hub already in use"
          False -> return ()


isUserHub :: HubName -> IO Bool
isUserHub hn = userHubPath hn >>= fileExists


userHubPath :: HubName -> IO FilePath
userHubPath hn = fst `fmap` userHubPaths hn

userHubPaths :: HubName -> IO (FilePath,FilePath)
userHubPaths hn = 
     do hme <- home
        return ( printf "%s/.hub/hub/%s.xml" hme hn
               , printf "%s/.hub/db/%s"      hme hn
               )

fst_hubname_c, hubname_c :: HubType -> Char -> Bool
fst_hubname_c AnyHT c = glb_first_hub_name_c c || usr_first_hub_name_c c
fst_hubname_c GlbHT c = glb_first_hub_name_c c
fst_hubname_c UsrHT c = usr_first_hub_name_c c
hubname_c     _     c = c `elem` "_." || isAlpha c || isDigit c

glb_first_hub_name_c, usr_first_hub_name_c :: Char -> Bool
glb_first_hub_name_c c = isDigit c
usr_first_hub_name_c c = c `elem` "_." || isAlpha c


createHubDirs :: IO ()
createHubDirs =
     do hme <- home
        let hub = printf "%s/.hub/hub" hme
            db  = printf "%s/.hub/db"  hme
        createDirectoryIfMissing True  hub
        createDirectoryIfMissing False db

home :: IO FilePath
home = catch (getEnv "HOME") $ \_ -> return "/"

