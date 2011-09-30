module Hub.Hub
    ( HubName
    , Hub(..)
    , homeHub
    , defaultHubPath
    , checkHubName
    , userHubAvailable
    , isGlobal
    , isUserHub
    , globalHubPath
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





defaultHubPath :: FilePath
defaultHubPath = "/usr/hs/hub/defaultHub"




isGlobal :: HubName -> Bool
isGlobal (c:_) = isDigit c
isGlobal _     = False




-- tests for presence of user Hub

checkHubName :: HubName -> IO ()
checkHubName hn =
        case hn of
          c:cs | first_hubname_c c && all hubname_c cs
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


globalHubPath :: HubName -> IO FilePath
globalHubPath hn = return $ printf "/usr/hs/hub/%s.xml" hn

userHubPath :: HubName -> IO FilePath
userHubPath hn = fst `fmap` userHubPaths hn

userHubPaths :: HubName -> IO (FilePath,FilePath)
userHubPaths hn = 
     do hme <- home
        return ( printf "%s/.hub/hub/%s.xml" hme hn
               , printf "%s/.hub/db/%s"      hme hn
               )

first_hubname_c, hubname_c :: Char -> Bool
first_hubname_c c = c `elem` "_." || isAlpha c || isDigit c -- FIXME:TODO:REVIEW: should this sometimes be checking that user hub names are being used
hubname_c       c = c `elem` "_." || isAlpha c || isDigit c


createHubDirs :: IO ()
createHubDirs =
     do hme <- home
        let hub = printf "%s/.hub/hub" hme
            db  = printf "%s/.hub/db"  hme
        createDirectoryIfMissing True  hub
        createDirectoryIfMissing False db

home :: IO FilePath
home = catch (getEnv "HOME") $ \_ -> return "/"

