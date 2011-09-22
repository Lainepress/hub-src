module Hub.Hub
    ( HubName
    , Hub(..)
    , homeHub
    , defaultHubPath
    , isGlobal
    , isUserHub
    , globalHubPath
    , userHubPath
    , checkHubName
    , userHubAvailable
    , createUserHub
    , copyUserHub
    , renameUserHub
    , removeUserHub
    ) where


type HubName = String

data Hub = HUB {
    handleHUB :: HubName,
    locatnHUB :: FilePath,
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
isGlobal = undefined







isUserHub :: HubName -> IO Bool
isUserHub = undefined


globalHubPath :: HubName -> FilePath
globalHubPath = undefined

userHubPath :: HubName -> FilePath
userHubPath = undefined



checkHubName :: HubName -> IO ()
checkHubName = undefined



userHubAvailable :: HubName -> IO ()
userHubAvailable = undefined



createUserHub :: HubName -> IO ()
createUserHub = undefined

copyUserHub :: Hub -> HubName -> IO ()
copyUserHub = undefined

renameUserHub :: Hub -> HubName -> IO ()
renameUserHub = undefined

removeUserHub :: Hub -> IO ()
removeUserHub  = undefined
