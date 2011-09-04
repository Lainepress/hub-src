module Distribution.Hub.Parse
    ( parse
    , Hub(..)
    , HubName
    ) where


import qualified Text.XML.Expat.Annotated


type HubName = String

data Hub = HUB {
    handleHUB :: HubName,
    locatnHUB :: FilePath,
    hc_binHUB :: FilePath,
    cb_binHUB :: FilePath,
    hp_binHUB :: Maybe FilePath,
    glb_dbHUB :: FilePath,
    usr_dbHUB :: Maybe FilePath
    }                                                           deriving (Show)

parse :: FilePath -> IO Hub
parse fp = undefined fp
