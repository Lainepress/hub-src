module Hub.SaveLoad
    ( save
    , load
    ) where
    
import           Hub.Hub
import           Hub.PackageDB

    
save :: Hub -> FilePath -> IO ()
save hub fp = undefined hub fp

load :: Hub -> FilePath -> IO ([PkgNick],[PkgNick])
load hn fp = undefined hn fp 
