module Hub.PackageDB
    ( PRef(..)
    , PackageDB(..)
    , packageDB
    ) where
    
data PRef = PR {
    namePR :: String,
    vrsnPR :: String,
    hashPR :: String
    }                                                           deriving (Show)

data PackageDB = PDB {
    refPDB          :: PRef,
    import_dirsPDB  :: [FilePath],
    library_dirsPDB :: [FilePath],
    dependsPDB      :: [PRef]
    }                                                           deriving (Show)

packageDB :: FilePath -> PackageDB
packageDB fp = undefined
