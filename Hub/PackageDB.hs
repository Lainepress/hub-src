module Hub.PackageDB
    ( PkgName
    , eraseClosure
    , importLibraryDirs
    , Package(..)
    , PRef(..)
    , packageDB
    ) where

import           Data.Char
import qualified Data.Map               as Map
import qualified Data.ByteString        as B
import qualified Data.ByteString.UTF8   as U
import           Text.Printf
import           System.Directory
import           Hub.System
import           Hub.Oops
import           Hub.Hub


type PkgName = String


eraseClosure :: Hub -> [PkgName] -> IO [PkgName]
eraseClosure hub pkns = undefined hub pkns packageDB

importLibraryDirs :: Hub -> IO [FilePath]
importLibraryDirs hub = undefined hub packageDB



data Package = PKG {
    refPKG          :: PRef,
    import_dirsPKG  :: [FilePath],
    library_dirsPKG :: [FilePath],
    dependsPKG      :: [PRef]
    }                                                           deriving (Show)

-- PkgName represents a medium length package identifier: <name>-<version>.

data PRef = PR {
    namePR :: String,
    vrsnPR :: String,
    pkidPR :: String
    }                                                           deriving (Show)

packageDB :: Hub -> IO (Map.Map PkgName Package)
packageDB hub =
     do cts <- package_dump hub
        return $ packages [ package $ record rec | rec<-records cts ]

package_dump :: Hub -> IO String
package_dump hub =
     do tf <- tmpFile "pkg-dump.txt"
        putStrLn $ printf "DEBUG: dumping to %s" tf
        db <- case usr_dbHUB hub of
                Nothing -> oops HubO $ printf "%s: user hub expected" $ name__HUB hub
                Just db -> return db
        let pc_a = printf "--package-conf=%s" db
        ghcPkg (EC (RedirctRS tf) DiscardRS) hub [pc_a,"dump"]
        bs <- B.readFile tf
        removeFile tf
        return $ U.toString bs
        
packages :: [Package] -> Map.Map PkgName Package
packages pkgs = Map.fromList [(pr2pkn $ refPKG pkg,pkg) | pkg<-pkgs ]
      where
        pr2pkn pr = namePR pr ++ "-" ++ vrsnPR pr
        
package :: Map.Map String [String] -> Package
package mp = PKG
        (PR (lu "name" sgl) (lu "version" sgl) (lu "id" sgl))
        (lu "import-dirs"  lst)
        (lu "library-dirs" lst)
        (lu "depends"      prs)
      where
        lu ky f = f $ Map.lookup ky mp
                  
        sgl Nothing    = ""
        sgl (Just lns) = trim $ unlines lns

        lst Nothing    = []
        lst (Just lns) = map trim lns

        prs Nothing    = []
        prs (Just lns) = map (s2pr . trim) lns

s2pr :: String -> PRef
s2pr s = PR (reverse r_nm) (reverse r_vr) s
      where
        r_nm        = cl_d rst2
        (r_vr,rst2) = break ('-'==) $ cl_d rst1
        (_   ,rst1) = break ('-'==) $ reverse s

        cl_d ('-':t) = t
        cl_d x       = x

records :: String -> [[String]]
records cts = rec [] $ lines cts
      where
        rec acc []       = [reverse acc]
        rec acc (ln:lns) =
                case ln of
                  ""            -> rec acc lns
                  _ | ln=="---" -> reverse acc:rec [] lns
                    | otherwise -> rec (ln:acc) lns

record :: [String] -> Map.Map String [String]
record lns = Map.fromList $ fields lns

fields :: [String] -> [(String,[String])]
fields []       = []
fields (ln:lns) = (tag,rst:cnt):fields lns'
      where
        (tag,rst)   = case words ln of
                        []   -> ("","")
                        w:ws -> (cln w,unwords ws)

        (cnt,lns')  = span chk lns
        
        chk []      = False
        chk (c:_) = isSpace c

        cln tg      = case reverse tg of
                        ':':r_tg -> reverse r_tg
                        _        -> tg

        