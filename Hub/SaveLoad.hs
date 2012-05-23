module Hub.SaveLoad
    ( save
    , load
    ) where
    
import qualified Data.Map               as Map
import           Hub.Hub
import           Hub.PackageDB

    
save :: Hub -> FilePath -> IO ()
save hub fp =
     do pdb <- packageDB hub
        let cts = unlines $ hdr : map (prettyPkgNick . iden2nick) (Map.keys pdb)
        case fp of
          "-" -> putStr cts
          _   -> writeFile fp cts
      where
        hdr = "^=" ++ maybe "" id (usr_ghHUB hub)

load :: FilePath -> IO (Maybe (HubName,[PkgNick]))
load fp = 
     do cts <- case fp of
                 "-" -> getContents
                 _   -> readFile fp
        return $ parse_har cts

parse_har :: String -> Maybe (HubName,[PkgNick])
parse_har cts0 = undefined cts0
{-

      where
        
        cts = pp_har cts0
        
hdr :: String -> POSS String (HubName,String)
-- check global hub
-}      