module Hub.SaveLoad
    ( save
    , load
    ) where
    
import           Data.Char
import qualified Data.Map               as Map
import           Hub.Hub
import           Hub.PackageDB

    
save :: Hub -> IO ()
save hub =
     do pdb <- packageDB hub
        let cts = unlines $ hdr : map (prettyPkgNick . iden2nick) (Map.keys pdb)
        putStr cts
      where
        hdr = "^=" ++ maybe "" id (usr_ghHUB hub)

load :: IO (Maybe (HubName,[PkgNick]))
load = 
     do cts <- getContents
        return $ parse_har cts

parse_har :: String -> Maybe (HubName,[PkgNick])
parse_har cts =
     do (gh,r) <- hdr_p $ pp cts
        (,) gh `fmap` bdy_p r 
        
hdr_p :: [String] -> Maybe (HubName,[String])
hdr_p []       = Nothing
hdr_p (ln:lns) = 
        case words ln of
          ['^':'=':hn]
            | isHubName hn == Just GlbHK
                -> Just (hn,lns)
          _     -> Nothing

bdy_p :: [String] -> Maybe [PkgNick]
bdy_p []       = Just []
bdy_p (ln:lns) =
        case words ln of
          [w] | Just nk <- parsePkgNick' w ->
             do nks <- bdy_p lns
                return $ nk:nks
          _ ->  Nothing

pp :: String -> [String]
pp = filter (not . all isSpace) . map sc . lines
      where
        sc ln = foldr cmt "" ln
        
        cmt '#' _ = ""
        cmt '-' t = case t of
                      '-':_ -> ""
                      _     -> '-':t
        cmt c   t = c:t
