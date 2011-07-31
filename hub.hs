
import           System.Environment
import           System.Directory
import           System.FilePath
import           System.Info
import           System.Posix.Env       (setEnv)
import           System.Posix.Process
import           Text.Printf
import           Data.Char
import qualified Data.Map               as Map


main :: IO ()
main = 
     do p <- get_prog
        putStrLn $ printf "prog : %s" $ show p 
        n <- which_hub
        putStrLn $ printf "hub  : %s" $ show n 
        h <- get_hub n
        putStrLn $ printf " =>  : %s" $ show h
        set_pkg_path h
        a <- getArgs
        putStrLn $ printf "exec %s %s" (prog_path h p) (unwords a)
        executeFile "/bin/bash" False [] Nothing

get_prog :: IO Prog
get_prog =
     do pn <- getProgName
        let (_,p_s) = splitFileName pn
        case Map.lookup p_s prog_mp of
          Nothing -> error "Oops, program not recognised"
          Just p  -> return p

type HubName = String

type GHCInst = String
type HPInst  = String

data Hub = HUB {
    ghcH   :: GHCInst,
    hpH    :: HPInst,
    s_hubH :: HubName,
    hubH   :: HubName
    }                                                           deriving (Show)


which_hub :: IO HubName
which_hub = (reverse.splitDirectories) `fmap` getCurrentDirectory >>= w_h
      where
        w_h []     = return "home"
        w_h (d:ds) = catch (here (d:ds)) (\_ -> w_h ds)
        
        here r_ds  =
             do cts <- readFile ( joinPath $ reverse $ ".hub":r_ds)  
                case words cts of
                  [w] -> return w
                  _   -> ioError $ userError "hub not here"

get_hub :: HubName -> IO Hub
get_hub hub =
     do cts <- hub_cts hub
        case map trim $ filter n_cmt $ lines cts of
          ghc:hp:s_hub:_ -> return $ HUB ghc hp s_hub hub
          _              -> error  $ "Hub " ++ hub ++ ": bad format"
      where
        n_cmt ('#':_) = False
        n_cmt ln      = not $ all isSpace ln

set_pkg_path :: Hub -> IO ()
set_pkg_path h =
     do hme <- home
        let spd = s_hub_pdb     h
            upd = hub_pdb   hme h
            pth = printf "%s:%s" upd spd
        setEnv "GHC_PACKAGE_PATH" pth True
        
prog_path :: Hub -> Prog -> FilePath
prog_path h p =
        case is_ghc_prog p of
          True  -> ghc_bin h </> prog_name p
          False -> hp_bin  h </> prog_name p




prog_mp :: Map.Map String Prog
prog_mp = Map.fromList
    [ (,) "ghc"         GhcP
    , (,) "ghc-pkg"     Ghc_pkgP
    , (,) "cabal"       CabalP
    ]

data Prog
    = GhcP
    | Ghc_pkgP
    | CabalP
                                                                deriving (Show)
is_ghc_prog :: Prog -> Bool
is_ghc_prog p =
        case p of
           GhcP      -> True
           Ghc_pkgP  -> True
           CabalP    -> False

prog_name :: Prog -> FilePath
prog_name p =
        case p of
           GhcP      -> "ghc"
           Ghc_pkgP  -> "ghc-pkg"
           CabalP    -> "cabal"

hub_cts :: HubName -> IO String
hub_cts nm = 
     do hm <-home 
        readFile $ printf "%s/.hs/hubs/%s.conf" hm nm

hub_pdb :: FilePath -> Hub -> FilePath
hub_pdb hme h = printf "%s/.hs/hubs/%s-%s-%s/%s.conf.d"
                                            hme arch os (ghcH h) (hubH   h)

s_hub_pdb :: Hub -> FilePath
s_hub_pdb h = printf "/usr/hs/hubs/%s-%s-%s/%s.conf.d" 
                                                arch os (ghcH h) (s_hubH h)

ghc_bin :: Hub -> FilePath
ghc_bin h = printf "/usr/local/hs/ghc/%s/bin" (ghcH h)

hp_bin :: Hub -> FilePath
hp_bin h = printf "/usr/local/hs/hp/%s/bin" (hpH h)

home :: IO FilePath
home = getEnv "HOME"

trim :: String -> String
trim ln0 =
        case reverse ln of
          c:r_cs | isSpace c -> reverse $ dropWhile isSpace r_cs
          _                  -> ln
      where
        ln = dropWhile isSpace ln0
