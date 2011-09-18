module Main(main) where 

import           System.Environment
import           System.Directory
import           System.FilePath
import           System.Info
--import           System.Posix.Env       (setEnv)
--import           System.Posix.Process
import           Text.Printf
import           Data.Char
import qualified Data.Map               as Map
import           Hub.CL
import           Hub.Parse


main :: IO ()
main = 
     do p <- get_prog
        -- putStrLn $ printf "prog : %s" $ show p 
        n <- which_hub
        -- putStrLn $ printf "hub  : %s" $ show n 
        h <- get_hub n
        -- putStrLn $ printf " =>  : %s" $ show h
        set_pkg_path h
        a <- getArgs
        -- putStrLn $ printf "exec %s %s" (prog_path h p) (unwords a)
        -- executeFile "/bin/bash" False [] Nothing
        -- executeFile (prog_path h p) False a Nothing
        return ()

get_prog :: IO Prog
get_prog =
     do pn <- getProgName
        let (_,p_s) = splitFileName pn
        case Map.lookup p_s prog_mp of
          Nothing -> error "Oops, program not recognised"
          Just p  -> return p

-- type HubName = String

type GHCInst = String
type HPInst  = String

data Hub_ = HUB_ {
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

get_hub :: HubName -> IO Hub_
get_hub hub =
     do cts <- hub_cts hub
        case map trim $ filter n_cmt $ lines cts of
          ghc:hp:s_hub:_ -> return $ HUB_ ghc hp s_hub hub
          _              -> error  $ "Hub " ++ hub ++ ": bad format"
      where
        n_cmt ('#':_) = False
        n_cmt ln      = not $ all isSpace ln

set_pkg_path :: Hub_ -> IO ()
set_pkg_path h =
     do hme <- home
     {-
        let spd = s_hub_pdb     h
            upd = hub_pdb   hme h
            pth = printf "%s:%s" upd spd :: String
      --setEnv "GHC_PACKAGE_PATH" pth True
      -}
        return ()
        
prog_path :: Hub_ -> Prog -> FilePath
prog_path h prog =
        case typPROG prog of
          HcPT -> ghc_bin h </> nmePROG prog
          _    -> hp_bin  h </> nmePROG prog

{-
data Prog = PG {
     progPG :: P,
     namePG :: String,
     ghcPG  :: Bool
     }                                                          deriving (Show)
-}

prog_mp :: Map.Map String Prog
prog_mp = Map.fromList [ (nmePROG pg,pg) | pg<-map p2prog [minBound..maxBound] ]

{-
data P
    = GhcP
    | GhciP
    | Ghc_pkgP
    | HaddockP
    | Hp2psP
    | HpcP
    | Hsc2hsP
    | RunghcP
    | RunhaskellP
    | AlexP
    | Basic_testsP
    | CabalP
    | Extended_testsP
    | HappyP
    | Terminal_testsP
                                            deriving (Eq,Ord,Bounded,Enum,Show)

p2pg :: P -> Prog
p2pg p =
    case p of
      GhcP               -> PG p "ghc"                  True
      GhciP              -> PG p "ghci"                 True
      Ghc_pkgP           -> PG p "ghc-pkg"              True
      HaddockP           -> PG p "haddock"              True
      Hp2psP             -> PG p "hp2ps"                True
      HpcP               -> PG p "hpc"                  True
      Hsc2hsP            -> PG p "hsc2hs"               True
      RunghcP            -> PG p "runghc"               True
      RunhaskellP        -> PG p "runhaskell"           True
      AlexP              -> PG p "alex"                 False
      Basic_testsP       -> PG p "basic-tests"          False
      CabalP             -> PG p "cabal"                False
      Extended_testsP    -> PG p "extended-tests"       False
      HappyP             -> PG p "happy"                False
      Terminal_testsP    -> PG p "terminal-tests"       False
-}

hub_cts :: HubName -> IO String
hub_cts nm = 
     do hme <-home 
        readFile $ printf "%s/.hs/hubs/%s.conf" hme nm

hub_pdb :: FilePath -> Hub_ -> FilePath
hub_pdb hme h = printf "%s/.hs/hubs/%s-%s-%s/%s.conf.d"
                                            hme arch os (ghcH h) (hubH   h)

s_hub_pdb :: Hub_ -> FilePath
s_hub_pdb h = printf "/usr/hs/hubs/%s/%s.conf.d"        (ghcH h) (s_hubH h)

ghc_bin :: Hub_ -> FilePath
ghc_bin h = printf "/usr/hs/ghc/%s/bin" (ghcH h)

hp_bin :: Hub_ -> FilePath
hp_bin h = printf "/usr/hs/hp/%s/bin" (hpH h)

home :: IO FilePath
home = getEnv "HOME"

trim :: String -> String
trim ln0 =
        case reverse ln of
          c:r_cs | isSpace c -> reverse $ dropWhile isSpace r_cs
          _                  -> ln
      where
        ln = dropWhile isSpace ln0


--
-- tools
--


{-
link_script :: IO ()
link_script = mapM_ link_out $ Map.keys prog_mp
      where
        link_out pn = putStrLn $ printf "ln -s hub %s" pn
-}


{-
--
-- XML Experiments
--


test :: IO ()
test =
     do cts <- readFile "test.hub"
        case parseXMLDoc cts of
          Nothing -> putStrLn "*** parse failure ***"
          Just el -> putStrLn $ showTopElement el
-}


