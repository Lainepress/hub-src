module Main(main) where 

import           System.Environment
import           System.Directory
import           System.FilePath
import           System.Posix.Env       (setEnv)
import           System.Posix.Process
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
        pth <- prog_path h p
        executeFile pth False a Nothing
        return ()

get_prog :: IO Prog
get_prog =
     do pn <- getProgName
        let (_,p_s) = splitFileName pn
        case Map.lookup p_s prog_mp of
          Nothing -> ioError $ userError $ error $ printf "hub: GHC/HP program %s not recognised" p_s
          Just p  -> return p

{-
type GHCInst = String
type HPInst  = String

data Hub = HUB_ {
    ghcH   :: GHCInst,
    hpH    :: HPInst,
    s_hubH :: HubName,
    hubH   :: HubName
    }                                                           deriving (Show)
-}

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
get_hub hn =
     do hf <- case hn of
          h:_ | isDigit h -> return $ printf "/usr/hs/hub/%s.xml" hn
          _               -> getEnv "HOME" >>= \hme ->
                                return $ printf "%s/.hs/hubs/%s.conf" hme hn
        parse hn hf

set_pkg_path :: Hub -> IO ()
set_pkg_path hub = setEnv "GHC_PACKAGE_PATH" pth True
      where
        pth        = maybe glb mk_pth $ usr_dbHUB hub
        
        mk_pth usr = printf "%s:%s" usr glb

        glb        = glb_dbHUB hub
        
prog_path :: Hub -> Prog -> IO FilePath
prog_path hub prog =
        case typPROG prog of
          HcPT -> return $ hc_binHUB hub </> nmePROG prog
          CiPT -> return $ hc_binHUB hub </> nmePROG prog
          HpPT -> chk_hp $ \hp_bin -> return $ hp_bin </>  nmePROG prog
      where
        chk_hp f = maybe nhp_err f $ hp_binHUB hub
        
        nhp_err  = ioError $ userError $
                        printf "Hub %s does not hava a Haskell Platform"
                                                            (handleHUB hub) 

prog_mp :: Map.Map String Prog
prog_mp = Map.fromList [ (nmePROG pg,pg) | pg<-map p2prog [minBound..maxBound] ]

{-
hub_cts :: HubName -> IO String
hub_cts nm = 
     do hme <-home 
        readFile $ printf "%s/.hs/hubs/%s.conf" hme nm

hub_pdb :: FilePath -> Hub -> FilePath
hub_pdb hme h = printf "%s/.hs/hubs/%s-%s-%s/%s.conf.d"
                                            hme arch os (ghcH h) (hubH   h)

s_hub_pdb :: Hub -> FilePath
s_hub_pdb h = printf "/usr/hs/hubs/%s/%s.conf.d"        (ghcH h) (s_hubH h)

ghc_bin :: Hub -> FilePath
ghc_bin h = printf "/usr/hs/ghc/%s/bin" (ghcH h)

hp_bin :: Hub -> FilePath
hp_bin h = printf "/usr/hs/hp/%s/bin" (hpH h)

home :: IO FilePath
home = getEnv "HOME"
-}


--
-- tools
--

{-
trim :: String -> String
trim ln0 =
        case reverse ln of
          c:r_cs | isSpace c -> reverse $ dropWhile isSpace r_cs
          _                  -> ln
      where
        ln = dropWhile isSpace ln0
-}

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


