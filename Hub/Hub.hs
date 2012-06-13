--
-- >>> Hub.hub <<<
--
-- This module provdes the central Hub abstraction. As the primary purpose
-- of a hub is to execute programs in a controlled environment it includes
-- the 'exec' utilities for setting up the PATH, GHC_PACKAGE_PATH and munging
-- cabal's command-line arguments, etc.
--
-- (c) 2011-2012 Chris Dornan


module Hub.Hub
    ( Hub(..)
    , UsrHub(..)
    , HubName
    , HubKind(..)
    , HubSource(..)
    , prettyHubKind
    , checkHubName
    , isHubName
    , hubUserPackageDBPath
    , usr_ghHUB
    , usr_dbHUB
    , lockedHUB
    , Mode(..)
    , execP
    , execProg
    ) where

import           Data.Char
import           Text.Printf
import           System.Exit
import           System.FilePath
import           System.Environment
import           System.Directory
import           Hub.FilePaths
import           Hub.System
import           Hub.Directory.Allocate
import           Hub.Oops
import           Hub.Prog


data Hub = HUB
    { sourceHUB :: HubSource
    , name__HUB :: HubName
    , kind__HUB :: HubKind
    , path__HUB :: FilePath
    , commntHUB :: String
    , hc_binHUB :: FilePath
    , tl_binHUB :: FilePath
    , ci_vrnHUB :: Maybe String
    , glb_dbHUB :: FilePath
    , usr___HUB :: Maybe UsrHub
    }                                                           deriving (Show)

data UsrHub = UHB
    { dir___UHB :: FilePath
    , glb_hnUHB :: HubName
    , usr_dbUHB :: FilePath
    , lockedUHB :: Bool
    }                                                           deriving (Show)


type HubName = String

data HubKind
    = GlbHK         -- global hub
    | UsrHK         -- user hub
                                            deriving (Eq,Ord,Bounded,Enum,Show)

data HubSource
    = ClHS          -- hub sepcified on command line
    | EvHS          -- hub specified by environment variable
    | DrHS          -- hub specified by a directory marker
    | DsHS          -- hub specified by system default
                                                                deriving (Show)

prettyHubKind :: HubKind -> String
prettyHubKind GlbHK = "global"
prettyHubKind UsrHK = "user"

checkHubName :: [HubKind] -> HubName -> IO HubKind
checkHubName hks hn =
        case isHubName hn of
          Nothing                 -> oops PrgO $ printf "%s is not a valid hub name" hn
          Just hk | hk `elem` hks -> return hk
                  | otherwise     -> oops PrgO $ printf "%s is a %s hub" hn $ prettyHubKind hk

isHubName :: HubName -> Maybe HubKind
isHubName hn =
        case hn of
          c:cs | all hubname_c cs -> fst_hubname_c c
          _                       -> Nothing

hubUserPackageDBPath :: Hub -> IO FilePath
hubUserPackageDBPath hub =
        case usr___HUB hub of
          Nothing  -> oops PrgO $ printf "%s: not a user hub" $ name__HUB hub
          Just uhb -> return $ usr_dbUHB uhb

usr_ghHUB :: Hub -> Maybe FilePath
usr_ghHUB = fmap glb_hnUHB . usr___HUB

usr_dbHUB :: Hub -> Maybe FilePath
usr_dbHUB = fmap usr_dbUHB . usr___HUB

lockedHUB :: Hub -> Bool
lockedHUB hub        = maybe False lockedUHB $ usr___HUB hub

data Mode = FullMDE | UserMDE

execP :: Oops -> ExecEnv -> Mode -> Hub -> P -> [String] -> IO ()
execP o ee0 mde hub p args0 = execProg o ee0 mde hub (p2prog p) args0

execProg :: Oops -> ExecEnv -> Mode -> Hub -> Prog -> [String] -> IO ()
execProg o ee0 mde hub prog args0 =
     do case (mde,usr___HUB hub) of
          (UserMDE,Nothing) -> oops o "user hub expected"
          _                 -> return ()
        (exe,args,tdy) <- mk_prog hub prog args0
        pth0 <- getEnv "PATH"
        ebds <- hub_env mde hub pth0
        let ee = ee0 { extendEnvtEE = ebds ++ extendEnvtEE ee0 }
        ec   <- exec ee exe args
        case tdy of
          Nothing -> return ()
          Just hd -> tidyDir hd
        case ec of
          ExitSuccess   -> return ()
          ExitFailure n -> oops o $ printf "%s failure (return code=%d)" exe n


--
-- Executing Programs
--

mk_prog :: Hub -> Prog -> [String] -> IO (FilePath,[String],Maybe FilePath)
mk_prog hub prog as0 =
     do (as,tdy) <- case (hk/=GlbHK,enmPROG prog,as0) of
                      (True,CabalP,"configure":as') -> ci "configure" as'
                      (True,CabalP,"install"  :as') -> ci "install"   as'
                      (True,CabalP,"upgrade"  :as') -> ci "upgrade"   as'
                      _                             -> return (as0,Nothing)
        return (exe,as,tdy)
      where
        exe =   case typPROG prog of
                  HcPT -> hc_binHUB hub </> nmePROG       prog
                  TlPT -> tl_binHUB hub </> prog_name hub prog

        hk  =   kind__HUB hub

        ci cmd as' =
             do hd <- allocate
                db <- hubUserPackageDBPath hub
                let _ld = "--libdir="     ++ hd
                    _pd = "--package-db=" ++ db
                return ( cmd : _ld : _pd : as', Just hd )

prog_name :: Hub -> Prog -> FilePath
prog_name hub prog =
        case (enmPROG prog,ci_vrnHUB hub) of
          (CabalP,Just ci_vrn) -> nmePROG prog ++ "-" ++ ci_vrn
          _                    -> nmePROG prog

hub_env :: Mode -> Hub -> String -> IO [(String,String)]
hub_env mde hub pth0 = 
     do mb_udb <- case mb_usr of
                    Nothing  -> prep_hp_user_pdb hub glb
                    Just uhb -> return $ Just $ usr_dbUHB uhb
        let mk_gpt udb = case mde of
                           UserMDE -> udb
                           FullMDE -> printf "%s:%s" udb glb
            mb_gpt     = fmap mk_gpt mb_udb
        return $ concat
            [ [ (,) "HUB"               hnm                        ]
            , [ (,) "PATH"              pth | is_usr               ]
            , [ (,) "GHC_PACKAGE_PATH"  gpt | Just gpt <- [mb_gpt] ]
            ]
      where
        is_usr    = hk /= GlbHK

        pth       = printf "%s:%s:%s" hubGccBin hubBinutilsBin pth0

        hnm       = name__HUB hub
        hk        = kind__HUB hub
        mb_usr    = usr___HUB hub
        glb       = glb_dbHUB hub

prep_hp_user_pdb :: Hub -> FilePath -> IO (Maybe FilePath)
prep_hp_user_pdb hub glb =
     do mb <- hpGlbPdb2dfUsrPdb glb
        case mb of
          Nothing  -> return Nothing
          Just udb ->
             do ok <- doesDirectoryExist udb
                case ok of
                  True  -> return ()
                  False -> const () `fmap` exec ee exe ["init",udb]
                return mb
      where
        ee  = EE InheritRS InheritRS []
        exe = hc_binHUB hub </> "ghc-pkg"


--
-- Validating Hub Names
--

fst_hubname_c :: Char -> Maybe HubKind
fst_hubname_c c | glb_first_hub_name_c c = Just GlbHK
                | usr_first_hub_name_c c = Just UsrHK
                | otherwise              = Nothing

hubname_c :: Char -> Bool
hubname_c c = c `elem` "_-." || isAlpha c || isDigit c

glb_first_hub_name_c, usr_first_hub_name_c :: Char -> Bool
glb_first_hub_name_c c = isDigit c
usr_first_hub_name_c c = c `elem` "_." || isAlpha c

