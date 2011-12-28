module Hub.Hub
    ( Hub(..)
    , HubName
    , HubKind(..)
    , prettyHubKind
    , checkHubName
    , isHubName
    , hubUserPackageDBPath
    , ghcPkg
    , cabal
    ) where

import Data.Char
import Text.Printf
import System.Exit
import System.FilePath
import Hub.System
import Hub.Oops


data Hub = HUB {
    name__HUB :: HubName,
    kind__HUB :: HubKind,
    path__HUB :: FilePath,
    hc_binHUB :: FilePath,
    tl_binHUB :: FilePath,
    glb_dbHUB :: FilePath,
    usr_dyHUB :: Maybe FilePath,
    usr_dbHUB :: Maybe FilePath
    }                                                           deriving (Show)


type HubName = String

data HubKind
    = GlbHK
    | UsrHK
                                            deriving (Eq,Ord,Bounded,Enum,Show)


prettyHubKind :: HubKind -> String
prettyHubKind GlbHK = "global" 
prettyHubKind UsrHK = "user"

checkHubName :: [HubKind] -> HubName -> IO HubKind
checkHubName hks hn =
        case isHubName hn of
          Nothing                 -> oops SysO $ printf "%s is not a valid hub name" hn
          Just hk | hk `elem` hks -> return hk
                  | otherwise     -> oops SysO $ printf "%s is a %s hub" hn $ prettyHubKind hk

isHubName :: HubName -> Maybe HubKind
isHubName hn =
        case hn of
          c:cs | all hubname_c cs -> fst_hubname_c c 
          _                       -> Nothing

hubUserPackageDBPath :: Hub -> IO FilePath
hubUserPackageDBPath hub =
        case usr_dbHUB hub of
          Nothing -> oops PrgO $ printf "%s: not a user hub" $ name__HUB hub
          Just db -> return db


ghcPkg, cabal :: ExecCtrl -> Hub -> [String] -> IO ()
ghcPkg ec hub as = exec_prog ec hub "ghc-pkg" as
cabal  ec hub as = exec_prog ec hub "cabal"   as


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


--
-- Executing Programs
--

exec_prog :: ExecCtrl -> Hub -> String -> [String] -> IO ()
exec_prog ec hub pr as =
     do ex <- execProg ec pr as
        case ex of
          ExitSuccess   -> return ()
          ExitFailure n -> oops HubO $
                                printf "%s failure (return code=%d)" pp n  
      where
        pp = hc_binHUB hub </> pr
