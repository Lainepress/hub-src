module Hub.CommandLine
    ( commandLine
    , CommandLine(..)
    , Prog(..)
    , P(..)
    , ProgType(..)
    , p2prog
    , readHub
    ) where

import           Monad
import           Char
import           IO
import           System
import           System.Directory
import           System.FilePath
import qualified Data.Map       as Map
import           Text.Printf
import           Hub.Help
import           Hub.Oops
import           Hub.System
import           Hub.Hub
import           Hub.Parse
import           Hub.Commands


commandLine :: IO CommandLine
commandLine = 
     do as  <- getArgs
        mb  <- prog as
        mb' <- maybe (hub_dispatch as) (return.Just) mb
        case mb' of
          Nothing -> return $ HelpCL True usage
          Just cl -> return   cl


data CommandLine
    = ProgCL Hub Prog [String]
    | HelpCL Bool String   -- False => help, True => usage
    | VrsnCL
    | DfltCL
    | StDfCL Hub
    | RsDfCL
    | LsCL
    | NameCL Hub
    | InfoCL Hub
    | PathCL Hub
    | XmlCL  Hub
    | InitCL Hub HubName
    | CpCL   Hub HubName
    | MvCL   Hub HubName
    | RmCL   Hub
    | SwapCL Hub HubName
                                                                deriving (Show)

data Prog = PROG {
    enmPROG :: P,
    nmePROG :: String,
    typPROG :: ProgType
    }                                                           deriving (Show)

data ProgType = HcPT | CiPT | HpPT
                                                                deriving (Show)


prog, hub_dispatch :: [String] -> IO (Maybe CommandLine)

prog as =
     do pn <- getProgName
        let (_,p_s) = splitFileName pn
        case Map.lookup p_s prog_mp of
          Nothing  -> return Nothing
          Just prg -> 
             do hub <- current_hub
                return $ Just $ cabal_fixup hub prg as 

hub_dispatch as = case as of
    ["--help"     ] ->                                      return $ Just $ HelpCL False help
    ["help"       ] ->                                      return $ Just $ HelpCL False help
    ["--version"  ] ->                                      return $ Just VrsnCL
    ["version"    ] ->                                      return $ Just VrsnCL
    ["--usage"    ] ->                                      return $ Just $ HelpCL False usage
    ["usage"      ] ->                                      return $ Just $ HelpCL False usage
    ["default"    ] ->                                      return $ Just $ DfltCL
    ["default","-"] ->                                      return $ Just $ RsDfCL
    ["default",hn ] -> readHub          hn      >>= \hub -> return $ Just $ StDfCL hub
    ["ls"         ] ->                                      return $ Just $ LsCL
    ["name"       ] -> current_hub              >>= \hub -> return $ Just $ NameCL hub
    ["info"       ] -> current_hub              >>= \hub -> return $ Just $ InfoCL hub
    ["info",hn    ] -> readHub          hn      >>= \hub -> return $ Just $ InfoCL hub
    ["path"       ] -> current_hub              >>= \hub -> return $ Just $ PathCL hub
    ["path",hn    ] -> readHub          hn      >>= \hub -> return $ Just $ PathCL hub
    ["xml"        ] -> current_hub              >>= \hub -> return $ Just $ XmlCL  hub
    ["xml" ,hn    ] -> readHub          hn      >>= \hub -> return $ Just $ XmlCL  hub
    ["init"   ,hn'] -> hub_pair Nothing     hn' >>= \hub -> return $ Just $ InitCL hub hn'
    ["init",hn,hn'] -> hub_pair (Just   hn) hn' >>= \hub -> return $ Just $ InitCL hub hn'
    ["cp"     ,hn'] -> hub_pair Nothing     hn' >>= \hub -> return $ Just $ CpCL   hub hn'
    ["cp"  ,hn,hn'] -> hub_pair (Just   hn) hn' >>= \hub -> return $ Just $ CpCL   hub hn'
    ["mv"     ,hn'] -> hub_pair Nothing     hn' >>= \hub -> return $ Just $ MvCL   hub hn'
    ["mv"  ,hn,hn'] -> hub_pair (Just   hn) hn' >>= \hub -> return $ Just $ MvCL   hub hn'
    ["rm"     ,hn'] -> readHub              hn' >>= \hub -> return $ Just $ RmCL   hub
    ["swap",hn,hn'] -> hub_pair (Just   hn) hn' >>= \hub -> return $ Just $ MvCL   hub hn'
    _               ->                                      return   Nothing  

usage :: String
usage = unlines
    [ "hub [--]help"
    , "    [--]version"
    , "    [--]usage"
    , "    default [<hub>|-]"
    , "    ls"    
    , "    name"
    , "    info [<hub>]"        -- ****
    , "    path [<hub>]"
    , "    xml  [<hub>]"
    , "    init [<hub>] <hub>"
    , "    cp   [<hub>] <hub>"
    , "    mv   [<hub>] <hub>"
    , "    rm           <hub>"
    , "    swap  <hub>  <hub>"  -- ****
    ]

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

p2prog :: P -> Prog
p2prog p =
    case p of
      GhcP               -> PROG p "ghc"                  HcPT
      GhciP              -> PROG p "ghci"                 HcPT
      Ghc_pkgP           -> PROG p "ghc-pkg"              HcPT
      HaddockP           -> PROG p "haddock"              HcPT
      Hp2psP             -> PROG p "hp2ps"                HcPT
      HpcP               -> PROG p "hpc"                  HcPT
      Hsc2hsP            -> PROG p "hsc2hs"               HcPT
      RunghcP            -> PROG p "runghc"               HcPT
      RunhaskellP        -> PROG p "runhaskell"           HcPT
      CabalP             -> PROG p "cabal"                CiPT
      AlexP              -> PROG p "alex"                 HpPT
      Basic_testsP       -> PROG p "basic-tests"          HpPT
      Extended_testsP    -> PROG p "extended-tests"       HpPT
      HappyP             -> PROG p "happy"                HpPT
      Terminal_testsP    -> PROG p "terminal-tests"       HpPT

prog_mp :: Map.Map String Prog
prog_mp = Map.fromList [ (nmePROG pg,pg) | pg<-map p2prog [minBound..maxBound] ]


      
current_hub :: IO Hub
current_hub = which_hub >>= readHub

hub_pair :: Maybe HubName -> HubName -> IO Hub
hub_pair Nothing   hn' = which_hub >>= \hn -> hub_pair' hn hn'
hub_pair (Just hn) hn' =                      hub_pair' hn hn' 

hub_pair' :: HubName -> HubName -> IO Hub
hub_pair' hn hn' = 
     do checkHubName AnyHT hn
        checkHubName UsrHT hn'
        when (hn==hn') $ oops HubO $
                                printf "%s: same source and destination" hn
        userHubAvailable hn'
        readHub hn

which_hub :: IO HubName
which_hub =
     do ei <- try $ getEnv "HUB"
        hn <- case ei of
                Left  _ -> trim `fmap` dir_which_hub True
                Right s -> trim `fmap` env_which_hub s
        checkHubName AnyHT hn
        return hn

env_which_hub :: String -> IO HubName
env_which_hub str =
        case str of
          "--default" -> dir_which_hub True
          "--dir    " -> dir_which_hub False
          "--user"    -> usr_which_hub
          "--global"  -> glb_which_hub
          _           -> return str

dir_which_hub :: Bool -> IO HubName
dir_which_hub def_usr = 
     do ds <- (reverse.splitDirectories) `fmap` getCurrentDirectory
        w_h ds
      where
        w_h [] | def_usr   = usr_which_hub
               | otherwise = oops HubO "no hub specified"
        w_h (d:ds)         = catch (here (d:ds)) (\_ -> w_h ds)
        
        here r_ds  = readAFile (joinPath $ reverse $ ".hub":r_ds)

usr_which_hub :: IO HubName
usr_which_hub =
     do yup <- isUserHub homeHub
        case yup of
          True  -> return ()
          False -> default_global_hub >>= \hub -> _init hub homeHub
        return homeHub

glb_which_hub :: IO HubName
glb_which_hub = readFile defaultHubPath



cabal_fixup :: Hub -> Prog -> [String] -> CommandLine
cabal_fixup hub prg as = ProgCL hub prg as' 
      where
        as'  = case enmPROG prg of
                 CabalP -> ci_fixup (usr_dbHUB hub) as
                 _      -> as 

ci_fixup :: Maybe FilePath -> [String] -> [String]
ci_fixup mb as =
        case as of
          "install"  :as' -> "install"   : x_as ++ as'
          "upgrade"  :as' -> "upgrade"   : x_as ++ as'
          "configure":as' -> "configure" : x_as ++ as'
          _               -> as
      where
        x_as = maybe [] (\db->["--package-db="++db]) mb

default_global_hub :: IO Hub
default_global_hub =
     do hn <- defaultGlobalHubName
        hf <- case isGlobal hn of
                True  -> return $ globalHubPath hn
                False -> userHubPath hn
        parse hn hf



trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
