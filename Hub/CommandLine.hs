module Hub.CommandLine
    ( commandLine
    , CommandLine(..)
    , Prog(..)
    , P(..)
    , ProgType(..)
    , p2prog
    ) where

import           Monad
import           Char
import           IO
import           System
import           System.Directory
import           System.FilePath
import qualified Data.Map       as Map
import           Text.Printf
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
    | NameCL Hub  
    | PathCL Hub
    | XmlCL  Hub
    | InitCL Hub HubName
    | CpCL   Hub HubName
    | MvCL   Hub HubName
    | RmCL   Hub
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
                return $ Just $ ProgCL hub prg as

hub_dispatch as = case as of
    ["--help"     ] ->                                      return $ Just $ HelpCL False usage
    ["--version"  ] ->                                      return $ Just VrsnCL
    ["name"       ] -> current_hub              >>= \hub -> return $ Just $ NameCL hub
    ["path"       ] -> current_hub              >>= \hub -> return $ Just $ PathCL hub
    ["path",hn    ] -> read_hub         hn      >>= \hub -> return $ Just $ PathCL hub
    ["xml"        ] -> current_hub              >>= \hub -> return $ Just $ XmlCL  hub
    ["xml" ,hn    ] -> read_hub         hn      >>= \hub -> return $ Just $ XmlCL  hub
    ["init"   ,hn'] -> hub_pair Nothing     hn' >>= \hub -> return $ Just $ InitCL hub hn'
    ["init",hn,hn'] -> hub_pair (Just   hn) hn' >>= \hub -> return $ Just $ InitCL hub hn'
    ["cp"     ,hn'] -> hub_pair Nothing     hn' >>= \hub -> return $ Just $ CpCL   hub hn'
    ["cp"  ,hn,hn'] -> hub_pair (Just   hn) hn' >>= \hub -> return $ Just $ CpCL   hub hn'
    ["mv"     ,hn'] -> hub_pair Nothing     hn' >>= \hub -> return $ Just $ MvCL   hub hn'
    ["mv"  ,hn,hn'] -> hub_pair (Just   hn) hn' >>= \hub -> return $ Just $ MvCL   hub hn'
    ["rm"  ,hn    ] -> read_hub         hn      >>= \hub -> return $ Just $ RmCL   hub
    _               ->                                      return   Nothing  

usage :: String
usage = unlines
    [ "hub --help"
    , "    --version"
    , "    name"
    , "    path [<hub>]"
    , "    xml  [<hub>]"
    , "    init [<hub>] <hub>"
    , "    cp   [<hub>] <hub>"
    , "    mv   [<hub>] <hub>"
    , "    rm           <hub>"
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
current_hub = which_hub >>= read_hub

read_hub :: HubName -> IO Hub
read_hub hn = 
     do hf <-  case isGlobal hn  of
                  True  -> globalHubPath hn
                  False -> userHubPath   hn
        checkHubName hn
        parse hn hf 

hub_pair :: Maybe HubName -> HubName -> IO Hub
hub_pair Nothing   hn' = which_hub >>= \hn -> hub_pair' hn hn'
hub_pair (Just hn) hn' =                      hub_pair' hn hn' 

hub_pair' :: HubName -> HubName -> IO Hub
hub_pair' hn hn' = 
     do checkHubName     hn
        checkHubName     hn'
        when (hn==hn') $ ioError $ userError $
                                printf "%s: same source and destination"
        userHubAvailable hn'
        read_hub hn

which_hub :: IO HubName
which_hub =
     do ei <- try $ getEnv "HUB"
        hn <- case ei of
                Left  _ -> trim `fmap` dir_which_hub True
                Right s -> trim `fmap` env_which_hub s
        checkHubName hn
        return       hn

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
               | otherwise = ioError $ userError "Hub"  
        w_h (d:ds)         = catch (here (d:ds)) (\_ -> w_h ds)
        
        here r_ds  = readAFile (joinPath $ reverse $ ".hub":r_ds)

usr_which_hub :: IO HubName
usr_which_hub =
     do yup <- isUserHub homeHub
        case yup of
          True  -> return ()
          False -> defaultGlobalHub >>= \hub -> _init hub homeHub
        return homeHub

glb_which_hub :: IO HubName
glb_which_hub = readFile defaultHubPath



trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
