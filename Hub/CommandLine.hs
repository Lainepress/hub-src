module Hub.CommandLine
    ( commandLine
    , CommandLine(..)
    , Prog(..)
    , P(..)
    , ProgType(..)
    , p2prog
    ) where


import           System
import           System.FilePath
import qualified Data.Map       as Map
import           Hub.Parse


commandLine :: IO CommandLine
commandLine = 
     do as  <- getArgs
        mb  <- prog as
        mb' <- maybe (hub_dispatch as) (return.Just) mb
        case mb' of
          Nothing -> return $ HelpCL True usage
          Just cl -> return   cl


data CommandLine
    = ProgCL Hub Prog
    | HelpCL Bool String   -- False => help, True => usage
    | VrsnCL
    | NameCL Hub  
    | PathCL Hub
    | XmlCL  Hub
    | InitCL Hub
    | CpCL   Hub HubName
    | MvCL   Hub HubName
    | RmCL   Hub

data Prog = PROG {
    enmPROG :: P,
    nmePROG :: String,
    typPROG :: ProgType
    }                                                           deriving (Show)

data ProgType = HcPT | CiPT | HpPT
                                                                deriving (Show)


prog, hub_dispatch :: [String] -> IO (Maybe CommandLine)

prog _  =
     do pn <- getProgName
        let (_,p_s) = splitFileName pn
        case Map.lookup p_s prog_mp of
          Nothing  -> return Nothing
          Just prg ->
             do hub <- current_hub
                return $ Just $ ProgCL hub prg 

hub_dispatch as = case as of
  ["--help"     ] ->                                      return $ Just $ HelpCL False usage
  ["--version"  ] ->                                      return $ Just VrsnCL
  ["name"       ] -> current_hub              >>= \hub -> return $ Just $ NameCL hub
  ["path"       ] -> current_hub              >>= \hub -> return $ Just $ PathCL hub
  ["path",hn    ] -> read_hub         hn      >>= \hub -> return $ Just $ PathCL hub
  ["xml"        ] -> current_hub              >>= \hub -> return $ Just $ XmlCL hub
  ["xml" ,hn    ] -> read_hub         hn      >>= \hub -> return $ Just $ XmlCL hub
  ["init"   ,hn'] -> read_hub             hn' >>= \hub -> return $ Just $ InitCL hub
  ["cp"     ,hn'] -> hub_pair Nothing     hn' >>= \hub -> return $ Just $ CpCL hub hn'
  ["cp"  ,hn,hn'] -> hub_pair (Just   hn) hn' >>= \hub -> return $ Just $ CpCL hub hn'
  ["mv"     ,hn'] -> hub_pair Nothing     hn' >>= \hub -> return $ Just $ MvCL hub hn'
  ["mv"  ,hn,hn'] -> hub_pair (Just   hn) hn' >>= \hub -> return $ Just $ MvCL hub hn'
  ["rm"  ,hn    ] -> read_hub         hn      >>= \hub -> return $ Just $ RmCL hub
  _               ->                                      return   Nothing  

usage :: String
usage = unlines
    [ "hub --help"
    , "    --version"
    , "    name"
    , "    path [<hub>]"
    , "    xml  [<hub>]"
    , "    init         <hub>"
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
read_hub hn = check_hub_name hn >> load_hub hn hf 
      where
        hf = if is_global hn then global_hub hn else user_hub hn

hub_pair :: Maybe HubName -> HubName -> IO Hub
hub_pair Nothing   hn' = which_hub >>= \hn -> hub_pair' hn hn'
hub_pair (Just hn) hn' =                      hub_pair' hn hn' 

hub_pair' :: HubName -> HubName -> IO Hub
hub_pair' hn hn' = check_user_hub_name_available hn' >> read_hub hn

which_hub :: IO HubName
which_hub = undefined

global_hub :: HubName -> FilePath
global_hub = undefined

user_hub :: HubName -> FilePath
user_hub = undefined

load_hub :: HubName -> FilePath -> IO Hub
load_hub = undefined

check_hub_name :: HubName -> IO ()
check_hub_name = undefined

check_user_hub_name_available :: HubName -> IO ()
check_user_hub_name_available = undefined

is_global :: HubName -> Bool
is_global = undefined
