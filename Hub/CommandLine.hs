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
        mb' <- maybe (hub as) (return.Just) mb
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


prog, hub :: [String] -> IO (Maybe CommandLine)

prog _  =
     do pn <- getProgName
        let (_,p_s) = splitFileName pn
        case Map.lookup p_s prog_mp of
          Nothing  -> return Nothing
          Just prg ->
             do hub <- current_hub
                return $ Just $ ProgCL hub prg 

hub as = case as of
  ["--help"     ] ->                                          return $ Just $ HelpCL False usage
  ["--version"  ] ->                                          return $ Just VrsnCL
  ["name"       ] -> current_hub            >>= \hub       -> return $ Just $ NameCL hub
  ["path"       ] -> current_hub            >>= \hub       -> return $ Just $ PathCL hub
  ["path",hn    ] -> read_hub hn            >>= \hub       -> return $ Just $ PathCL hub
  ["xml"        ] -> current_hub            >>= \hub       -> return $ Just $ XmlCL hub
  ["xml" ,hn    ] -> read_hub hn            >>= \hub       -> return $ Just $ XmlCL hub
  ["init",hn    ] -> read_hub hn            >>= \hub       -> return $ Just $ InitCL hub
  ["cp"  ,hn    ] -> hub_pair hn Nothing    >>= \(hub,hn_) -> return $ Just $ CpCL hub hn_
  ["cp"  ,hn,hn'] -> hub_pair hn (Just hn') >>= \(hub,hn_) -> return $ Just $ CpCL hub hn_
  ["mv"  ,hn    ] -> hub_pair hn Nothing    >>= \(hub,hn_) -> return $ Just $ MvCL hub hn_
  ["mv"  ,hn,hn'] -> hub_pair hn (Just hn') >>= \(hub,hn_) -> return $ Just $ MvCL hub hn_
  ["rm"  ,hn    ] -> read_hub hn            >>= \hub       -> return $ Just $ RmCL hub
  _               ->                                          return   Nothing  

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
current_hub = undefined

read_hub :: HubName -> IO Hub
read_hub hn = undefined hn

hub_pair :: HubName -> Maybe HubName -> IO (Hub,HubName)
hub_pair hn mb = undefined hn mb
