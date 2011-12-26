module Hub.CommandLine
    ( commandLine
    , CommandLine(..)
    , Prog(..)
    , P(..)
    , ProgType(..)
    , p2prog
    ) where

import           Text.Printf
import           Control.Monad
import           System.Environment
import           System.FilePath
import qualified Data.Map       as Map
import           Hub.Help
import           Hub.Oops
import           Hub.Hub
import           Hub.Directory
import           Hub.Discover


commandLine :: IO CommandLine
commandLine = 
     do as  <- getArgs
        mb  <- prog as
        mb' <- maybe (hub_dispatch as) (return.Just) mb
        case mb' of
          Nothing -> return $ HelpCL True usage
          Just cl -> return   cl


data CommandLine
    = ProgCL Hub (Prog,[String])
    | HelpCL Bool String   -- False => help, True => usage
    | VrsnCL
    | DfltCL
    | StDfCL  Hub
    | RsDfCL
    | LsCL
    | GetCL
    | SetCL   Hub
    | UnsetCL
    | NameCL  Hub
    | InfoCL  Hub
    | PathCL  Hub
    | XmlCL   Hub
    | InitCL  Hub HubName
    | CpCL    Hub HubName
    | MvCL    Hub HubName
    | RmCL    Hub
    | SwapCL  Hub HubName
                                                                deriving (Show)

data Prog = PROG {
    enmPROG :: P,
    nmePROG :: String,
    typPROG :: ProgType
    }                                                           deriving (Show)

data ProgType = HcPT | CiPT (Maybe FilePath) | TlPT
                                                                deriving (Show)


prog, hub_dispatch :: [String] -> IO (Maybe CommandLine)

prog as =
     do pn <- getProgName
        let (_,p_s) = splitFileName pn
        case Map.lookup p_s prog_mp of
          Nothing  -> return Nothing
          Just prg -> 
             do hub <- discover Nothing
                Just `fmap` cabal_fixup hub prg as 

hub_dispatch as = case as of
    ["--help"     ] ->                                       return $ Just $ HelpCL False helpText
    ["help"       ] ->                                       return $ Just $ HelpCL False helpText
    ["--help",cd  ] -> help cd                   >>= \hlp -> return $ Just $ HelpCL False hlp
    ["help"  ,cd  ] -> help cd                   >>= \hlp -> return $ Just $ HelpCL False hlp
    ["--version"  ] ->                                       return $ Just   VrsnCL
    ["version"    ] ->                                       return $ Just   VrsnCL
    ["--usage"    ] ->                                       return $ Just $ HelpCL False usage
    ["usage"      ] ->                                       return $ Just $ HelpCL False usage
    ["default"    ] ->                                       return $ Just $ DfltCL
    ["default","-"] ->                                       return $ Just $ RsDfCL
    ["default",hn ] -> discover (Just   hn)      >>= \hub -> return $ Just $ StDfCL hub
    ["ls"         ] -> initDirectory             >>= \_   -> return $ Just $ LsCL
    ["set"        ] ->                                       return $ Just $ GetCL
    ["set","-"    ] ->                                       return $ Just $ UnsetCL
    ["set",hn     ] -> discover (Just   hn)      >>= \hub -> return $ Just $ SetCL  hub
    ["name"       ] -> discover Nothing          >>= \hub -> return $ Just $ NameCL hub
    ["info"       ] -> discover Nothing          >>= \hub -> return $ Just $ InfoCL hub
    ["info",hn    ] -> discover (Just   hn)      >>= \hub -> return $ Just $ InfoCL hub
    ["path"       ] -> discover Nothing          >>= \hub -> return $ Just $ PathCL hub
    ["path",hn    ] -> discover (Just   hn)      >>= \hub -> return $ Just $ PathCL hub
    ["xml"        ] -> discover Nothing          >>= \hub -> return $ Just $ XmlCL  hub
    ["xml" ,hn    ] -> discover (Just   hn)      >>= \hub -> return $ Just $ XmlCL  hub
    ["init"   ,hn'] -> hub_pair Nothing     hn'  >>= \hub -> return $ Just $ InitCL hub hn'
    ["init",hn,hn'] -> hub_pair (Just   hn) hn'  >>= \hub -> return $ Just $ InitCL hub hn'
    ["cp"     ,hn'] -> hub_pair Nothing     hn'  >>= \hub -> return $ Just $ CpCL   hub hn'
    ["cp"  ,hn,hn'] -> hub_pair (Just   hn) hn'  >>= \hub -> return $ Just $ CpCL   hub hn'
    ["mv"     ,hn'] -> hub_pair Nothing     hn'  >>= \hub -> return $ Just $ MvCL   hub hn'
    ["mv"  ,hn,hn'] -> hub_pair (Just   hn) hn'  >>= \hub -> return $ Just $ MvCL   hub hn'
    ["rm"     ,hn'] -> discover       (Just hn') >>= \hub -> return $ Just $ RmCL   hub
    ["swap",   hn'] -> hub_swap Nothing     hn'  >>= \hub -> return $ Just $ SwapCL hub hn'
    ["swap",hn,hn'] -> hub_swap (Just   hn) hn'  >>= \hub -> return $ Just $ SwapCL hub hn'
    _               ->                                       return   Nothing  

data P
    = GhcP
    | GhciP
    | Ghc_pkgP
    | Hp2psP
    | HpcP
    | Hsc2hsP
    | RunghcP
    | RunhaskellP
    | CabalP
    | AlexP
    | HappyP
    | HaddockP
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
      CabalP             -> PROG p "cabal"               (CiPT Nothing)
      AlexP              -> PROG p "alex"                 TlPT
      HappyP             -> PROG p "happy"                TlPT

prog_mp :: Map.Map String Prog
prog_mp = Map.fromList [ (nmePROG pg,pg) | pg<-map p2prog [minBound..maxBound] ]


hub_pair, hub_swap :: Maybe HubName -> HubName -> IO Hub
hub_pair = hub_pair' False
hub_swap = hub_pair' True

hub_pair' :: Bool -> Maybe HubName -> HubName -> IO Hub
hub_pair' sw mb_hn hn' = 
     do hub <- discover mb_hn
        let hn = name__HUB hub
        _ <- checkHubName [UsrHK,GlbHK] hn
        _ <- checkHubName [UsrHK      ] hn'
        when (hn==hn') $
            oops HubO $ printf "%s: same source and destination" hn'
        when sw $
            userHubExists    hn'
        when (not sw) $
            userHubAvailable hn'
        return hub


cabal_fixup :: Hub -> Prog -> [String] -> IO CommandLine
cabal_fixup hub prg as = ProgCL hub `fmap` cf_as 
      where
        cf_as  = case enmPROG prg of
                   CabalP | kind__HUB hub /= GlbHK -> ci_fixup hub prg as
                   _                               -> return (prg,as) 

ci_fixup :: Hub -> Prog -> [String] -> IO (Prog,[String])
ci_fixup hub prg as =
     do db <- hubUserPackageDBPath hub
        case as of
          "install"  :as' -> alloc db "install"   as'
          "upgrade"  :as' -> alloc db "upgrade"   as'
          "configure":as' -> alloc db "configure" as'
          _               -> return (prg,as)
      where
        alloc db cmd as' =
             do ln <- allocate
                let prg' = prg { typPROG = CiPT (Just ln) }
                return ( prg' , cmd : _ld ln : _pd db : as' )                                 

        _ld ln = "--libdir="     ++ ln
        _pd db = "--package-db=" ++ db

