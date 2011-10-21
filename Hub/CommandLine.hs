module Hub.CommandLine
    ( commandLine
    , CommandLine(..)
    , Prog(..)
    , P(..)
    , ProgType(..)
    , p2prog
    , readHub
    ) where

import           Char
import           Maybe
import           Monad
import           IO
import           System
import           System.Directory
import           System.FilePath
import qualified Data.Map       as Map
import           Text.Printf
import           Text.Regex
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

data ProgType = HcPT | CiPT (Maybe FilePath) | HpPT
                                                                deriving (Show)


prog, hub_dispatch :: [String] -> IO (Maybe CommandLine)

prog as =
     do pn <- getProgName
        let (_,p_s) = splitFileName pn
        case Map.lookup p_s prog_mp of
          Nothing  -> return Nothing
          Just prg -> 
             do hub <- current_hub
                Just `fmap` cabal_fixup hub prg as 

hub_dispatch as = case as of
    ["--help"     ] ->                                      return $ Just $ HelpCL False help
    ["help"       ] ->                                      return $ Just $ HelpCL False help
    ["--help",cd  ] -> lu_help cd               >>= \hlp -> return $ Just $ HelpCL False hlp
    ["help"  ,cd  ] -> lu_help cd               >>= \hlp -> return $ Just $ HelpCL False hlp
    ["--version"  ] ->                                      return $ Just VrsnCL
    ["version"    ] ->                                      return $ Just VrsnCL
    ["--usage"    ] ->                                      return $ Just $ HelpCL False usage
    ["usage"      ] ->                                      return $ Just $ HelpCL False usage
    ["default"    ] ->                                      return $ Just $ DfltCL
    ["default","-"] ->                                      return $ Just $ RsDfCL
    ["default",hn ] -> readHub          hn      >>= \hub -> return $ Just $ StDfCL hub
    ["ls"         ] -> createHubDirs            >>= \_   -> return $ Just $ LsCL
    ["set"        ] ->                                      return $ Just $ GetCL
    ["set","-"    ] ->                                      return $ Just $ UnsetCL
    ["set",hn     ] -> readHub          hn      >>= \hub -> return $ Just $ SetCL  hub
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
    ["swap",   hn'] -> hub_swap Nothing     hn' >>= \hub -> return $ Just $ SwapCL hub hn'
    ["swap",hn,hn'] -> hub_swap (Just   hn) hn' >>= \hub -> return $ Just $ SwapCL hub hn'
    _               ->                                      return   Nothing  

usage :: String
usage = unlines $ filter is_hdr $ lines help
      where
        is_hdr ('h':'u':'b':' ':_) = True
        is_hdr _                   = False

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
    | CabalP
    | AlexP
    | HappyP
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
      AlexP              -> PROG p "alex"                 HpPT
      HappyP             -> PROG p "happy"                HpPT

prog_mp :: Map.Map String Prog
prog_mp = Map.fromList [ (nmePROG pg,pg) | pg<-map p2prog [minBound..maxBound] ]


lu_help :: String -> IO String
lu_help "--usage"   = dd_help `fmap` lu_help "usage"
lu_help "--help"    = dd_help `fmap` lu_help "help"
lu_help "--version" = dd_help `fmap` lu_help "version"
lu_help cd =
        case sc_help cd $ lines help of
          Nothing  -> oops HubO $ printf "%s: hub command not recognised" cd
          Just hlp -> return hlp

dd_help :: String -> String
dd_help ('h':'u':'b':' ':t) = "hub --" ++ t
dd_help hlp                 = hlp

sc_help :: String -> [String] -> Maybe String
sc_help _  []       = Nothing
sc_help cd (ln:lns) =
        case is_help_hdr cd ln of
          True  -> Just $ unlines $ ln : takeWhile is_help_bdy lns
          False -> sc_help cd lns

is_help_hdr :: String -> String -> Bool
is_help_hdr cmd = match $ mk_re $ printf "hub %s.*" cmd

is_help_bdy :: String -> Bool
is_help_bdy = not . match not_help_bd_re

not_help_bd_re :: Regex
not_help_bd_re = mk_re "hub.*"

      
current_hub :: IO Hub
current_hub = which_hub >>= readHub

hub_swap :: Maybe HubName -> HubName -> IO Hub
hub_swap Nothing   hn' = which_hub >>= \hn -> hub_pair' True  hn hn'
hub_swap (Just hn) hn' =                      hub_pair' True  hn hn'

hub_pair :: Maybe HubName -> HubName -> IO Hub
hub_pair Nothing   hn' = which_hub >>= \hn -> hub_pair' False hn hn'
hub_pair (Just hn) hn' =                      hub_pair' False hn hn' 

hub_pair' :: Bool -> HubName -> HubName -> IO Hub
hub_pair' sw hn hn' = 
     do checkHubName AnyHT hn
        checkHubName UsrHT hn'
        when (hn==hn') $
            oops HubO $ printf "%s: same source and destination" hn
        when sw $
            userHubExists    hn'
        when (not sw) $
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



cabal_fixup :: Hub -> Prog -> [String] -> IO CommandLine
cabal_fixup hub prg as = ProgCL hub `fmap` cf_as 
      where
        cf_as  = case enmPROG prg of
                   CabalP -> ci_fixup hub prg as
                   _      -> return (prg,as) 

ci_fixup :: Hub -> Prog -> [String] -> IO (Prog,[String])
ci_fixup hub prg as =
     do db <- snd `fmap` hubUserLib hub
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

default_global_hub :: IO Hub
default_global_hub =
     do hn <- defaultGlobalHubName
        hf <- case isGlobal hn of
                True  -> return $ globalHubPath hn
                False -> userHubPath hn
        parse hn hf

mk_re :: String -> Regex
mk_re re_str = mkRegexWithOpts (printf "^%s$" re_str) False True

match :: Regex -> String -> Bool
match re = isJust . matchRegex re

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
