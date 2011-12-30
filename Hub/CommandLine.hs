module Hub.CommandLine
    ( commandLine
    , CommandLine(..)
    ) where

import           Text.Printf
import           Control.Monad
import           System.Environment
import           System.FilePath
import qualified Data.Map       as Map
import           Hub.Help
import           Hub.Oops
import           Hub.Prog
import           Hub.Hub
import           Hub.Directory
import           Hub.Discover
import           Hub.PackageDB


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
    | StDfCL    Hub
    | RsDfCL
    | LsCL
    | GetCL
    | SetCL     Hub
    | UnsetCL
    | NameCL    Hub
    | InfoCL    Hub
    | PathCL    Hub
    | XmlCL     Hub
    | InitCL    Hub HubName
    | CpCL      Hub HubName
    | MvCL      Hub HubName
    | RmCL      Hub
    | SwapCL    Hub HubName
    | GcCL
    | ListCL    Hub
    | CheckCL   Hub
    | InstallCL Hub [PkgNick]
    | EraseCL   Hub [PkgNick]
                                                                deriving (Show)



prog, hub_dispatch :: [String] -> IO (Maybe CommandLine)

prog as =
     do pn <- getProgName
        let (_,p_s) = splitFileName pn
        case Map.lookup p_s progMap of
          Nothing  -> return Nothing
          Just prg -> 
                 do hub <- discover Nothing
                    return $ Just $ ProgCL hub (prg,as)


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
    ["default",hn ] -> discover (Just   hn)      >>= \hub -> return $ Just $ StDfCL  hub
    ["ls"         ] -> initDirectory             >>= \_   -> return $ Just $ LsCL
    ["set"        ] ->                                       return $ Just $ GetCL
    ["set","-"    ] ->                                       return $ Just $ UnsetCL
    ["set",hn     ] -> discover (Just   hn)      >>= \hub -> return $ Just $ SetCL   hub
    ["name"       ] -> discover Nothing          >>= \hub -> return $ Just $ NameCL  hub
    ["info"       ] -> discover Nothing          >>= \hub -> return $ Just $ InfoCL  hub
    ["info",hn    ] -> discover (Just   hn)      >>= \hub -> return $ Just $ InfoCL  hub
    ["path"       ] -> discover Nothing          >>= \hub -> return $ Just $ PathCL  hub
    ["path",hn    ] -> discover (Just   hn)      >>= \hub -> return $ Just $ PathCL  hub
    ["xml"        ] -> discover Nothing          >>= \hub -> return $ Just $ XmlCL   hub
    ["xml" ,hn    ] -> discover (Just   hn)      >>= \hub -> return $ Just $ XmlCL   hub
    ["init"   ,hn'] -> hub_pair Nothing     hn'  >>= \hub -> return $ Just $ InitCL  hub hn'
    ["init",hn,hn'] -> hub_pair (Just   hn) hn'  >>= \hub -> return $ Just $ InitCL  hub hn'
    ["cp"     ,hn'] -> hub_pair Nothing     hn'  >>= \hub -> return $ Just $ CpCL    hub hn'
    ["cp"  ,hn,hn'] -> hub_pair (Just   hn) hn'  >>= \hub -> return $ Just $ CpCL    hub hn'
    ["mv"     ,hn'] -> hub_pair Nothing     hn'  >>= \hub -> return $ Just $ MvCL    hub hn'
    ["mv"  ,hn,hn'] -> hub_pair (Just   hn) hn'  >>= \hub -> return $ Just $ MvCL    hub hn'
    ["rm"     ,hn'] -> discover       (Just hn') >>= \hub -> return $ Just $ RmCL    hub
    ["swap",   hn'] -> hub_swap Nothing     hn'  >>= \hub -> return $ Just $ SwapCL  hub hn'
    ["swap",hn,hn'] -> hub_swap (Just   hn) hn'  >>= \hub -> return $ Just $ SwapCL  hub hn'
    ["gc"         ] ->                                       return $ Just $ GcCL
    ["list"       ] -> discover Nothing          >>= \hub -> return $ Just $ ListCL  hub
    ["list",hn    ] -> discover (Just   hn)      >>= \hub -> return $ Just $ ListCL  hub
    ["check"      ] -> discover Nothing          >>= \hub -> return $ Just $ CheckCL hub
    ["check",hn   ] -> discover (Just   hn)      >>= \hub -> return $ Just $ CheckCL hub
    "install"     :   p:ps -> hub_pks  Nothing  p ps >>= \(hub,pkns) -> return $ Just $ InstallCL hub $ pkns
    "install-into":hn:p:ps -> hub_pks (Just hn) p ps >>= \(hub,pkns) -> return $ Just $ InstallCL hub $ pkns
    "erase"       :   p:ps -> hub_pks  Nothing  p ps >>= \(hub,pkns) -> return $ Just $ EraseCL   hub $ pkns
    "erase-from"  :hn:p:ps -> hub_pks (Just hn) p ps >>= \(hub,pkns) -> return $ Just $ EraseCL   hub $ pkns
    _               ->                                       return   Nothing  



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

hub_pks :: Maybe HubName -> String -> [String] -> IO (Hub,[PkgNick])
hub_pks mb p ps =
     do hub  <- discover mb
        pkns <- mapM parsePkgNick (p:ps)
        return (hub,pkns)

{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
        