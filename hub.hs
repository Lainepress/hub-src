module Main(main) where 

import Control.Monad
import System.IO
import System.Exit
import Text.Printf
import Hub.System
import Hub.FilePaths
import Hub.CommandLine
import Hub.Commands


version :: String
version = "1.0"


main :: IO ()
main = 
     do cl <- commandLine
        case cl of
          ProgCL    hub (prg,as) -> _prog hub prg as
          HelpCL    err hlp      -> _help err hlp
          VrsnCL                 -> _vrsn
          DfltCL                 -> _default
          StDfCL    hub          -> _default_hub $ Just hub
          RsDfCL                 -> _default_hub   Nothing
          LsCL                   -> _ls
          GetCL                  -> _get
          SetCL     hub          -> _set     hub
          UnsetCL                -> _unset
          NameCL    hub          -> _name    hub
          InfoCL    hub          -> _info    hub
          PathCL    hub          -> _path    hub
          XmlCL     hub          -> _xml     hub
          InitCL    hub hn set   -> _init    hub hn set
          CpCL      hub hn       -> _cp      hub hn
          MvCL      hub hn       -> _mv      hub hn
          RmCL      hub          -> _rm      hub
          SwapCL    hub hn       -> _swap    hub hn
          GcCL                   -> _gc
          ListCL    hub          -> _list    hub
          CheckCL   hub          -> _check   hub
          LoadCL        hn fp    -> _load        hn fp
          SaveCL    hub    fp    -> _save    hub    fp
          VerifyCL  hub    fp sf -> _verify  hub    fp sf
          InstallCL hub pkns     -> _install hub pkns
          EraseCL   hub pkns     -> _erase   hub pkns
        

_help :: Bool -> String -> IO ()
_help False hlp = putStr hlp
_help True  hlp = hPutStrLn stderr hlp >> exitWith (ExitFailure 1)

_vrsn :: IO ()
_vrsn =
     do putStr $ printf "hub %s\n" version
        ex <- fileExists sysVersion
        when ex $
            readAFile sysVersion >>= putStr
