module Main(main) where 

import IO
import System
import Text.Printf
import Hub.CommandLine
import Hub.Prog
import Hub.Commands


version :: String
version = "0.0"


main :: IO ()
main = 
     do cl <- commandLine
        case cl of
          ProgCL  hub prg as -> _prog hub prg as
          HelpCL  err hlp    -> _help err hlp
          VrsnCL             -> _vrsn
          DfltCL             -> _default
          StDfCL  hub        -> _default_hub $ Just hub
          RsDfCL             -> _default_hub   Nothing
          LsCL               -> _ls
          GetCL              -> _get
          SetCL   hub        -> _set  hub
          UnsetCL            -> _unset
          NameCL  hub        -> _name hub
          InfoCL  hub        -> _info hub
          PathCL  hub        -> _path hub
          XmlCL   hub        -> _xml  hub
          InitCL  hub hn     -> _init hub hn
          CpCL    hub hn     -> _cp   hub hn
          MvCL    hub hn     -> _mv   hub hn
          RmCL    hub        -> _rm   hub
          SwapCL  hub hn     -> _swap hub hn
        

_help :: Bool -> String -> IO ()
_help False hlp = putStr hlp
_help True  hlp = hPutStrLn stderr hlp >> exitWith (ExitFailure 1)

_vrsn :: IO ()
_vrsn = putStrLn $ printf "hub %s" version
