module Hub.Oops
    ( Oops(..)
    , oops
    ) where

import IO
import System
import Text.Printf

data Oops
    = PrgO
    | SynO
    | SysO
    | HubO

oops :: Oops -> String -> IO a
oops o msg = 
     do pn <- getProgName
        as <- getArgs
        let ar = case as of
                   a:_ -> a
                   _   -> "*no program*"
        hPutStr stderr $ err pn ar
        exitWith $ ExitFailure 1
      where
        err pn ar = 
                case o of
                  PrgO -> printf "%s (hub wrapper):  %s\n" pn    msg
                  SynO -> printf "%s: %s\n"                pn    msg
                  SysO -> printf "%s (hub wrapper):  %s\n" pn    msg
                  HubO -> printf "%s %s: %s\n"             pn ar msg
