module Hub.Oops
    ( Oops(..)
    , oops
    ) where

import System.IO
import System.Exit
import System.Environment
import System.FilePath
import Text.Printf

data Oops
    = PrgO
    | SynO
    | SysO
    | HubO
                                                                deriving (Show)

oops :: Oops -> String -> IO a
oops o0 msg = 
     do pn <- getProgName
        as <- getArgs
        let o  = refineO (snd $ splitFileName pn) o0
            ar = case as of
                   a:_ -> a
                   _   -> "*no program*"
        hPutStr stderr $ err pn ar o
        exitWith $ ExitFailure 1
      where
        err pn ar o = 
                case o of
                  PrgO -> printf "%s (hub wrapper):  %s\n" pn    msg
                  SynO -> printf "%s: %s\n"                pn    msg
                  SysO -> printf "%s (hub wrapper):  %s\n" pn    msg
                  HubO -> printf "%s %s: %s\n"             pn ar msg

refineO :: String -> Oops -> Oops
refineO "hub" _ = HubO
refineO _     o = o
        