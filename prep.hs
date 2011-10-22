module Main(main) where

import System.Locale
import Data.Time
import Text.Printf

main :: IO ()
main =
     do cts <- readFile "help.txt"
        writeFile "Hub/Help.hs" $ mk_text_mod  "Hub.Help" "help" cts
        ztm <- getZonedTime
        writeFile "Hub/Build.hs" $ mk_time_mod "Hub.Build" "build" ztm

mk_time_mod :: String -> String -> ZonedTime -> String
mk_time_mod mn fn zt = unlines
                   [ printf "module %s(%s) where" mn fn
                   ,        ""
                   , printf "%s :: String" fn
                   , printf "%s = %s"      fn $ show $
                                    formatTime defaultTimeLocale "%F  %T %z" zt                                    
                   ]

mk_text_mod :: String -> String -> String -> String
mk_text_mod mn fn cts = 
        case lines cts of
          []     -> error "that is strange, the text file is empty"
          ln:lns -> unlines $ pre ln ++ foldr mdl pst lns
      where
        pre ln   = [ printf "module %s(%s) where" mn fn
                   ,        ""
                   , printf "%s :: String"        fn
                   , printf "%s = unlines"        fn
                   , printf "    [ %s"     $ show ln
                   ]
                            
        mdl ln t = [ printf "    , %s"     $ show ln
                   ] ++ t
        
        pst      = [        "    ]"
                   ]
