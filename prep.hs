module Main(main) where

import Text.Printf

main :: IO ()
main =
     do cts <- readFile "help.txt"
        -- writeFile "Hub/Gelp.hs"
        writeFile "Hub/Help.hs" $ mk_text_mod "Hub.Help" "help" cts

mk_text_mod :: String -> String -> String -> String
mk_text_mod mn fn cts = 
        case lines cts of
          []     -> error "that is strange: the text file is empty"
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
