
import           System.Environment
import           System.FilePath
import qualified Data.Map             as Map


main :: IO ()
main = get_prog >>= print


get_prog :: IO Prog
get_prog =
     do pn <- getProgName
        let (_,p_s) = splitFileName pn
        case Map.lookup p_s prog_mp of
          Nothing -> error "Oops, program not recognised"
          Just p  -> return p

prog_mp :: Map.Map String Prog
prog_mp = Map.fromList
        [ (,) "ghc"         GhcP
        , (,) "ghc-pkg"     Ghc_pkgP
        , (,) "cabal"       CabalP
        ]

data Prog
        = GhcP
        | Ghc_pkgP
        | CabalP
                                                                deriving (Show)

is_ghc_prog :: Prog -> Bool
is_ghc_prog p =
        case p of
           GhcP      -> True
           Ghc_pkgP  -> True
           CabalP    -> False
