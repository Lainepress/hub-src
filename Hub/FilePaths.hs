--
-- >>> Hub.FilePaths <<<
--
-- This module abstracts the layout of /usr/hs -- the system hub area.
--
-- (c) 2011-2012 Chris Dornan


module Hub.FilePaths where

import           Text.Printf
import           Text.Regex
import           Data.Maybe
import           Hub.System


-- Work out the corresponding default user package database from a standard
-- (Hub) path to Haskell Platform package database; if the path is not
-- recognized as such then return Nothing.

hpGlbPdb2dfUsrPdb :: FilePath -> IO (Maybe String)
hpGlbPdb2dfUsrPdb glb = 
     do (sys,mac) <- systemMachine
        mb_hme    <- homeDir
        return $
             do hme <- mb_hme
                hpv <- match (mk_re hpDbREs) glb
                case hpv of
                  "2009.2.0.2" -> return $ mk hme mac sys "6.10.4" ""
                  "2010.2.0.0" -> return $ mk hme mac sys "6.12.3" ".d"
                  "2011.2.0.1" -> return $ mk hme mac sys "7.0.3"  ".d"
                  "2011.4.0.0" -> return $ mk hme mac sys "7.0.4"  ".d"
                  "2012.2.0.0" -> return $ mk hme mac sys "7.4.1"  ".d"
                  _            -> Nothing
      where
        mk = printf "%s/.ghc/%s-%s-%s/package.conf%s" 


hubLib, sysVersion, distroDefaultHubPath, sysDefaultHubPath,
                    defaultHubPath, globalHubDir,
                    toolsBin, hubGccBin, hubBinutilsBin :: FilePath

hcBinREs, globalHubREs, hpDbREs :: String

hubLib                  = "/usr/hs/lib"
sysVersion              = "/usr/hs/lib/version.txt"
distroDefaultHubPath    = "/usr/hs/lib/distro-default.hub"
sysDefaultHubPath       = "/usr/hs/lib/sys-default.hub"
defaultHubPath          = "/usr/hs/lib/the-default.hub"
globalHubDir            = "/usr/hs/hub"
toolsBin                = "/usr/hs/tools"
hubGccBin               = "/usr/hs/gcc/bin"
hubBinutilsBin          = "/usr/hs/binutils/bin"

hcBinREs                = "/usr/hs/ghc/([a-zA-Z0-9_.-]+)/bin"
globalHubREs            = "/usr/hs/db/([^/]+)\\.d"
hpDbREs                 = "/usr/hs/db/("++hp_re++")(\\.d)?"


hp_re :: String
hp_re = "20[0-9][0-9]\\.[0-9]\\.[0-9]\\.[0-9](-[a-z0-9]*)?"



--
-- Regular Expression Utilities
--


mk_re :: String -> Regex
mk_re re_str = mkRegexWithOpts (printf "^%s$" re_str) False True

gmatch :: Regex -> String -> Bool
gmatch re st = isJust $ matchRegex re st

match :: Regex -> String -> Maybe String
match re st = case matchRegex re st of
                Just (se:_) -> Just se
                _           -> Nothing
