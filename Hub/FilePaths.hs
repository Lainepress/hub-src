module Hub.FilePaths where

hubLib, sysVersion, sysDefaultHubPath, defaultHubPath, globalHubDir, hubBin,
                                toolsBin, hubGccBin, hubBinutilsBin :: FilePath
hcBinREs, hpDbREs :: String
hubLib            = "/usr/hs/lib"
sysVersion        = "/usr/hs/lib/version.txt"
sysDefaultHubPath = "/usr/hs/lib/sys-default.hub"
defaultHubPath    = "/usr/hs/lib/the-default.hub"
globalHubDir      = "/usr/hs/hub"
hubBin            = "/usr/hs/bin"
toolsBin          = "/usr/hs/tools" 
hubGccBin         = "/usr/hs/gcc/bin"
hubBinutilsBin    = "/usr/hs/binutils/bin"
hcBinREs          = "/usr/hs/ghc/([a-zA-Z0-9_.-]+)/bin"
hpDbREs           = "/usr/hs/db/("++hp_re++")(\\.d)?"

hp_re :: String
hp_re = "20[0-9][0-9]\\.[0-9]\\.[0-9]\\.[0-9](-[a-z0-9]*)?"