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
hcBinREs          = "/usr/hs/ghc/([a-z0-9.-_]+)/bin"
hpDbREs           = "/usr/hs/db/(20[a-z0-9.-_]+)(.db)?/bin"
