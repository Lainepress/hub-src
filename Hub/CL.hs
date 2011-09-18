module Hub.CL
    ( cl
    , CL(..)
    , Prog(..)
    , P(..)
    , ProgType(..)
    , p2prog
    ) where


import Hub.Parse


cl :: IO CL
cl = undefined


data CL
    = RunCL Prog
    | HlpCL String
    | VrnCL
    | NameCL Hub  
    | PathLC Hub
    | CatCL  Hub
    | InitCL Hub
    | CpCL   Hub HubName
    | MvCL   Hub HubName
    | RmCL   Hub

data Prog = PROG {
    enmPROG :: P,
    nmePROG :: String,
    typPROG :: ProgType
    }                                                           deriving (Show)

data ProgType = HcPT | CiPT | HpPT
                                                                deriving (Show)
data P
    = GhcP
    | GhciP
    | Ghc_pkgP
    | HaddockP
    | Hp2psP
    | HpcP
    | Hsc2hsP
    | RunghcP
    | RunhaskellP
    | AlexP
    | Basic_testsP
    | CabalP
    | Extended_testsP
    | HappyP
    | Terminal_testsP
                                            deriving (Eq,Ord,Bounded,Enum,Show)

p2prog :: P -> Prog
p2prog p =
    case p of
      GhcP               -> PROG p "ghc"                  HcPT
      GhciP              -> PROG p "ghci"                 HcPT
      Ghc_pkgP           -> PROG p "ghc-pkg"              HcPT
      HaddockP           -> PROG p "haddock"              HcPT
      Hp2psP             -> PROG p "hp2ps"                HcPT
      HpcP               -> PROG p "hpc"                  HcPT
      Hsc2hsP            -> PROG p "hsc2hs"               HcPT
      RunghcP            -> PROG p "runghc"               HcPT
      RunhaskellP        -> PROG p "runhaskell"           HcPT
      CabalP             -> PROG p "cabal"                CiPT
      AlexP              -> PROG p "alex"                 HpPT
      Basic_testsP       -> PROG p "basic-tests"          HpPT
      Extended_testsP    -> PROG p "extended-tests"       HpPT
      HappyP             -> PROG p "happy"                HpPT
      Terminal_testsP    -> PROG p "terminal-tests"       HpPT
