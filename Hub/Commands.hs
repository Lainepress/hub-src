module Hub.Commands
    ( _default
    , _default_hub
    , _ls
    , _get
    , _set
    , _unset
    , _name
    , _info
    , _path
    , _xml
    , _init
    , _cp
    , _mv
    , _rm
    , _swap
    ) where

import           Char
import           Monad
import           System.Cmd
import           System.Exit
import           System.FilePath
import           System.Directory
import           Text.Printf
import           Hub.Oops
import           Hub.System
import           Hub.Hub
import           Hub.Parse


_default :: IO ()
_default = defaultGlobalHubName >>= putStrLn

_default_hub :: Maybe Hub -> IO ()
_default_hub Nothing   = fileExists defaultHubPath >>= \ex -> when ex $ removeFile defaultHubPath
_default_hub (Just hb) = is_global hb >>= writeAFile defaultHubPath

_ls :: IO ()
_ls = do hns <- lsHubs AnyHT; putStr $ unlines hns

_get :: IO ()
_get =
     do yup <- fileExists ".hub"
        case yup of
          False -> putStrLn "No hub set for this directory"
          True  -> 
             do hn <- trim `fmap` readAFile ".hub"
                checkHubName AnyHT hn
                putStrLn hn
                
_set :: Hub -> IO ()
_set hub = writeAFile ".hub" $ name__HUB hub ++ "\n"

_unset ::   IO ()
_unset =
     do yup <- fileExists ".hub"
        case yup of
          False -> putStrLn "No hub set for this directory"
          True  -> removeFile ".hub"

_name :: Hub -> IO ()
_name hub = putStrLn $ name__HUB hub

_info :: Hub -> IO ()
_info hub = putStr $ unlines $
    [ printf "%s (%s hub)"                              name ht                                                             ] ++
    [ printf "   Toolchain       : GHC %s"              hc      |                         Just hc <- [bin2toolchain hc_bin] ] ++
    [ printf "   Tools           : %s"                  hc_bin                                                              ] ++
    [ printf "   Platform        : Haskell Platform %s" hp      | Just hp_bin <- [mb_hp], Just hp <- [bin2platform  hp_bin] ] ++ 
    [ printf "   Platform Tools  : %s"                  hp_bin  | Just hp_bin <- [mb_hp]                                    ] ++ 
    [ printf "   Cabal           : %s/cabal"            ci_bin
    ,        "   Package DBs"
    , printf "      global       : %s"                  glb_db                                                              ] ++
    [ printf "      user         : %s"                  usr_db  | Just usr_db <- [mb_ud]                                    ]
  where
    ht     = if isGlobal name then "global" else "user"
    name   = name__HUB hub
    hc_bin = hc_binHUB hub
    ci_bin = ci_binHUB hub
    mb_hp  = hp_binHUB hub
    glb_db = glb_dbHUB hub
    mb_ud  = usr_dbHUB hub

_path :: Hub -> IO ()
_path hub = putStrLn $ path__HUB hub

_xml :: Hub -> IO ()
_xml hub = readAFile (path__HUB hub) >>= putStr

_init :: Hub -> HubName -> IO ()
_init hub0 hn = 
     do createHubDirs
        userHubAvailable hn
        (h_fp,lib,db) <- userHubPaths hn
        createDirectoryIfMissing True lib
        pkg_init hub0 db
        let hub = hub0 { name__HUB=hn, path__HUB=h_fp, usr_dbHUB=Just db }
        dump hub 

_cp :: Hub -> HubName -> IO ()
_cp hub hn =
     do createHubDirs
        _    <- not_global hub
        lib0 <- fst `fmap` hubUserLib hub
        (h_fp,lib,db) <- userHubPaths hn
        cpFileDir lib0 lib
        let hub' = hub { name__HUB=hn, path__HUB=h_fp, usr_dbHUB=Just db }
        pkg_recache hub' db
        dump hub'

_mv :: Hub -> HubName -> IO ()
_mv hub hn =
     do createHubDirs
        _    <- not_global hub
        lib0 <- fst `fmap` hubUserLib hub
        (h_fp,lib,db) <- userHubPaths hn
        mvFileDir lib0 lib
        removeFile $ path__HUB hub
        let hub' = hub { name__HUB=hn, path__HUB=h_fp, usr_dbHUB=Just db }
        dump hub'

_rm :: Hub -> IO ()
_rm hub =
     do _ <- not_global hub
        (h_fp,lib,_) <- userHubPaths $ name__HUB hub
        removeFile h_fp
        removeRF   lib

_swap :: Hub -> HubName -> IO ()
_swap hub1 hn2 =
     do hub2 <- readHub hn2
        _    <- not_global hub1
        _    <- not_global hub2
        lib1 <- x_lib `fmap` userHubPaths (name__HUB hub1)
        lib2 <- x_lib `fmap` userHubPaths (name__HUB hub2)
        let hub1' = hub1 {name__HUB=name__HUB hub2,path__HUB=path__HUB hub2,usr_dbHUB=usr_dbHUB hub2}
            hub2' = hub2 {name__HUB=name__HUB hub1,path__HUB=path__HUB hub1,usr_dbHUB=usr_dbHUB hub1}
        dump hub1'
        dump hub2'
        swap_files lib1 lib2
      where
        x_lib (_,lib,_) = lib

pkg_init :: Hub -> FilePath -> IO ()
pkg_init hub fp =
     do ec <- rawSystem ghc_pkg ["init",fp]
        case ec of
          ExitSuccess   -> return ()
          ExitFailure n -> oops HubO $
                                printf "ghc-pkg failure (return code=%d)" n  
      where
        ghc_pkg = hc_binHUB hub </> "ghc-pkg"

pkg_recache :: Hub -> FilePath -> IO ()
pkg_recache hub fp = 
     do ec <- rawSystem ghc_pkg ["recache","-f",fp]
        case ec of
          ExitSuccess   -> return ()
          ExitFailure n -> oops HubO $
                                printf "ghc-pkg failure (return code=%d)" n  
      where
        ghc_pkg = hc_binHUB hub </> "ghc-pkg"

not_global :: Hub -> IO FilePath
not_global hub =
        case usr_dbHUB hub of
          Nothing -> oops HubO $ printf "%s: is a global hub" $name__HUB hub
          Just db -> return db

is_global :: Hub -> IO HubName
is_global hub =
        case usr_dbHUB hub of
          Nothing -> return                                    $ name__HUB hub
          Just  _ -> oops HubO $ printf "%s: not a global hub" $ name__HUB hub

swap_files :: FilePath -> FilePath -> IO ()
swap_files fp fp' = swap_files'' fp fp' $ oops SysO

--swap_files' :: FilePath -> FilePath -> IO () -> IO ()
--swap_files' fp fp' tdy = swap_files'' fp fp' $ \err -> tdy >> oops SysO err

swap_files'' :: FilePath -> FilePath -> (String->IO ()) -> IO ()
swap_files'' fp fp' h = catch (sw_files fp fp') $ \_ -> h err
      where
        err = printf "Failed to swap %s and %s (permissions?)" fp fp' 

sw_files :: FilePath -> FilePath -> IO ()
sw_files fp_1 fp_2 =
     do fp_t <- mk_tmp 0 fp_1
        mvFileDir fp_1 fp_t
        mvFileDir fp_2 fp_1
        mvFileDir fp_t fp_2

mk_tmp :: Int -> FilePath -> IO FilePath
mk_tmp i fp =
     do yup <- fileDirExists fp'
        case yup of
          True  -> mk_tmp (i+1) fp
          False -> return fp' 
      where
        fp' = printf "%s-%d" fp i

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
