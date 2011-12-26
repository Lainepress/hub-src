module Hub.Discover
    ( discover
    ) where

import qualified Control.Exception      as E
import           System.Directory
import           System.FilePath
import           System.Environment
import           Hub.System
import           Hub.Oops
import           Hub.Hub
import           Hub.Parse
import           Hub.Directory


home_hub ::HubName
home_hub = "home" 

discover :: Maybe HubName -> IO Hub
discover Nothing   = which_hub >>= read_hub
discover (Just hn) = read_hub hn


read_hub :: HubName -> IO Hub
read_hub hn = 
     do hk <- checkHubName [minBound..maxBound] hn
        hubExists hn
        hf <-  case isHubName hn==Just GlbHK  of
                 True  -> return $ globalHubPath hn
                 False -> userHubPath hn
        dy <- directoryPath
        parse dy hn hf hk 

which_hub :: IO HubName
which_hub =
     do ei <- tryIO $ getEnv "HUB"
        hn <- case ei of
                Left  _ -> trim `fmap` dir_which_hub False
                Right s -> trim `fmap` env_which_hub s
        _ <- checkHubName [UsrHK,GlbHK] hn
        return hn

env_which_hub :: String -> IO HubName
env_which_hub str =
        case str of
          "--default" -> dir_which_hub True
          "--dir    " -> dir_which_hub False
          "--user"    -> usr_which_hub
          "--global"  -> defaultGlobalHubName
          _           -> return str

dir_which_hub :: Bool -> IO HubName
dir_which_hub def_usr = 
     do ds <- (reverse.splitDirectories) `fmap` getCurrentDirectory
        w_h ds
      where
        w_h [] | def_usr   = usr_which_hub
               | otherwise = defaultGlobalHubName
        w_h (d:ds)         = catchIO (here (d:ds)) (\_ -> w_h ds)
        
        here r_ds  = readAFile (joinPath $ reverse $ ".hub":r_ds)

usr_which_hub :: IO HubName
usr_which_hub =
     do yup <- isUserHub home_hub
        case yup of
          True  -> return ()
          False -> default_global_hub >>= \hub -> createHub False hub home_hub
        return home_hub

default_global_hub :: IO Hub
default_global_hub =
     do hn <- defaultGlobalHubName
        hf <- case isHubName hn==Just GlbHK of
                True  -> return $ globalHubPath hn
                False -> userHubPath hn
        dy <- directoryPath
        parse dy hn hf GlbHK


tryIO :: IO a -> IO (Either IOError a)
tryIO = E.try

catchIO :: IO a -> (IOError->IO a) -> IO a
catchIO = E.catch
