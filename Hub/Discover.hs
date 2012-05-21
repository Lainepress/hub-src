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
discover Nothing               = which_hub >>= read_hub
discover (Just hn) | hn=="^"   = discover Nothing
                   | otherwise = read_hub (ClHS,hn)


read_hub :: (HubSource,HubName) -> IO Hub
read_hub (hs,hn) = 
     do hk <- checkHubName [minBound..maxBound] hn
        hubExists hn
        hf <-  case isHubName hn==Just GlbHK  of
                 True  -> return $ globalHubPath hn
                 False -> userHubPath hn
        dy <- defaultDirectoryPath
        parse hs dy hn hf hk 

which_hub :: IO (HubSource,HubName)
which_hub =
     do ei      <- tryIO $ getEnv "HUB"
        (hs,hn) <- case ei of
                     Left  _ -> dir_which_hub False
                     Right s -> env_which_hub s
        _       <- checkHubName [UsrHK,GlbHK] hn
        return (hs,hn)

env_which_hub :: String -> IO (HubSource,HubName)
env_which_hub str =
        case str of
          "--default" -> dir_which_hub True
          "--dir"     -> dir_which_hub False
          "--user"    -> usr_which_hub
          "--global"  -> (,) DsHS `fmap` defaultGlobalHubName
          _           -> return $ (EvHS,trim str)

dir_which_hub :: Bool -> IO (HubSource,HubName)
dir_which_hub def_usr = 
     do ds <- (reverse.splitDirectories) `fmap` getCurrentDirectory
        w_h ds
      where
        w_h [] | def_usr   = usr_which_hub
               | otherwise = (,) DsHS `fmap` defaultGlobalHubName
        w_h (d:ds)         = catchIO (here (d:ds)) (\_ -> w_h ds)
        
        here r_ds  = ((,) DrHS . trim) `fmap`
                            readAFile (joinPath $ reverse $ ".hub":r_ds)

usr_which_hub :: IO (HubSource,HubName)
usr_which_hub =
     do yup <- isUserHub home_hub
        case yup of
          True  -> return ()
          False -> default_global_hub >>= \hub -> createHub False hub home_hub
        return (DuHS,home_hub)

default_global_hub :: IO Hub
default_global_hub =
     do hn <- defaultGlobalHubName
        hf <- case isHubName hn==Just GlbHK of
                True  -> return $ globalHubPath hn
                False -> userHubPath hn
        dy <- defaultDirectoryPath
        parse DsHS dy hn hf GlbHK


tryIO :: IO a -> IO (Either IOError a)
tryIO = E.try

catchIO :: IO a -> (IOError->IO a) -> IO a
catchIO = E.catch
