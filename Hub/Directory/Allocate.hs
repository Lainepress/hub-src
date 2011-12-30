module Hub.Directory.Allocate
    ( allocate
    ) where

import qualified Control.Exception      as E
import           Text.Printf
import           System.Directory
import           System.Environment
import           Hub.System


-- allocate a library directory from the heap in the (default) directory

allocate :: IO FilePath
allocate =
     do hme <- home
        createDirectoryIfMissing True $ printf "%s/.hubrc/heap"             hme
        i <- inc                      $ printf "%s/.hubrc/heap/counter.txt" hme
        let pth =                       printf "%s/.hubrc/heap/%d"          hme i
        createDirectoryIfMissing True   pth
        return pth

home :: IO FilePath
home = catchIO (getEnv "HOME") $ \_ -> return "/"

catchIO :: IO a -> (IOError->IO a) -> IO a
catchIO = E.catch
