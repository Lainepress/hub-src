
import System.Posix.IO
import System.Posix.Unistd
import GHC.IO.Device



main :: IO ()
main =
     do fd <- openFd "lock.hs" WriteOnly Nothing defaultFileFlags
        putStrLn "getting lock"
        waitToSetLock fd (WriteLock,AbsoluteSeek,0,0)
        putStrLn "lock acquired; sleeping"
        _ <- sleep 10
        putStrLn "closing file and exiting"
        closeFd fd
