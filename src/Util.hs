module Util
       ( Ms
       , fork
       , now
       , die
       , exit
       , ec2n
       , n2ec
       , sleep
       , waitFor
       ) where

import           Control.Concurrent      (forkFinally, threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import           Data.Ratio              (numerator)
import           Data.Time.Clock.POSIX   (getPOSIXTime)
import           GHC.Conc.Sync           (ThreadId)
import           System.Exit             (ExitCode (..), exitWith)
import qualified System.Exit             as SE
import qualified System.Process          as P


-- concurrency
fork :: IO () -> IO (ThreadId, MVar ())
fork f = do
    mOnExit <- newEmptyMVar
    tid <- forkFinally f (\_ -> putMVar mOnExit ())
    return (tid, mOnExit)

-- time
type Ms = Int

now :: IO Ms
now =
    timeInMillis
  where
    timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime
    timeInMillis = (`div` 1000) . fromIntegral <$> timeInMicros

-- process
die :: String -> IO a
die = SE.die

exit :: Int -> IO a
exit = exitWith . n2ec

ec2n :: ExitCode -> Int
ec2n = \case
    ExitSuccess   -> 0
    ExitFailure n -> n

n2ec :: Int -> ExitCode
n2ec = \case
    0 -> ExitSuccess
    n -> ExitFailure n

sleep :: Ms -> IO ()
sleep ms = threadDelay $ ms * 1000

-- NOTE: a workaround to P.waitForProcess causing operations on handles to block
--       forever when a handle is read from another thread
waitFor :: P.ProcessHandle -> IO Int
waitFor h =
    P.getProcessExitCode h >>=
        \case
            Nothing -> sleep 1000 >> waitFor h
            Just ec -> return $ ec2n ec
