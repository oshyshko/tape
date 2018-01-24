{-# LANGUAGE LambdaCase #-}

module Util where

import           Control.Concurrent      (forkFinally, forkIO, threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception       (uninterruptibleMask_)
import           Control.Monad           (when)
import qualified Data.Binary             as BI
import qualified Data.Binary.Get         as BIG
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import           Data.Ratio              (numerator)
import           Data.Time.Clock.POSIX   (getPOSIXTime)
import           GHC.Conc.Sync           (ThreadId)
import           Numeric                 (showHex)
import           System.Exit             (ExitCode (..), exitWith)
import qualified System.Exit             as SE
import qualified System.Process          as P

-- concurrency
mvar :: IO (MVar a)
mvar = newEmptyMVar

mput :: MVar a -> a -> IO ()
mput = putMVar

mtake :: MVar a -> IO a
mtake = takeMVar

fork :: IO () -> IO (ThreadId, MVar ())
fork f = do
  mOnExit <- mvar
  tid <- forkFinally f (\_ -> putMVar mOnExit ())
  return (tid, mOnExit)

-- time
type Ms = Int

now :: IO Ms
now = timeInMillis where
  timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime
  timeInMillis = (`div` 1000) . fromIntegral <$> timeInMicros

-- pprint and debugging
pprint :: L.ByteString -> String
pprint = concatMap (`showHex` "") . L.unpack

-- process
die :: String -> IO a
die = SE.die

exit :: Int -> IO a
exit = exitWith . n2ec

ec2n :: ExitCode -> Int
ec2n = \case ExitSuccess   -> 0
             ExitFailure n -> n

n2ec :: Int -> ExitCode
n2ec = \case 0 -> ExitSuccess
             n -> ExitFailure n

sleep :: Ms -> IO ()
sleep ms = threadDelay $ ms * 1000

-- NOTE: a workaround to P.waitForProcess causing operations on handles to block
--       forever when a handle is read from another thread
waitFor :: P.ProcessHandle -> IO Int
waitFor h = let loop = P.getProcessExitCode h >>=
                          \case Nothing -> sleep 1000 >> loop
                                Just ec -> return $ ec2n ec
              in loop

-- serialization
decodeList :: BI.Binary b => L.ByteString -> [b]
decodeList bs
   | L.null bs = []
   | otherwise =
      let (x, xs, _) = BIG.runGetState BI.get bs 0 -- TODO migrate to runGetIncremental
      in x : decodeList xs

encodeList :: BI.Binary b => [b] -> L.ByteString
encodeList xs = L.concat $ map BI.encode xs
