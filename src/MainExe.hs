module Main
       ( main
       ) where

import           Control.Concurrent.MVar (takeMVar)
import           Control.Monad           (foldM, when)
import           Control.Monad.Fix       (fix)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import           Data.Functor            ((<&>))
import           System.IO               (Handle, IOMode (..), hFlush, stderr,
                                          stdin, stdout, withBinaryFile)

import           Args                    (TapeMode (..), getHelpOrMode)
import           Tape                    (Event (..), Record (..), Stream (..),
                                          capture, decodeList, encode)
import           Util                    (exit, now, sleep)

main :: IO ()
main =
    getHelpOrMode >>= \case
        Left helpPage -> putStrLn helpPage
        Right mode ->
            let (ioMode, defaultHandle, h2io) = case mode of
                    ModeRecord  _ exe exeArgs -> (WriteMode, stdout, record exe exeArgs)
                    ModeReplay  _ withDelays  -> (ReadMode,  stdin,  replay withDelays)
                    ModeInspect _             -> (ReadMode,  stdin,  inspect)

            in case file mode of
                Just path -> withBinaryFile path ioMode h2io
                Nothing   -> h2io defaultHandle


record :: String -> [String] -> Handle -> IO ()
record exe exeArgs logH = do
    recordsM <- capture exe exeArgs stdin

    -- pump Records from `capture` and keep serializing to a log until getting `Exit` event
    fix $ \loop -> do
        r <- takeMVar recordsM

        L.hPut logH $ encode r
        hFlush logH

        case r of
            Record _ (Exit _) -> return ()
            _                 -> loop


replay :: Bool -> Handle -> IO ()
replay withDelays logH = do
    records <- decodeList <$> L.hGetContents logH

    -- TODO the log is never empty, but it would be good to check it
    let (Record recordStartedMs _) = head records
    replayStartedMs <- now

    -- deserialize Records and send to stdout/stderr
    exitCodeMb <- foldM
        (\mEc (Record ms event) -> do
            when withDelays $ do
                nowMs <- now
                let waitMs = max 0 $ (ms - recordStartedMs) - (nowMs - replayStartedMs)
                --                   \__ from recording __/   \__ replay lag comp. __/
                sleep waitMs

            case event of
                Command _ _ -> return mEc
                Data s bs   -> do
                    case s of
                        In  -> return ()
                        Out -> B.hPut stdout bs >> hFlush stdout
                        Err -> B.hPut stderr bs >> hFlush stderr
                    return mEc
                Close _     -> return mEc
                Exit n      -> return $ Just n
                Signal _    -> return mEc)

        Nothing
        records

    -- TODO report broken log if there were records after Exit or multiple Exits
    case exitCodeMb of
        Nothing -> exit 1 -- TODO report broken log if there's no Exit in the end
        Just n  -> exit n


-- TODO add an option to print human-readable time stamps? (in UTC TZ?)
inspect :: Handle -> IO ()
inspect logH =
    L.hGetContents logH
        <&> decodeList
        >>= mapM_ (\(Record ms e) -> putStrLn $ show ms ++ " -- " ++ showEvent e)
  where
    showEvent = \case
        Data s bs -> show s ++ " <" ++ show (B.length bs) ++ ">"
        e         -> show e
