{-# LANGUAGE LambdaCase    #-}
-- for Binary
{-# LANGUAGE DeriveGeneric #-}

module Tape.Tape where

import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar (MVar)
import           Control.Monad           (foldM, mapM_, void, when)
import           Control.Monad.Fix       (fix)
import qualified Data.Binary             as BI
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import           GHC.Generics            (Generic)
import qualified GHC.IO.Handle           as H
import qualified System.Directory        as D
import qualified System.Environment      as E
import           System.IO               (Handle, hClose, hFlush, hPutStrLn,
                                          stderr, stdin, stdout)
import           System.IO.Error         (catchIOError)
import           System.Posix.Signals    (Handler (CatchInfo), installHandler,
                                          keyboardSignal, siginfoSignal)
import qualified System.Process          as P
import           Util


data Stream = In | Out | Err deriving (Generic, Show)

data Event  = Command FilePath [String] -- cwd cmd
            | Data Stream B.ByteString
            | Close Stream
            | Exit Int
            | Signal Int
            deriving (Generic, Show)

data Record = Record Ms Event deriving (Generic, Show)

instance BI.Binary Stream
instance BI.Binary Event
instance BI.Binary Record

data ChildState = ChildState { outOpen :: Bool
                             , errOpen :: Bool
                             , running :: Bool } deriving (Eq, Show)


pumpHandle2MVar :: Stream -> Handle -> MVar Event -> IO ()
pumpHandle2MVar stream h m = fix $ \loop ->
  catchIOError (do bs <- B.hGetSome h 32768
                   if bs /= B.empty
                      then mput m (Data stream bs) >> loop
                      else mput m (Close stream))
               (\_ -> mput m (Close stream))


record :: [String] -> Handle -> IO ()
record cmd hlog = do
  let p = (P.proc (head cmd) (tail cmd))
            { P.std_in  = P.CreatePipe
            , P.std_out = P.CreatePipe
            , P.std_err = P.CreatePipe }

  cwd <- D.getCurrentDirectory

  eventsM <- mvar
  mput eventsM $ Command cwd cmd

  installHandler
    keyboardSignal
    (CatchInfo (\i -> mput eventsM $ Signal . fromIntegral . siginfoSignal $ i))
    Nothing

  (Just cin, Just cout, Just cerr, h) <- P.createProcess p

  -- TODO when ^C is caught during recording,
  --      pass it to child process first + record exit code, then exit?

  -- convert child process events to a sequence of Events
  pumps <- sequence [ fork $ pumpHandle2MVar In  stdin eventsM
                    , fork $ pumpHandle2MVar Out cout  eventsM
                    , fork $ pumpHandle2MVar Err cerr  eventsM ]

  -- wait for stdout+stderr handles to close, only then put exit code
  fork $ do mapM_ (mtake . snd) $ tail pumps
            ec <- P.waitForProcess h
            mput eventsM $ Exit (ec2n ec)

  -- consume Events until all 3 conditions satisfy: stdout and stderr closed, child process exited
  flip fix -- flip fix :: b -> ((b -> c) -> (b -> c)) -> c
    (ChildState True True True) -- inital state
    $ \loop childState ->
      when (childState /= ChildState False False False) $ do
        e <- mtake eventsM

        nowMs <- now
        let r = Record nowMs e

        -- write to log
        L.hPut hlog $ BI.encode r

        -- pass to child process
        case e of
           Command _ _ -> loop childState
           Data s bs -> do case s of
                             In  -> catchIOError (B.hPut cin bs >> hFlush cin)
                                                 (\_ -> hClose stdin)
                             Out -> return ()
                             Err -> return ()
                           loop childState

           Close s   -> loop $ case s of
                                 In  -> childState
                                 Out -> childState {outOpen = False}
                                 Err -> childState {errOpen = False}

           Exit _    -> loop $ childState {running = False}
           Signal _  -> loop childState


replay :: Bool -> Handle -> IO ()
replay withDelays hlog = do
  records <- decodeList <$> L.hGetContents hlog

  let (Record recordStartedMs _) = head records -- TODO the log is never empty, but good to have it fixed
  replayStartedMs <- now

  exitCodeMb <- foldM (\ecMb (Record ms event) -> do
                  when withDelays $ do
                    nowMs <- now
                    let waitMs = max 0 $ (ms - recordStartedMs) - (nowMs - replayStartedMs)
                    --                   \__ from recording __/   \__ replay lag comp. __/
                    sleep waitMs

                  case event of
                    Command _ _ -> return ecMb
                    Data s bs   -> do case s of In  -> return ()
                                                Out -> B.hPut stdout bs
                                                Err -> B.hPut stderr bs
                                      return ecMb
                    Close _     -> return ecMb
                    Exit n      -> return $ Just n
                    Signal _    -> return ecMb)
                Nothing
                records

  case exitCodeMb of
    Nothing -> exit 1 -- TODO report absence of exit code in the log? (possibly corrupted log)
    Just n  -> exit n

-- TODO add an option to print human-readable time stamps? (in UTC TZ?)
inspect :: Handle -> IO ()
inspect hlog = do
  records <- decodeList <$> L.hGetContents hlog
  mapM_ (\(Record ms e) ->
           putStrLn $ show ms ++ " -- " ++ showEvent e)
        records

showEvent :: Event -> String
showEvent = \case
  Data s bs -> show s ++ " <" ++ show (B.length bs) ++ ">"
  e         -> show e
