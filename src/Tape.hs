-- for Binary
{-# LANGUAGE DeriveGeneric #-}

module Tape where

import           Control.Concurrent.MVar (MVar)
import           Control.Monad           (when)
import           Control.Monad.Fix       (fix)
import qualified Data.Binary             as BI
import qualified Data.ByteString         as B
import           GHC.Generics            (Generic)
import           ShortcutsAndStuff
import qualified System.Directory        as D
import           System.IO               (Handle, hClose, hFlush)
import           System.IO.Error         (catchIOError)
import           System.Posix.Signals    (Handler (CatchInfo), installHandler,
                                          keyboardSignal, siginfoSignal)
import qualified System.Process          as P


data Stream = In | Out | Err deriving (Generic, Show)

data Event  = Command FilePath [String] -- cwd cmd
            | Data Stream B.ByteString
            | Close Stream
            | Exit Int
            | Signal Int
            deriving (Generic, Show)

data Record = Record Ms Event deriving (Generic, Show)

-- TODO remove Binary + Generic and make orphan instances outside?
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

capture :: [String] -> Handle -> IO (MVar Record)
capture cmd inH = do
  let p = (P.proc (head cmd) (tail cmd))
            { P.std_in  = P.CreatePipe
            , P.std_out = P.CreatePipe
            , P.std_err = P.CreatePipe }

  eventsM <- mvar

  cwd <- D.getCurrentDirectory
  mput eventsM $ Command cwd cmd

  oldKeyboardHandler <- installHandler
    keyboardSignal
    (CatchInfo (\i -> mput eventsM $ Signal . fromIntegral . siginfoSignal $ i))
    Nothing

  (Just cin, Just cout, Just cerr, h) <- P.createProcess p

  -- TODO when ^C is caught during recording,
  --      pass it to child process first + record exit code, then exit?

  -- convert child process events to a sequence of Events
  pumps <- sequence [ fork $ pumpHandle2MVar In  inH  eventsM
                    , fork $ pumpHandle2MVar Out cout eventsM
                    , fork $ pumpHandle2MVar Err cerr eventsM ]

  recordsM <- mvar

  -- wait for stdout+stderr handles to close, only then put exit code
  fork $ do
    mapM_ (mtake . snd) $ tail pumps
    ec <- P.waitForProcess h
    mput eventsM $ Exit (ec2n ec)

    -- TODO race condition: can't call again, until results from preceding `capture` are fully pumped out
    -- restore previous keyboard signal handler
    installHandler
      keyboardSignal
      oldKeyboardHandler
      Nothing

    return ()

  -- consume Events until all 3 conditions satisfy: stdout and stderr closed, child process exited
  fork $ flip fix -- flip fix :: b -> ((b -> c) -> (b -> c)) -> c
    (ChildState True True True) -- initial state
    $ \loop childState ->
      when (childState /= ChildState False False False) $ do
        e <- mtake eventsM

        nowMs <- now
        let r = Record nowMs e

        mput recordsM r

        -- pass to child process
        case e of
           Command _ _ -> loop childState
           Data s bs -> do case s of
                             In  -> catchIOError (B.hPut cin bs >> hFlush cin)
                                                 (\_ -> hClose inH)
                             Out -> return ()
                             Err -> return ()
                           loop childState

           Close s   -> loop $ case s of
                                 In  -> childState
                                 Out -> childState {outOpen = False}
                                 Err -> childState {errOpen = False}

           Exit _    -> loop $ childState {running = False}
           Signal _  -> loop childState

  return recordsM
