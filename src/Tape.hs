module Tape ( Record (..)
            , Event (..)
            , Stream (..)
            , Exe
            , ExeArg
            , capture
            , encode
            , encodeList
            , decodeList
            ) where

import           Control.Concurrent.MVar       (MVar, newEmptyMVar, putMVar,
                                                takeMVar)
import           Control.Monad                 (when)
import           Control.Monad.Fix             (fix)
import           Data.Binary                   (Binary)
import qualified Data.Binary                   as BI
import           Data.Binary.Get               (Decoder (..), runGetIncremental)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as L
-- import           Flat
import           GHC.Generics                  (Generic)
import qualified System.Directory              as D
import           System.IO                     (Handle, hClose, hFlush)
import           System.IO.Error               (catchIOError)
import           System.Posix.Signals          (Handler (CatchInfo),
                                                installHandler, keyboardSignal,
                                                siginfoSignal)
import qualified System.Process                as P

import           Util                          (Ms, ec2n, fork, now)

type Exe    = String
type ExeArg = String

data Stream = In | Out | Err
            deriving (Generic, Show)

data Event  = Command Exe [ExeArg] -- cwd cmd
            | Data Stream B.ByteString
            | Close Stream
            | Exit Int
            | Signal Int
            deriving (Generic, Show)

data Record = Record Ms Event
            deriving (Generic, Show)

instance BI.Binary Stream
instance BI.Binary Event
instance BI.Binary Record

data ChildState = ChildState
                { outOpen :: Bool
                , errOpen :: Bool
                , running :: Bool }
                deriving (Eq, Show)

pumpHandle2MVar :: Stream -> Handle -> MVar Event -> IO ()
pumpHandle2MVar stream h m =
    fix $ \loop ->
        catchIOError
            (do
                bs <- B.hGetSome h 32768
                if bs /= B.empty
                   then do
                       putMVar m (Data stream bs)
                       loop
                   else putMVar m (Close stream))

           (\_ -> putMVar m (Close stream))

capture :: Exe -> [ExeArg] -> Handle -> IO (MVar Record)
capture exe args inH = do
    let p = (P.proc exe args)
              { P.std_in  = P.CreatePipe
              , P.std_out = P.CreatePipe
              , P.std_err = P.CreatePipe }

    eventsM <- newEmptyMVar

    cwd <- D.getCurrentDirectory
    putMVar eventsM $ Command cwd (exe:args)

    oldKeyboardHandler <- installHandler
        keyboardSignal
        (CatchInfo (\i -> putMVar eventsM $ Signal . fromIntegral . siginfoSignal $ i))
        Nothing

    (Just cin, Just cout, Just cerr, h) <- P.createProcess p

  -- TODO when ^C is caught during recording,
  --      pass it to child process first + record exit code, then exit?

  -- convert child process events to a sequence of Events
    pumps <- sequence
        [ fork $ pumpHandle2MVar In  inH  eventsM
        , fork $ pumpHandle2MVar Out cout eventsM
        , fork $ pumpHandle2MVar Err cerr eventsM ]

    recordsM <- newEmptyMVar

    -- wait for stdout+stderr handles to close, only then put exit code
    fork $ do
        mapM_ (takeMVar . snd) $ tail pumps
        ec <- P.waitForProcess h
        putMVar eventsM $ Exit (ec2n ec)

      -- restore previous keyboard signal handler
      -- TODO race condition: can't call again, until results from preceding `capture` are fully pumped out
        installHandler keyboardSignal oldKeyboardHandler Nothing

        return ()

    -- consume Events until all 3 conditions satisfy: stdout and stderr closed, child process exited
    -- NOTE: flip fix :: b -> ((b -> c) -> (b -> c)) -> c
    fork $ flip fix
        (ChildState True True True) -- initial state
        $ \loop childState ->
            when (childState /= ChildState False False False) $ do
                e <- takeMVar eventsM
                nowMs <- now
                let r = Record nowMs e
                putMVar recordsM r

                -- pass to child process
                case e of
                    Command _ _ ->
                        loop childState

                    Data s bs -> do
                        case s of
                            In  -> catchIOError
                                       (B.hPut cin bs >> hFlush cin)
                                       (\_ -> hClose inH)
                            Out -> return ()
                            Err -> return ()
                        loop childState

                    Close s ->
                        loop $ case s of
                            In  -> childState
                            Out -> childState {outOpen = False}
                            Err -> childState {errOpen = False}

                    Exit _ ->
                        loop $ childState {running = False}

                    Signal _ ->
                        loop childState

    return recordsM

encode :: Binary b => b -> L.ByteString
encode = BI.encode

-- convert a list of "binaries" to a lazy ByteString
encodeList :: Binary b => [b] -> L.ByteString
encodeList xs = L.concat $ map BI.encode xs

-- convert a lazy ByteString into a list of "binaries"
decodeList :: Binary b => L.ByteString -> [b]
decodeList =
    -- feed chunks from a given lazy ByteString to a decoder.
    -- once the next "binary" is ready, cons it and keep going with the rest lazily
    go (runGetIncremental BI.get)
  where
    go :: Binary b => Decoder b -> L.ByteString -> [b]
    go (Done leftover _ !b) bs =
        let next = L.chunk leftover bs
        in b : if L.null next
          then []
          else go (runGetIncremental BI.get) next

    go (Partial k) bs =
        go (k (takeHeadChunk bs)) (dropHeadChunk bs)

    go (Fail _ offset msg) _ =
        error $ "decodeList at position " ++ show offset ++ ": " ++ msg

    takeHeadChunk = \case
        (L.Chunk bs _) -> Just bs
        _              -> Nothing

    dropHeadChunk = \case
        (L.Chunk _ bs) -> bs
        _              -> L.Empty
