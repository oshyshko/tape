{-# LANGUAGE LambdaCase         #-}
-- for cmdargs
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Control.Monad                   (foldM, mapM_, when)
import           Control.Monad.Fix               (fix)
import qualified Data.Binary                     as BI
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as L
import           Data.Maybe                      (isJust)
import           ShortcutsAndStuff
import           System.Console.CmdArgs          as Args
import           System.Console.CmdArgs.Explicit (processArgs)
import           System.IO                       (Handle, IOMode (..), stderr,
                                                  stdin, stdout, withBinaryFile)
import           Tape

data Tape = ModeRecord  {file :: Maybe String, command :: String, commandArgs :: [String]}
          | ModeReplay  {file :: Maybe String, delays :: Bool}
          | ModeInspect {file :: Maybe String}
            deriving (Data, Typeable, Show, Eq)


main :: IO ()
main = do
  m <- processArgs $ cmdArgsMode $ modes
         [ ModeRecord  { file        = def
                       , command     = def &= argPos 0
                       , commandArgs = def &= args } &= explicit &= name "record"
         , ModeReplay  { file        = def
                       , delays      = def }         &= explicit &= name "replay"
         , ModeInspect { file        = def }         &= explicit &= name "inspect"
         ]

  if isJust $ cmdArgsHelp m
    then putStrLn helpPage
    else
      case cmdArgsValue m of
        ModeRecord  fm cName cArgs -> withBinaryFileOrHandle fm WriteMode stdout $ Main.record (cName:cArgs)
        ModeReplay  fm withDelays  -> withBinaryFileOrHandle fm ReadMode  stdin  $ replay withDelays
        ModeInspect fm             -> withBinaryFileOrHandle fm ReadMode  stdin    inspect


record :: [String] -> Handle -> IO ()
record cmd logOutH = do
  recordsM <- capture cmd stdin

  -- pump Records from `capture` and keep serializing to a log until getting `Exit` event
  fix $ \loop -> do
    r <- mtake recordsM

    L.hPut logOutH $ BI.encode r

    case r of
      Record _ (Exit _) -> return ()
      _                 -> loop


replay :: Bool -> Handle -> IO ()
replay withDelays logInH = do
  records <- decodeList <$> L.hGetContents logInH

  -- TODO the log is never empty, but it would be good to check it
  let (Record recordStartedMs _) = head records
  replayStartedMs <- now

  -- deserialize Records and send to stdout/stderr
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

  -- TODO report a broken log if there were records after Exit or in case of multiple Exits
  case exitCodeMb of
    Nothing -> exit 1 -- TODO report a broken log in the absence of exit code
    Just n  -> exit n


-- TODO add an option to print human-readable time stamps? (in UTC TZ?)
inspect :: Handle -> IO ()
inspect logInH = do
  -- deserialize Records and print
  records <- decodeList <$> L.hGetContents logInH
  mapM_ (\(Record ms e) ->
           putStrLn $ show ms ++ " -- " ++ showEvent e)
        records


showEvent :: Event -> String
showEvent = \case
  Data s bs -> show s ++ " <" ++ show (B.length bs) ++ ">"
  e         -> show e


helpPage :: String
helpPage = unlines
  [ "`tape` records and replays stdout/stderr produced by a command."
  , ""
  , "Usage:"
  , "  tape record  [-f=file] -- command [args]"
  , "  tape replay  [-f=file]"
  , "  tape inspect [-f=file] [-d]"
  , ""
  , "Flags:"
  , "  -f --file=file  Use a file instead of stdin/stdout"
  , "  -d --delays     Reproduce delays (off by default)"
  , ""
  , "Examples:"
  , "  tape record -- traceroute 8.8.8.8 > log.rec"
  , "  cat log.rec | tape replay -d"
  , "  cat log.rec | tape inspect"
  ]

withBinaryFileOrHandle :: Maybe FilePath -> IOMode -> Handle -> (Handle -> IO r) -> IO r
withBinaryFileOrHandle Nothing  _ h a = a h
withBinaryFileOrHandle (Just f) m _ a = withBinaryFile f m a
