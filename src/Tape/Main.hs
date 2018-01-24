-- for cmdargs
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Tape.Main where

import           Data.List                       (isInfixOf)
import           Data.Maybe (isJust)
import           System.Console.CmdArgs          as Args
import           System.Console.CmdArgs.Explicit (processArgs)
import           System.IO                       (Handle, IOMode (..), stdin,
                                                  stdout, withBinaryFile)
import qualified Tape.Tape                       as Tape

data Tape = Record  {file :: Maybe String, command :: String, commandArgs :: [String]}
          | Replay  {file :: Maybe String, delays :: Bool}
          | Inspect {file :: Maybe String}
            deriving (Data, Typeable, Show, Eq)
main :: IO ()
main = do
  m <- processArgs $ cmdArgsMode $ modes
         [ Record  { file        = def
                   , command     = def &= argPos 0
                   , commandArgs = def &= args }
         , Replay  { file        = def
                   , delays      = def}
         , Inspect { file        = def} ]

  if isJust $ cmdArgsHelp m
    then putStrLn helpPage
    else
      case cmdArgsValue m of
        Record  fm cmd cargs  -> withFileOrHandle fm WriteMode stdout $ Tape.record (cmd:cargs)
        Replay  fm withDelays -> withFileOrHandle fm ReadMode  stdin  $ Tape.replay withDelays
        Inspect fm            -> withFileOrHandle fm ReadMode  stdin    Tape.inspect

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

withFileOrHandle :: Maybe FilePath -> IOMode -> Handle -> (Handle -> IO r) -> IO r
withFileOrHandle Nothing  _ h a = a h
withFileOrHandle (Just f) m _ a = withBinaryFile f m a
