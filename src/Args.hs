module Args
       ( TapeMode (..)
       , getHelpOrMode
       ) where

import           System.Console.CmdArgs          (Data, Typeable, argPos, args,
                                                  cmdArgsHelp, cmdArgsMode,
                                                  cmdArgsValue, def, explicit,
                                                  modes, name, (&=))
import           System.Console.CmdArgs.Explicit (processArgs)

data TapeMode = ModeRecord  { file    :: Maybe String
                            , exe     :: String
                            , exeArgs :: [String] }
              | ModeReplay  { file   :: Maybe String
                            , delays :: Bool }
              | ModeInspect { file :: Maybe String }
              deriving (Data, Typeable, Show, Eq)

getHelpOrMode :: IO (Either String TapeMode)
getHelpOrMode = do
    c <- processArgs $ cmdArgsMode $ modes
           [ ModeRecord  { file    = def
                         , exe     = def &= argPos 0
                         , exeArgs = def &= args } &= explicit &= name "record"
           , ModeReplay  { file    = def
                         , delays  = def         } &= explicit &= name "replay"
           , ModeInspect { file    = def         } &= explicit &= name "inspect"
           ]

    return $ case cmdArgsHelp c of
        Just _  -> Left helpPage
        Nothing -> Right $ cmdArgsValue c

helpPage :: String
helpPage = unlines
  [ "`tape` records and replays stdout/stderr produced by a command."
  , ""
  , "Usage:"
  , "  tape record  [-f <file.rec>] -- <command> [command-args]"
  , "  tape replay  [-f <file.rec>] [-d]"
  , "  tape inspect [-f <file.rec>]"
  , ""
  , "Flags:"
  , "  -f --file <file.rec>  Use a file instead of stdin/stdout"
  , "  -d --delays           Reproduce delays (off by default)"
  , ""
  , "Examples:"
  , "  tape record -- ping 8.8.8.8 > log.rec"
  , "  cat log.rec | tape replay -d"
  , "  cat log.rec | tape inspect"
  ]
