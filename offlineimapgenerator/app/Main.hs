{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import OfflineImapGenerator
import qualified Options.Applicative as OA
import System.Environment

data Args = Args Bool

args :: OA.Parser Args
args =
  Args --TODO
    <$> OA.switch
      ( OA.long "verbose"
          <> OA.short 'v'
          <> OA.help "Be verbose."
      )

-- <*> OA.strArgument
-- (OA.metavar "another" <> OA.help "Another argument")

argsInfo :: OA.ParserInfo Args
argsInfo = OA.info args OA.fullDesc

main :: IO ()
main = do
  Args verbose <- OA.execParser argsInfo
  home <- getEnv "HOME"
  generate home verbose
