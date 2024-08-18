module Credentials where

import Control.Applicative ((<**>))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Entities (CmdOpts (CmdOpts))
import qualified Options.Applicative as OA
import System.Environment (getEnv)

data Credentials = Credentials
  { bkToken :: String,
    bkUser :: String,
    bkPasswd :: String
  }

getUser :: Credentials -> ByteString
getUser = pack . bkUser

getPassword :: Credentials -> ByteString
getPassword = pack . bkPasswd

getToken :: Credentials -> ByteString
getToken = pack . bkToken

loadCreds :: IO Credentials
loadCreds =
  Credentials
    <$> getEnv "BK_TOK"
    <*> getEnv "BK_USR"
    <*> getEnv "BK_PASS"

buildOpts :: OA.Parser CmdOpts
buildOpts =
  CmdOpts
    <$> OA.strOption (OA.long "build-id" <> OA.short 'b' <> OA.metavar "BUILD_ID" <> OA.help "Buildkite build ID")
    <*> OA.switch (OA.long "show-all" <> OA.short 'a' <> OA.help "Whether to show all results or only the failed ones")

optParser :: OA.ParserInfo CmdOpts
optParser =
  OA.info
    (buildOpts <**> OA.helper)
    (OA.fullDesc <> OA.progDesc "Summarize tests in a buildkite build!" <> OA.header "show-build-summary")
