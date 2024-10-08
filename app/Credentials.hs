module Credentials where

import Control.Applicative ((<**>))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Entities (CmdOpts (CmdOpts))
import LoadEnv (loadEnvFromAbsolute)
import qualified Options.Applicative as OA
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))

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
    <*> OA.strOption (OA.long "org" <> OA.short 'o' <> OA.metavar "ORG_ID" <> OA.help "Organization (redpanda|vtools)" <> OA.value "redpanda")
    <*> OA.switch (OA.long "show-all" <> OA.short 'a' <> OA.help "Whether to show all results or only the failed ones")
    <*> OA.switch (OA.long "print-concise" <> OA.short 's' <> OA.help "Whether to show a condensed report")

optParser :: OA.ParserInfo CmdOpts
optParser =
  OA.info
    (buildOpts <**> OA.helper)
    (OA.fullDesc <> OA.progDesc "Summarize tests in a buildkite build!" <> OA.header "show-build-summary")

loadEnvVars :: IO ()
loadEnvVars = do
  paths <- envFiles
  mapM_ loadEnvFromAbsolute paths

envFiles :: IO [FilePath]
envFiles = do
  dirs <- sequence [getHomeDirectory, getCurrentDirectory]
  return $ map (</> ".ft-env") dirs
