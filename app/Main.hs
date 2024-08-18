{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<**>))
import Data.Aeson (FromJSON)
import Data.Aeson.Decoding (decode)
import Data.Aeson.Types (Value)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Utils (replace, strip)
import Entities
import Network.HTTP.Client (redirectCount)
import Network.HTTP.Client.Conduit (responseHeaders)
import Network.HTTP.Simple
import qualified Options.Applicative as OA
import System.Environment (getEnv)

rootUrl :: String
rootUrl = "https://api.buildkite.com/"

-- 52960
buildPath :: String -> ByteString
buildPath = pack . ("/v2/organizations/redpanda/pipelines/redpanda/builds/" <>)

makeGetRequest :: String -> IO Request
makeGetRequest = parseRequest . ("GET " <>)

doMain :: String -> Credentials -> IO ()
doMain build_id creds = do
  do
    r <- makeGetRequest rootUrl
    let req = setRequestPath (buildPath build_id) r

    putStrLn $ "getting build details for " <> build_id

    decoded <- getWithToken req token
    let failed = failedJobs <$> decoded
        artifacts = maybe [] artifactUrls failed

    putStrLn $ "Found " <> (show . length) artifacts <> " failed jobs"

    artRequests <- mapM makeGetRequest artifacts
    artResponses <- mapM (`getWithToken` token) artRequests
    let allArtResp = concatMap (fromMaybe []) artResponses
        htmlReports = filter (("vbuild/ducktape/results/final/report.html" ==) . artPath) allArtResp
    htmlReqs <- mapM (makeGetRequest . artDlUrl) htmlReports
    redirectedArtUrls <- mapM fetchRedirected htmlReqs

    mapM_ getArtSummary $ concat redirectedArtUrls
  where
    token = pack $ bkToken creds
    user = pack $ bkUser creds
    passwd = pack $ bkPasswd creds
    failedJobs response = filter (("failed" ==) . state) $ jobs response
    artifactUrls = map artifactsUrl
    getArtSummary u = printTestSummaries u user passwd
    fetchRedirected req =
      getRedirectTargets req (pack $ bkToken creds)

getWithToken :: (FromJSON a) => Request -> ByteString -> IO (Maybe a)
getWithToken request token = do
  let req = setRequestBearerAuth token request
  response <- httpLBS req
  pure $ decode $ getResponseBody response

getRedirectTargets :: Request -> ByteString -> IO [ByteString]
getRedirectTargets request token = do
  let reqWithAuth = setRequestBearerAuth token request
      noRedirectReq = reqWithAuth {redirectCount = 0}
  response <- httpJSON noRedirectReq :: IO (Response Value)
  let locationHeader = filter ((== "Location") . fst) $ responseHeaders response
      locs = map snd locationHeader
  return locs

extractTestsFromHtmlResponse :: BS.ByteString -> [TestRun]
extractTestsFromHtmlResponse responseData =
  let responseString = BS.unpack responseData
      rows = lines responseString
      failedTests = filter (" FAILED_TESTS=[" `isInfixOf`) rows
      stripped = map (replace "FAILED_TESTS=[" "" . strip) failedTests
      validJson = map (reverse . drop 3 . reverse) stripped
   in mapMaybe ((\c -> decode c :: Maybe TestRun) . BS.pack) validJson

printTestSummaries :: ByteString -> ByteString -> ByteString -> IO ()
printTestSummaries artUrl user passwd = do
  putStrLn $ "Fetching " <> unpack artUrl

  artRequest <- makeGetRequest $ unpack artUrl
  let reqWithAuth = setRequestBasicAuth user passwd artRequest
  response <- httpLBS reqWithAuth

  putStrLn $ "Fetched... " <> show (getResponseStatus response)
  mapM_ (putStrLn . testName) $ extractTestsFromHtmlResponse $ getResponseBody response

main :: IO ()
main = do
  cmdOpts <- OA.execParser optParser
  creds <- loadCreds
  doMain (buildId cmdOpts) creds

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

data Credentials = Credentials
  { bkToken :: String,
    bkUser :: String,
    bkPasswd :: String
  }

loadCreds :: IO Credentials
loadCreds =
  Credentials
    <$> getEnv "BK_TOK"
    <*> getEnv "BK_USR"
    <*> getEnv "BK_PASS"
