{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader (ReaderT (runReaderT), asks, liftIO)
import Credentials (Credentials, getPassword, getToken, getUser, loadCreds, loadEnvVars, optParser)
import Data.Aeson (FromJSON, Value)
import Data.Aeson.Decoding (decode)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (fromJust, fromMaybe)
import Entities
import Network.HTTP.Client (redirectCount)
import Network.HTTP.Client.Conduit (responseHeaders)
import Network.HTTP.Simple
import qualified Options.Applicative as OA
import Text.Regex.Posix ((=~))

rootUrl :: String
rootUrl = "https://api.buildkite.com/"

reportPattern :: String
reportPattern = "vbuild/ducktape/results/.*/report.json"

buildPath :: String -> String -> ByteString
buildPath build_id org_id = pack $ orgUrl <> build_id
  where
    orgUrl = "/v2/organizations/redpanda/pipelines/" <> org_id <> "/builds/"

makeGetRequest :: String -> IO Request
makeGetRequest = parseRequest . ("GET " <>)

doMain :: String -> String -> Bool -> Credentials -> IO ()
doMain build_id org_id print_concise creds = do
  do
    r <- makeGetRequest rootUrl
    let req = setRequestPath (buildPath build_id org_id) r

    decoded <- fetchWithToken req
    let failed = failedJobs <$> decoded
        artifacts = maybe [] artifactUrls failed

    putStrLn $ "Found " <> (show . length) artifacts <> " failed jobs"
    artRequests <- mapM makeGetRequest artifacts
    artResponses <- mapM fetchWithToken artRequests

    let allArtResp = concatMap (fromMaybe []) artResponses
        jsonReports = filter (matchesReportPath . artPath) allArtResp
    jsonGets <- mapM (makeGetRequest . artDlUrl) jsonReports
    redirectedArtUrls <- mapM fetchRedirected jsonGets

    mapM_ getArtSummary $ concat redirectedArtUrls
  where
    matchesReportPath :: String -> Bool
    matchesReportPath p = p =~ reportPattern
    fetchWithToken request = runReaderT (getWithToken request) creds
    failedJobs response = filter (("failed" ==) . state) $ jobs response
    artifactUrls = map artifactsUrl
    getArtSummary u = runReaderT (fetchAndParseJsonResults u print_concise) creds
    fetchRedirected req = runReaderT (getRedirectTargets req) creds

getWithToken :: (FromJSON a) => Request -> ReaderT Credentials IO (Maybe a)
getWithToken request = do
  token <- asks getToken
  response <- httpLBS $ setRequestBearerAuth token request
  pure $ decode $ getResponseBody response

getRedirectTargets :: Request -> ReaderT Credentials IO [ByteString]
getRedirectTargets request = do
  token <- asks getToken
  let reqWithAuth = setRequestBearerAuth token request
      noRedirectReq = reqWithAuth {redirectCount = 0}
  response <- liftIO (httpJSON noRedirectReq :: IO (Response Value))
  pure $ map snd $ filter ((== "Location") . fst) $ responseHeaders response

fetchAndParseJsonResults :: ByteString -> Bool -> ReaderT Credentials IO ()
fetchAndParseJsonResults someUrl print_concise = do
  artRequest <- liftIO $ makeGetRequest $ unpack someUrl
  user <- asks getUser
  password <- asks getPassword

  let reqWithAuth = setRequestBasicAuth user password artRequest
  response <- httpLBS reqWithAuth
  let tests = getResponseBody response
      results = decode tests :: Maybe TestResults
      testRuns = testResults $ fromJust results
      failedTestRuns = filter ((== "FAIL") . testStatus) testRuns
   in liftIO $ mapM_ display failedTestRuns
  where
    display =
      if print_concise
        then putStrLn . testId
        else print

main :: IO ()
main = do
  loadEnvVars
  cmdOpts <- OA.execParser optParser
  creds <- loadCreds
  doMain (buildId cmdOpts) (orgId cmdOpts) (printConcise cmdOpts) creds
