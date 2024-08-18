{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader (ReaderT (runReaderT), asks, liftIO)
import Credentials (Credentials (bkToken), getPassword, getToken, getUser, loadCreds, optParser)
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

    decoded <- fetchWithToken req
    let failed = failedJobs <$> decoded
        artifacts = maybe [] artifactUrls failed

    putStrLn $ "Found " <> (show . length) artifacts <> " failed jobs"

    artRequests <- mapM makeGetRequest artifacts
    artResponses <- mapM fetchWithToken artRequests

    let allArtResp = concatMap (fromMaybe []) artResponses
        htmlReports = filter (("vbuild/ducktape/results/final/report.html" ==) . artPath) allArtResp
    htmlReqs <- mapM (makeGetRequest . artDlUrl) htmlReports
    redirectedArtUrls <- mapM fetchRedirected htmlReqs

    mapM_ getArtSummary $ concat redirectedArtUrls
  where
    fetchWithToken request = runReaderT (getWithToken request) creds
    failedJobs response = filter (("failed" ==) . state) $ jobs response
    artifactUrls = map artifactsUrl
    getArtSummary u = runReaderT (printTestSummaries u) creds
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

extractTestsFromHtmlResponse :: BS.ByteString -> [TestRun]
extractTestsFromHtmlResponse responseData =
  let responseString = BS.unpack responseData
      rows = lines responseString
      failedTests = filter (" FAILED_TESTS=[" `isInfixOf`) rows
      stripped = map (replace "FAILED_TESTS=[" "" . strip) failedTests
      validJson = map (reverse . drop 3 . reverse) stripped
   in mapMaybe ((\c -> decode c :: Maybe TestRun) . BS.pack) validJson

printTestSummaries :: ByteString -> ReaderT Credentials IO ()
printTestSummaries artUrl = do
  liftIO $ putStrLn $ "Fetching " <> unpack artUrl
  artRequest <- liftIO $ makeGetRequest $ unpack artUrl
  user <- asks getUser
  password <- asks getPassword
  let reqWithAuth = setRequestBasicAuth user password artRequest
  response <- httpLBS reqWithAuth

  liftIO $ putStrLn $ "Fetched... " <> show (getResponseStatus response)
  mapM_ (liftIO . putStrLn . testName) $ extractTestsFromHtmlResponse $ getResponseBody response

main :: IO ()
main = do
  cmdOpts <- OA.execParser optParser
  creds <- loadCreds
  doMain (buildId cmdOpts) creds
