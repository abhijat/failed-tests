{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Entities where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.String.Utils (strip)

data Resp = Resp
  { url :: String,
    source :: String,
    jobs :: [Job]
  }
  deriving (Show)

instance FromJSON Resp where
  parseJSON :: Value -> Parser Resp
  parseJSON = withObject "Response" makeRespParser
    where
      makeRespParser obj = Resp <$> obj .: "url" <*> obj .: "source" <*> obj .: "jobs"

data Job = Job
  { state :: String,
    command :: String,
    artifactsUrl :: String
  }
  deriving (Show)

instance FromJSON Job where
  parseJSON :: Value -> Parser Job
  parseJSON = withObject "Job" makeJobParser
    where
      makeJobParser o = Job <$> o .: "state" <*> o .: "command" <*> o .: "artifacts_url"

data Artifact = Artifact
  { artId :: String,
    artJobId :: String,
    artDlUrl :: String,
    artState :: String,
    artPath :: String
  }
  deriving (Show)

instance FromJSON Artifact where
  parseJSON :: Value -> Parser Artifact
  parseJSON = withObject "Artifact" makeArtifactParser
    where
      makeArtifactParser o = Artifact <$> o .: "id" <*> o .: "job_id" <*> o .: "download_url" <*> o .: "state" <*> o .: "path"

newtype TestResults = TestResults {testResults :: [TestRun]} deriving (Show)

instance FromJSON TestResults where
  parseJSON :: Value -> Parser TestResults
  parseJSON = withObject "TestResults" go
    where
      go obj =
        TestResults <$> obj .: "results"

data TestRun = TestRun
  { testClass :: String,
    testFunction :: String,
    testModule :: String,
    testInjectedArgs :: Value,
    testResultsDir :: String,
    testStatus :: String,
    testSummary :: String,
    testId :: String
  }

instance FromJSON TestRun where
  parseJSON :: Value -> Parser TestRun
  parseJSON = withObject "TestRun" f
    where
      f obj =
        TestRun
          <$> obj .: "cls_name"
          <*> obj .: "function_name"
          <*> obj .: "module_name"
          <*> obj .: "injected_args"
          <*> obj .: "results_dir"
          <*> obj .: "test_status"
          <*> obj .: "summary"
          <*> obj .: "test_id"

instance Show TestRun where
  show t =
    unlines pieces
    where
      pieces =
        [ "=======================================================",
          testClass t,
          testModule t,
          testFunction t,
          BS.unpack (encodePretty $ testInjectedArgs t),
          strip (testSummary t),
          "======================================================="
        ]

data CmdOpts = CmdOpts
  { buildId :: String,
    orgId :: String,
    showAllResults :: Bool
  }
  deriving (Show)
