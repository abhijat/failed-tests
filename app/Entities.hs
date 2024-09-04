{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Entities where

import Data.Aeson
import Data.Aeson.Types (Parser)

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

data TestRun = TestRun
  { testName :: String,
    testDescription :: String,
    testRunTime :: String,
    testSummary :: String,
    testResult :: String
  }
  deriving (Show)

instance FromJSON TestRun where
  parseJSON = withObject "TestRun" f
    where
      f obj =
        TestRun
          <$> obj .: "test_name"
          <*> obj .: "description"
          <*> obj .: "run_time"
          <*> obj .: "summary"
          <*> obj .: "test_result"

data CmdOpts = CmdOpts
  { buildId :: String,
    orgId :: String,
    showAllResults :: Bool
  }
  deriving (Show)
