{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE QuasiQuotes       #-}

module Main where

  import           MyLib (app)
  import           Control.Monad       (mzero)
  import           Data.Aeson
  import           Test.Hspec
  import           Test.Hspec.Wai
  import           Network.HTTP.Types (methodPost, hContentType)

  --import           Test.Hspec.Wai.JSON

  --instance of Message
  data ResponseMsg = ResponseMsg { name    :: String
                                 , message :: String
                                 } deriving (Eq, Show)
  instance FromJSON ResponseMsg where
   parseJSON (Object o) =
     ResponseMsg <$> o .: "name"
                 <*> o .: "message"
   parseJSON _ = mzero

  instance ToJSON ResponseMsg where
    -- this generates a Value
    toJSON (ResponseMsg n m) =
        object ["name" .= n, "message" .= m]

  --instance of respnseData
  data ResponseRespData = ResponseRespData { response :: String
                                           } deriving (Eq, Show)
  instance FromJSON ResponseRespData where
   parseJSON (Object o) =
     ResponseRespData <$> o .: "response"
   parseJSON _ = mzero

  instance ToJSON ResponseRespData where
    -- this generates a Value
    toJSON (ResponseRespData r) =
        object ["response" .= r]

  --instance of LastCommitDetails
  data ResonseLastCommit = ResonseLastCommit { commit_url        :: String
                                             , last_commit_hash_value  :: String
                                             } deriving (Eq, Show)
  instance FromJSON ResonseLastCommit where
   parseJSON (Object o) =
     ResonseLastCommit <$> o .: "commit_url"
                       <*> o .: "last_commit_hash_value"
   parseJSON _ = mzero

  instance ToJSON ResonseLastCommit where
    -- this generates a Value
    toJSON (ResonseLastCommit c l) =
        object ["commit_url" .= c, "last_commit_hash_value" .= l]

  --instance of metadata
  data ResonseMetadata = ResonseMetadata {  url               :: String
                                          , no_of_commits     :: String
                                          , last_commit_hash  :: String
                                         } deriving (Eq, Show)
  instance FromJSON ResonseMetadata where
    parseJSON (Object v) =
      ResonseMetadata <$> v .: "url"
                      <*> v .: "no_of_commits"
                      <*> v .: "last_commit_hash"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = mzero
  instance ToJSON ResonseMetadata where
    -- this generates a Value
    toJSON (ResonseMetadata u n l) =
        object ["url" .= u, "no_of_commits" .= n, "last_commit_hash" .= l]

  --instance of complexity
  data ResonseComplexity = ResonseComplexity {  repo_url          :: String
                                              , complexity        :: String
                                             } deriving (Eq, Show)
  instance FromJSON ResonseComplexity where
    parseJSON (Object v) =
      ResonseComplexity <$> v .: "repo_url"
                        <*> v .: "complexity"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = mzero
  instance ToJSON ResonseComplexity where
    -- this generates a Value
    toJSON (ResonseComplexity r c) =
        object ["repo_url" .= r, "complexity" .= c]


  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = with (return app) $ do
    --test case for sample storage message
    describe "POST /storeMessage true" $ do
      it "responds with storeMessage" $ do
        let postJson p = Test.Hspec.Wai.request methodPost p [
                  (hContentType, "application/json;charset=utf-8")
                ]
        (postJson "/storeMessage" $ encode $ toJSON $ ResponseMsg "ecky" "hello") `shouldRespondWith` "true" {matchHeaders = ["Content-Type" <:> "application/json"]}

    --test case for sample searchMessage, when search null message should return empty list
    describe "GET /searchMessage name is null" $ do
      it "responds with searchMessage" $ do
        get "/searchMessage?name=\"\"" `shouldRespondWith` "[]" {matchStatus = 200}

    --test case for sample searchMessage, when search not existed message should return empty list
    describe "GET /searchMessage name is null" $ do
      it "responds with searchMessage" $ do
        get "/searchMessage?name=notexit" `shouldRespondWith` "[]" {matchStatus = 200}

    --test case for sample search message with key ecky stored from previous case
    describe "GET /searchMessage?name=ecky" $ do
      it "responds with searchMessage" $ do
        get "/searchMessage?name=ecky" `shouldRespondWith` "[{\"name\":\"ecky\",\"message\":\"hello\"}]" {matchStatus = 200}

    --test case for storeMetaData API
    describe "POST /storeMetaData true" $ do
      it "responds with storeMetaData" $ do
        let postJson p = Test.Hspec.Wai.request methodPost p [
                  (hContentType, "application/json;charset=utf-8")
                ]
        (postJson "/storeMetaData" $ encode $ toJSON $ ResonseMetadata "https://github.com/sagarsachdeva/database-service" "15" "HESBDGADHBSD") `shouldRespondWith` "true" {matchHeaders = ["Content-Type" <:> "application/json"]}

    --test case for getLastCommitDetails, when get url is null should return empty
    describe "GET /getLastCommitDetails url is null" $ do
      it "responds with getLastCommitDetails" $ do
        get "/getLastCommitDetails?url=\"\"" `shouldRespondWith` "[]" {matchStatus = 200}

    --test case for getLastCommitDetails, when get url is not stored in dbs should return empty
    describe "GET /getLastCommitDetails url is null" $ do
      it "responds with getLastCommitDetails" $ do
        get "/getLastCommitDetails?url=notextes" `shouldRespondWith` "[]" {matchStatus = 200}


    --test case for getLastCommitDetails, when get url is the value stored from the last test case
    describe "GET /getLastCommitDetails url is www.storeMetaDataTest.com" $ do
      it "responds with getLastCommitDetails" $ do
        get "/getLastCommitDetails?url=https://github.com/sagarsachdeva/database-service" `shouldRespondWith` "[{\"last_commit_hash_value\":\"HESBDGADHBSD\",\"commit_url\":\"https://github.com/sagarsachdeva/database-service\"}]" {matchStatus = 200}

    --test case for storeMetaData API
    describe "POST /storeComplexity true" $ do
      it "responds with storeComplexity" $ do
        let postJson p = Test.Hspec.Wai.request methodPost p [
                  (hContentType, "application/json;charset=utf-8")
                ]
        (postJson "/storeComplexity" $ encode $ toJSON $ ResonseComplexity "https://github.com/sagarsachdeva/database-service" "10") `shouldRespondWith` "true" {matchHeaders = ["Content-Type" <:> "application/json"]}

    --test case for getRepoMetrics, when get url is null should return empty
    describe "GET /getRepoMetrics url is null" $ do
      it "responds with getRepoMetrics" $ do
        get "/getRepoMetrics?url=\"\"" `shouldRespondWith` "[]" {matchStatus = 200}

    --test case for getRepoMetrics, when get url is not stored in dbs should return empty
    describe "GET /getRepoMetrics url is null" $ do
      it "responds with getRepoMetrics" $ do
        get "/getRepoMetrics?url=notextes" `shouldRespondWith` "[]" {matchStatus = 200}

    --test case for getRepoMetrics, when get url is the value stored from the last test case
    describe "GET /getRepoMetrics url is https://github.com/sagarsachdeva/database-service" $ do
      it "responds with getRepoMetrics" $ do
        get "/getRepoMetrics?url=https://github.com/sagarsachdeva/database-service" `shouldRespondWith` "[{\"url\":\"https://github.com/sagarsachdeva/database-service\",\"last_commit_hash\":\"HESBDGADHBSD\",\"complexity\":\"10\",\"no_of_commits\":\"15\"}]" {matchStatus = 200}
