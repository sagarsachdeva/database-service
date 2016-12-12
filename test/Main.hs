{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

  import           MyLib (app)
  import           Test.Hspec
  import           Test.Hspec.Wai
  import           Test.Hspec.Wai.JSON


  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = with (return app) $ do
    --test case for sample storage message
    describe "POST /storeMessage true" $ do
      it "responds with storeMessage" $ do
        post "/storeMessage" [json|{name:"ecky", message:"mess"}|] `shouldRespondWith` "true%" {matchHeaders = ["Content-Type" <:> "application/json"]}

    --test case for sample searchMessage, when search null message should return empty list
    describe "GET /searchMessage name is null" $ do
      it "responds with searchMessage" $ do
        get "/searchMessage?name=\"\"" `shouldRespondWith` "[]" {matchStatus = 200}
    --test case for sample search message with key ecky stored from previous case
    describe "GET /searchMessage?name=ecky" $ do
      it "responds with searchMessage" $ do
        get "/searchMessage?name=ecky" `shouldRespondWith` "[{\"name\":\"ecky\",\"message\":\"mess\"}]" {matchStatus = 200}

    --test case for storeMetaData API
    describe "POST /storeMetaData true" $ do
      it "responds with storeMetaData null" $ do
        post "/storeMetaData" [json|{url:"www.storeMetaDataTest.com", no_of_commits:"15",last_commit_hash:"HESBDGADHBSD" }|] `shouldRespondWith` "true%" {matchHeaders = ["Content-Type" <:> "application/json"]}

    --test case for getLastCommitDetails, when get url is null should return empty
    describe "GET /getLastCommitDetails url is null" $ do
      it "responds with getLastCommitDetails" $ do
        get "/getLastCommitDetails?url=\"\"" `shouldRespondWith` "[]" {matchStatus = 200}

    --test case for getLastCommitDetails, when get url is the value stored from the last test case
    describe "GET /getLastCommitDetails url is www.storeMetaDataTest.com" $ do
      it "responds with getLastCommitDetails" $ do
        get "/getLastCommitDetails?url=www.storeMetaDataTest.com" `shouldRespondWith` "[{\"url\":\"www.storeMetaDataTest.com\",\"no_of_commits\":\"15\",\"last_commit_hash\":\"HESBDGADHBSD\"}]" {matchStatus = 200}
