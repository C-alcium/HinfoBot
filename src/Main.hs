module Main where

import           NewsAPI
import           System.Environment

main :: IO ()
main = do
  newsapikey <- getEnv "NEWS_API_KEY"
  let mApiKey = Just newsapikey
  let mSearch = Just "bitcoin"
  let eParams = defaultEverythingParams { apiKey = mApiKey, q = mSearch}
  res <- runEverything eParams
  print res

