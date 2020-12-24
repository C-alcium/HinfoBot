module Main where

import qualified NewsAPI            as NAPI

import           System.Environment

main :: IO ()
main = do
  newsapikey <- getEnv "NEWS_API_KEY"
  res <- NAPI.runEverything newsapikey "bitcoin"
  case res of
    (Left e)   -> print e
    (Right r ) -> print r
