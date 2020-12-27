{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad      (unless)
import           Data.Either
import           Data.Text          as T
import           Data.Text.IO       as TIO
import           Discord
import qualified Discord.Requests   as DR
import           Discord.Types
import           NewsAPI
import           System.Environment

main :: IO ()
main = do
  newsapikey <- getEnv "NEWS_API_KEY"
  discordApiToken <- getEnv "DISCORD_API_TOKEN"
  let mApiKey = Just newsapikey
  userFacingError <- runDiscord $ def
    { discordToken = T.pack discordApiToken
    , discordOnEvent = messageHandler
    }
  TIO.putStrLn userFacingError

messageHandler event = case event of
  MessageCreate m -> unless (fromBot m) $ do
      _ <- restCall (DR.CreateMessage (messageChannel m) "Ping")
      pure ()
  _ -> pure ()


fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)
