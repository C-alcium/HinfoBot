{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Command.Execution
import           Data.Text          as T
import           Data.Text.IO       as TIO
import           Discord
import qualified Discord.Requests   as DR
import           Discord.Types
import           System.Environment

main :: IO ()
main = do
  discordApiToken <- getEnv "DISCORD_API_TOKEN"
  userFacingError <- runDiscord $ def
    { discordToken = T.pack discordApiToken
    , discordOnEvent = commandHandler
    }
  TIO.putStrLn userFacingError

