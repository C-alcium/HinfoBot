{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           Control.Monad              (when)
import           Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import           Data.Either
import qualified Data.List.Split            as S
import           Data.Text                  as T
import           Data.Text.IO               as TIO
import           Data.Void
import Data.Typeable
import           Discord
import qualified Discord.Requests           as DR
import           Discord.Types
import           NewsAPI
import           System.Environment
import           Text.Megaparsec
import           Text.Megaparsec.Char

main :: IO ()
main = do
  discordApiToken <- getEnv "DISCORD_API_TOKEN"
  userFacingError <- runDiscord $ def
    { discordToken = T.pack discordApiToken
    , discordOnEvent = messageHandler
    }
  TIO.putStrLn userFacingError

--------------- Event Handling Code ---------------

messageHandler event = case event of
  MessageCreate m -> when (not (fromBot m) && isCommandMessage m) $ do
      let commandParseResult = applyCommandParse (messageText m)
      performCommandAction commandParseResult m
  _ -> pure ()
  where
    applyCommandParse m = runParser pCommand "" (T.unpack m)

--------------- Command Execution ---------------

performCommandAction (Left _)                 _ = pure ()
performCommandAction (Right (vCommand, args)) m =
  case vCommand of
    Help -> executeHelpCommand m
    Ping -> executePingCommand m
    Search -> executeSearchCommand m args 

-- Ping

executePingCommand :: Message -> ReaderT DiscordHandle IO ()
executePingCommand m = do
  _ <- restCall (DR.CreateMessage (messageChannel m) "Pong")
  pure ()

-- Help

executeHelpCommand :: Message -> ReaderT DiscordHandle IO ()
executeHelpCommand m = do
  _ <- restCall (DR.CreateMessageEmbed (messageChannel m) "" $
    def { createEmbedTitle       = "Help Menu :: HinfoBot"
        , createEmbedDescription = "The basic functionality of each command is described below" 
        , createEmbedFields      = commandDescriptions
        })
  pure ()


describeCommand :: ValidCommand -> EmbedField
describeCommand c =
  case c of
    Help -> EmbedField "Help" "Display the help menu" Nothing
    Ping -> EmbedField "Ping" "Pong!" Nothing
    Search -> EmbedField "Search" "Display a search result from the Newsapi.org" Nothing

commandDescriptions :: [EmbedField]
commandDescriptions = Prelude.map describeCommand commandList 

-- Search 

executeSearchCommand :: Message -> [String] -> ReaderT DiscordHandle IO ()
executeSearchCommand m args = do
  let glued = Prelude.unwords args
  let target = messageChannel m
  searchRes <- liftIO (search' glued)
  _         <- case searchRes of
                 Left  e -> sendMessageOrError target (T.pack ("API Call failed: " <> show e))
                 Right a -> sendMessageOrError target ((T.pack .show . totalResults) a)
  pure ()

-- Search the API 

search' query = do
 newsapikey <- getEnv "NEWS_API_KEY"
 runEverything defaultEverythingParams { apiKey = Just newsapikey, q = Just query }

--------------- Discord Utilities ---------------

sendMessageOrError :: ChannelId -> Text -> ReaderT DiscordHandle IO ()
sendMessageOrError target message = do
  result <- restCall (DR.CreateMessage target message)
  case result of
    (Left e)  -> do
      _ <- liftIO $ print e
      pure ()
    (Right v) -> pure ()


--------------- Command Predicates ---------------

commandPrefix :: Text
commandPrefix = "&"

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isCommandMessage :: Message -> Bool
isCommandMessage m = commandPrefix `isPrefixOf` messageText m

--------------- Parse Command ---------------

type Parser = Parsec Void String

data ValidCommand = Help 
                  | Ping 
                  | Search deriving (Eq, Show, Ord, Enum, Bounded)

class ExecutableCommand x where
  describe :: x -> String

instance ExecutableCommand ValidCommand where
  describe Help   = "Displays this help menu"
  describe Ping   = "Pong!"
  describe Search = "Search the news API for a given search term"

commandList :: [ValidCommand]
commandList = enumFrom minBound :: [ValidCommand]

validCommandP :: Parser ValidCommand
validCommandP = choice $ Prelude.map build' commandList 
  where
    build' a = a <$ string' (show a)

pCommand :: Parser (ValidCommand, [String])
pCommand = do
  _             <- char '&'
  parsedCommand <- validCommandP
  arguments     <- manyTill anySingle eof
  pure (parsedCommand, S.splitOn " " arguments)

