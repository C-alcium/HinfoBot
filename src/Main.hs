{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified CommandParsing             as CMDParse
import qualified Commands                   as CMD
import           Control.Monad              (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Either
import qualified Data.List.Split            as S
import           Data.Maybe                 (fromJust, isJust)
import           Data.Text                  as T
import           Data.Text.IO               as TIO
import           Data.Typeable
import           Discord
import qualified Discord.Requests           as DR
import           Discord.Types
import           NewsAPI
import           System.Environment         (getEnv)
import           Text.Megaparsec

type DiscordEffect = ReaderT DiscordHandle IO ()

main :: IO ()
main = do
  discordApiToken <- getEnv "DISCORD_API_TOKEN"
  userFacingError <- runDiscord $ def
    { discordToken = T.pack discordApiToken
    , discordOnEvent = messageHandler
    }
  TIO.putStrLn userFacingError

--------------- Event Handling Code ---------------

messageHandler :: Event -> DiscordEffect
messageHandler event = case event of
  MessageCreate m -> when (not (CMDParse.fromBot m) && CMDParse.isCommandMessage m) $ do
      let commandParseResult = CMDParse.parseCommand (messageText m)
      performCommandAction commandParseResult m
  _ -> pure ()

--------------- Command Execution ---------------

-- Hook for determining which command to execute

performCommandAction (Left _)                 _ = pure ()
performCommandAction (Right (vCommand, args)) m =
  case vCommand of
    CMD.Help   -> executeHelpCommand m
    CMD.Ping   -> executePingCommand m
    CMD.Search -> executeSearchCommand m args

-- Ping

executePingCommand :: Message -> DiscordEffect
executePingCommand m = do
  _ <- restCall (DR.CreateMessage (messageChannel m) "Pong")
  pure ()

-- Help

executeHelpCommand :: Message -> DiscordEffect
executeHelpCommand m = do
  _ <- restCall (DR.CreateMessageEmbed (messageChannel m) "" $
    def { createEmbedTitle       = "Help Menu :: HinfoBot"
        , createEmbedDescription = "The basic functionality of each command is described below"
        , createEmbedFields      = commandDescriptions
        })
  pure ()

commandDescriptions :: [EmbedField]
commandDescriptions = Prelude.map describeCommand CMD.commandList
  where
    describeCommand c = EmbedField (tShow c) (CMD.describe c) Nothing

-- Search

executeSearchCommand :: Message -> [String] -> DiscordEffect
executeSearchCommand m args = do
  let glued = Prelude.unwords args
  let target = messageChannel m
  searchRes <- liftIO (search' glued)
  _         <- case searchRes of
                 Left e -> sendMessageOrError target (T.pack ("API Call failed: " <> show e))
                 Right a -> do
                   _ <- restCall (DR.CreateMessageEmbed (messageChannel m) "" (buildSearchEmbed a (T.pack glued)))
                   pure ()

  pure ()

buildSearchEmbed :: NewsResult -> Text -> CreateEmbed
buildSearchEmbed result query =
  def { createEmbedTitle       = "Results for" <> query
      , createEmbedDescription = "found" <> tShow (totalResults result) <> " result(s)"
      , createEmbedFields      = produceResultFields (articles result)
      }

produceResultFields :: [ArticleResult] -> [EmbedField]
produceResultFields articles = Prelude.map produceField (validArticles articles)
  where
    validArticles as     = Prelude.filter (\ a -> isJust (title a) && isJust (url a)) as
    produceField article = EmbedField ((T.pack . fromJust) (title article)) ((T.pack . fromJust) (url article)) Nothing


-- Search the API


search' query = do
 newsapikey <- getEnv "NEWS_API_KEY"
 runEverything defaultEverythingParams { apiKey = Just newsapikey, q = Just query }

--------------- Utilities ---------------

sendMessageOrError :: ChannelId -> Text -> ReaderT DiscordHandle IO ()
sendMessageOrError target message = do
  result <- restCall (DR.CreateMessage target message)
  case result of
    (Left e)  -> do
      _ <- liftIO $ print e
      pure ()
    (Right v) -> pure ()

tShow :: (Show a) => a -> Text
tShow = T.pack . show

