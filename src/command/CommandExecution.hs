{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module CommandExecution
  ( messageHandler
  )
    where

import qualified CommandParsing             as CMDParse
import qualified Commands                   as CMD
import           Control.Monad              (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.List.Split            as S
import           Data.Maybe                 (fromJust, isJust)
import           Data.Text                  as T
import           Data.Text.IO               as TIO
import           Data.Time.Calendar
import           Data.Time.Clock
import           Discord
import qualified Discord.Requests           as DR
import           Discord.Types
import           NewsAPI
import           Servant.Client
import           System.Environment         (getEnv)
import           System.Log.Logger
import Types

loggerName :: String
loggerName = "command.execution"

--------------- Event Handling Code ---------------

messageHandler :: Event -> DiscordEffect
messageHandler event = case event of
  MessageCreate m -> when (not (CMDParse.fromBot m) && CMDParse.isCommandMessage m) $ do
      let commandParseResult = CMDParse.parseCommand (messageText m)
      let chan               = messageChannel m
      case commandParseResult of
        (Left _ )            -> sendMessageOrError chan "Command parsing failed."
        (Right (cmd, args) ) -> performCommandAction cmd m args
  _ -> pure ()

--------------- Command Execution ---------------

-- Hook for determining which command to execute

performCommandAction :: CMD.ValidCommand -> Message -> String -> DiscordEffect
performCommandAction cmd m args = do
  case cmd of
    CMD.Help   -> executeHelpCommand m
    CMD.Ping   -> executePingCommand m
    CMD.Search -> executeSearchCommand m args
  logExecution cmd

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

executeSearchCommand :: Message -> String -> DiscordEffect
executeSearchCommand m args = do
  let target = messageChannel m
  searchRes <- liftIO (searchNewsAPI args)
  _         <- case searchRes of
                 Left e -> sendMessageOrError target (T.pack ("API Call failed: " <> show e))
                 Right a -> do
                   _ <- restCall (DR.CreateMessageEmbed (messageChannel m) "" (buildSearchEmbed a (T.pack args)))
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

searchNewsAPI :: String -> IO (Either ClientError NewsResult)
searchNewsAPI query = do
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

logExecution :: (MonadIO m, Show a) => a -> m ()
logExecution c = do
  currentDate <- liftIO dateString
  liftIO (warningM loggerName ( "[" <> currentDate <> "]" <> "Executing " <> show c <> " command"))
    where
      dateString = getCurrentTime >>= (pure . show)

