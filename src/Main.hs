{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Control.Monad              (when)
import           Control.Monad.Trans.Reader
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
  newsapikey <- getEnv "NEWS_API_KEY"
  discordApiToken <- getEnv "DISCORD_API_TOKEN"
  let mApiKey = Just newsapikey
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

--------------- Command Code ---------------

performCommandAction (Left _)                 _ = pure ()
performCommandAction (Right (vCommand, args)) m =
  case vCommand of
    Help -> executeHelpCommand m
    Ping -> executePingCommand m


--------------- Command Execution ---------------

executePingCommand :: Message -> ReaderT DiscordHandle IO ()
executePingCommand m = do
  _ <- restCall (DR.CreateMessage (messageChannel m) "Pong")
  pure ()


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

commandDescriptions :: [EmbedField]
commandDescriptions = Prelude.map describeCommand commandList 

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
                  | Ping deriving (Eq, Show, Ord, Enum, Bounded)

commandList :: [ValidCommand]
commandList = enumFrom minBound :: [ValidCommand]

pCommand :: Parser (ValidCommand, [String])
pCommand = do
  _             <- char '&'
  parsedCommand <- choice [ Help <$ string' "help"
                          , Ping <$ string' "ping" ]
  arguments     <- manyTill anySingle eof
  pure (parsedCommand, S.splitOn " " arguments)

