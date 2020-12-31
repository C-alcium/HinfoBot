{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Command.Parsing where

import           Data.Text
import           Discord.Types

import qualified Command.Definitions  as CMD
import           Command.Types
import           Data.List.Split      as S
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char


--------------- Command Predicates ---------------

commandPrefix :: Text
commandPrefix = "&"

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isCommandMessage :: Message -> Bool
isCommandMessage m = commandPrefix `isPrefixOf` messageText m

--------------- Command Parsing ---------------

parseCommand :: Text -> Either (ParseErrorBundle String Void) (CMD.ValidCommand, String)
parseCommand m = runParser pCommand "" (unpack m)

validCommandP :: Parser CMD.ValidCommand
validCommandP = choice $ Prelude.map build' CMD.commandList
  where
    build' a = a <$ string' (show a)

pCommand :: Parser (CMD.ValidCommand, String)
pCommand = do
  _             <- char '&'
  parsedCommand <- validCommandP
  arguments     <- manyTill anySingle eof
  pure (parsedCommand, arguments)

