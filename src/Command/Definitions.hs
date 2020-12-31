{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Command.Definitions
  ( ValidCommand (..)
  , describe
  , commandList
  )
  where

import           Data.Text
import           Discord.Types

data ValidCommand = Help
                  | Ping
                  | Search deriving (Eq, Show)

describe :: ValidCommand -> Text
describe Help    = "Displays this help menu"
describe Ping    = "Pong!"
describe Search  = "Search the news API for a given search term"

commandList :: [ValidCommand]
commandList = [ Help, Ping, Search ]

