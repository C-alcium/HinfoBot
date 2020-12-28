{-# LANGUAGE OverloadedStrings #-}

module Commands where

import           Data.Text
import           Discord.Types

data ValidCommand = Help
                  | Ping
                  | Search deriving (Eq, Show, Ord, Enum, Bounded)

class ExecutableCommand x where
  describe :: x -> Text

instance ExecutableCommand ValidCommand where
  describe Help   = "Displays this help menu"
  describe Ping   = "Pong!"
  describe Search = "Search the news API for a given search term"

commandList :: [ValidCommand]
commandList = [minBound..]

