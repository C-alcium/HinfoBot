module Types 
  ( Parser
  , DiscordEffect
  )
  where


import           Data.Void
import           Text.Megaparsec
import Discord
import Discord.Types
import Discord.Requests
import           Control.Monad.Trans.Reader

type Parser = Parsec Void String

type DiscordEffect = ReaderT DiscordHandle IO ()
