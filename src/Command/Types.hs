module Command.Types
  ( Parser
  , DiscordEffect
  )
  where


import           Control.Monad.Trans.Reader
import           Data.Void
import           Discord
import           Discord.Requests
import           Discord.Types
import           Text.Megaparsec

type Parser = Parsec Void String

type DiscordEffect = ReaderT DiscordHandle IO ()
