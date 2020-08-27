module Args where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data ServerType = Player | Table
  deriving (Show, Read)

data Args = Args { port :: Int
                 , serverType :: ServerType
                 , playerId :: Maybe String
                 , playerName :: Maybe String
                 }

args :: Parser Args
args = Args
  <$> option auto (long "port" <> short 'p' <> help "The port to run on")
  <*> option auto (long "server-type" <> short 's' <> help "The type of the server (Player/Table)")
  <*> optional (strOption (long "player-id" <> short 'i' <> help "The player ID"))
  <*> optional (strOption (long "player-name" <> short 'n' <> help "The player name"))

opts :: ParserInfo Args
opts = info (args <**> helper) (fullDesc <> progDesc "Starts a game server for a player or the 'table'")

parseArgs :: IO Args
parseArgs = execParser opts
