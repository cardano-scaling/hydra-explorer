module Hydra.Explorer.Options where

import Hydra.Prelude

import Network.Socket (PortNumber)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  showDefault,
  str,
  value,
 )

data Options = Options
  { clientPort :: PortNumber
  , observerPort :: PortNumber
  , staticFilePath :: FilePath
  }
  deriving stock (Show, Eq)

clientPortParser :: Parser PortNumber
clientPortParser =
  option
    auto
    ( long "client-port"
        <> value 9090
        <> showDefault
        <> metavar "PORT"
        <> help "Port to serve client REST API on."
    )

observerPortParser :: Parser PortNumber
observerPortParser =
  option
    auto
    ( long "observer-port"
        <> value 8080
        <> showDefault
        <> metavar "PORT"
        <> help "Port to serve observer REST API on."
    )

staticFilePathParser :: Parser FilePath
staticFilePathParser =
  option
    str
    ( long "static-path"
        <> value "static"
        <> showDefault
        <> metavar "PATH"
        <> help "Path to static files."
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> clientPortParser
    <*> observerPortParser
    <*> staticFilePathParser

hydraExplorerOptions :: ParserInfo Options
hydraExplorerOptions =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Explore Hydra heads."
        <> header "hydra-explorer"
    )
