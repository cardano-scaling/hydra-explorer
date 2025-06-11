module Hydra.Explorer.Env where

import Hydra.Prelude

import qualified Blammo.Logging.LogSettings.Env as Env
import Env (helpDoc, Parser, Error)
import Blammo.Logging.LogSettings (LogSettings)

envHelp :: IsString s => s
envHelp = fromString $ helpDoc 0 parser

parser :: Parser Error LogSettings
parser = Env.parser

parse :: IO LogSettings
parse = Env.parse
