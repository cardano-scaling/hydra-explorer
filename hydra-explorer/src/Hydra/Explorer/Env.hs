module Hydra.Explorer.Env where

import Hydra.Prelude

import Blammo.Logging.LogSettings (LogSettings)
import Blammo.Logging.LogSettings.Env qualified as Env
import Env (Error, Parser, helpDoc)

envHelp :: IsString s => s
envHelp = fromString $ helpDoc 0 parser

parser :: Parser Error LogSettings
parser = Env.parser

parse :: IO LogSettings
parse = Env.parse
