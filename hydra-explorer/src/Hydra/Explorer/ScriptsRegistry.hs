{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Explorer.ScriptsRegistry where

import Hydra.Prelude

import Data.Aeson (eitherDecodeFileStrict', withObject, (.:))
import Hydra.SerialisedScriptRegistry (SerialisedScriptRegistry (..), cborHexToSerialisedScript, serialisedScriptFromText)
import Text.Show (Show (..))

instance FromJSON SerialisedScriptRegistry where
    parseJSON = withObject "ScriptsRegistry" $ \r -> do
        initialScript <- r .: "initialScript"
        commitScript <- r .: "commitScript"
        headScript :: Text <- r .: "headScript"
        depositScript :: Text <- r .: "depositScript"
        pure
            SerialisedScriptRegistry
                { initialScriptValidator = serialisedScriptFromText initialScript
                , commitScriptValidator = serialisedScriptFromText commitScript
                , headScriptValidator = cborHexToSerialisedScript (encodeUtf8 headScript)
                , depositScriptValidator = cborHexToSerialisedScript (encodeUtf8 depositScript)
                }

data HydraScriptRegistry = HydraScriptRegistry
    { version :: String
    , registry :: SerialisedScriptRegistry
    }
    deriving stock (Eq, Generic)

instance FromJSON HydraScriptRegistry where
    parseJSON = withObject "HydraScriptRegistry" $ \o -> do
        version <- o .: "version"
        registry <- o .: "registry"
        pure HydraScriptRegistry{version, registry}

type ScriptsRegistry = [HydraScriptRegistry]

scriptsRegistryFromFile :: FilePath -> IO ScriptsRegistry
scriptsRegistryFromFile fp = do
    putStrLn $ "Reading scripts registry from: " <> fp
    eitherDecodeFileStrict' fp >>= either (die . Hydra.Prelude.show) pure

summarize :: (Show a) => a -> String
summarize x =
    let s = Hydra.Prelude.show x
     in if length s <= 10
            then s
            else take 5 s ++ "..." ++ take 5 (reverse s)

summarizedRegistry :: SerialisedScriptRegistry -> String
summarizedRegistry SerialisedScriptRegistry{initialScriptValidator, commitScriptValidator, headScriptValidator, depositScriptValidator} =
    "SerialisedScriptRegistry { "
        ++ "initialScriptValidator = "
        ++ summarize initialScriptValidator
        ++ ", commitScriptValidator = "
        ++ summarize commitScriptValidator
        ++ ", headScriptValidator = "
        ++ summarize headScriptValidator
        ++ ", depositScriptValidator = "
        ++ summarize depositScriptValidator
        ++ " }"

instance Show HydraScriptRegistry where
    show HydraScriptRegistry{version, registry} =
        "HydraScriptRegistry { "
            ++ "version = "
            ++ Hydra.Prelude.show version
            ++ ", registry = "
            ++ summarizedRegistry registry
            ++ " }"
