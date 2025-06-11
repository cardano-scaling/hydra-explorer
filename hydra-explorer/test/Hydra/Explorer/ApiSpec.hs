{-# LANGUAGE OverloadedStrings #-}

-- | Tests that the hydra-explorer client API endpoints correspond to the
-- advertised openapi specification.
module Hydra.Explorer.ApiSpec where

import Hydra.Prelude hiding (get)
import Test.Hydra.Prelude

import Control.Lens (at, (^.), (^?!))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, nth)
import Data.Aeson.Types qualified as Aeson
import Data.OpenApi (
  Definitions,
  OpenApi (..),
  Schema,
  components,
  content,
  get,
  paths,
  responses,
  schema,
  schemas,
  validateJSON,
  _Inline,
 )
import Data.Yaml qualified as Yaml
import Hydra.Explorer (clientApi)
import Hydra.Explorer.ObservationApi (Observation, parseOldObservation)
import Hydra.Tx.Observe (HeadObservation)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Aeson.GenericSpecs (defaultSettings, roundtripAndGoldenADTSpecsWithSettings, roundtripAndGoldenSpecsWithSettings, sampleSize)
import Test.Hspec.Wai (MatchBody (..), ResponseMatcher (ResponseMatcher), shouldRespondWith, (<:>))
import Test.Hspec.Wai qualified as Wai
import Test.Hspec.Wai.Internal qualified as Wai

spec :: Spec
spec = apiServerSpec

apiServerSpec :: Spec
apiServerSpec = do
  describe "Observer API" $ do
    -- NOTE: Detect regressions in observer interface
    let settings = defaultSettings{sampleSize = 1}
    roundtripAndGoldenSpecsWithSettings settings $ Proxy @Observation
    roundtripAndGoldenADTSpecsWithSettings settings $ Proxy @(MinimumSized HeadObservation)

    -- NOTE: Test compatibility using old golden/OnChainTx/*.json files
    it "is backwards compatible" $ do
      let goldenDir = "golden/OnChainTx"
      files <- listDirectory goldenDir
      forM_ files $ \fn -> do
        bytes <- readFileLBS $ goldenDir </> fn
        let v = bytes ^?! key "samples" . nth 0
        case Aeson.parseEither parseOldObservation v of
          Left err -> failure $ "Failed to decode file " <> fn <> ": " <> err
          Right _ -> pure ()

  -- TODO: test conformance, but prop_validateJSONSchema only works for hydra-node schemas right now
  -- prop "conforms to observer-api.yaml" $
  --   prop_validateJSONSchema @Observation ("json-schemas" </> "observer-api.yaml") $
  --     key "paths"
  --       . key "/observations/{network}/{version}"
  --       . key "post"
  --       . key "requestBody"
  --       . key "content"
  --       . key "application/json"
  --       . key "schema"

  describe "Client API" $ do
    describe "GET /heads" $
      prop "matches schema" $ \(ReasonablySized explorerState) -> do
        Wai.withApplication (clientApi "static" $ pure explorerState) $ do
          let openApiSchema = "json-schemas" </> "client-api.yaml"
          openApi <- liftIO $ Yaml.decodeFileThrow @_ @OpenApi openApiSchema
          let componentSchemas = openApi ^?! components . schemas
          let maybeHeadsSchema = do
                path <- openApi ^. paths . at "/heads"
                endpoint <- path ^. get
                res <- endpoint ^. responses . at 200
                -- XXX: _Inline here assumes that no $ref is used within the
                -- openapi Operation
                jsonContent <- res ^. _Inline . content . at "application/json"
                s <- jsonContent ^. schema
                pure $ s ^. _Inline
          case maybeHeadsSchema of
            Nothing -> liftIO . failure $ "Failed to find schema for GET /heads endpoint"
            Just headsSchema -> do
              liftIO $ headsSchema `shouldNotBe` mempty
              Wai.get "heads"
                `shouldRespondWith` matchingJSONSchema componentSchemas headsSchema

    describe "GET /ticks" $
      prop "matches schema" $ \(ReasonablySized explorerState) -> do
        Wai.withApplication (clientApi "static" $ pure explorerState) $ do
          let openApiSchema = "json-schemas" </> "client-api.yaml"
          openApi <- liftIO $ Yaml.decodeFileThrow @_ @OpenApi openApiSchema
          let componentSchemas = openApi ^?! components . schemas
          let maybeTickSchema = do
                path <- openApi ^. paths . at "/ticks"
                endpoint <- path ^. get
                res <- endpoint ^. responses . at 200
                -- XXX: _Inline here assumes that no $ref is used within the
                -- openapi Operation
                jsonContent <- res ^. _Inline . content . at "application/json"
                s <- jsonContent ^. schema
                pure $ s ^. _Inline
          case maybeTickSchema of
            Nothing -> liftIO . failure $ "Failed to find schema for GET /ticks endpoint"
            Just ticksSchema -> do
              liftIO $ ticksSchema `shouldNotBe` mempty
              Wai.get "ticks"
                `shouldRespondWith` matchingJSONSchema componentSchemas ticksSchema

matchingJSONSchema :: Definitions Schema -> Schema -> ResponseMatcher
matchingJSONSchema definitions s =
  ResponseMatcher
    { matchStatus = 200
    , matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]
    , matchBody = MatchBody $ \_headers body ->
        case Aeson.eitherDecode body of
          Left err -> Just $ "Failed to decode body: " <> err
          Right value ->
            case validateJSON definitions s value of
              [] -> Nothing
              errs ->
                Just . toString . unlines $
                  map toText errs
    }
