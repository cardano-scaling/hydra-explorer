{-# LANGUAGE OverloadedStrings #-}

module Hydra.ExplorerSpec where

import Hydra.Prelude hiding (get)
import Test.Hydra.Prelude

import Control.Lens (at, (^.), (^?!))
import Data.Aeson qualified as Aeson
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
import Hydra.Explorer (httpApp)
import Hydra.Logging (nullTracer)
import System.FilePath ((</>))
import Test.Hspec.Wai (MatchBody (..), ResponseMatcher (ResponseMatcher), shouldRespondWith, (<:>))
import Test.Hspec.Wai qualified as Wai
import Test.QuickCheck (generate)

spec :: Spec
spec = apiServerSpec

apiServerSpec :: Spec
apiServerSpec = do
    Wai.with (return webServer)
        $ describe "API should respond correctly"
        $ do
            describe "GET /heads/{hydraVersion}"
                $ it "matches schema"
                $ do
                    let openApiSchema = "json-schemas" </> "hydra-explorer-api.yaml"
                    openApi <- liftIO $ Yaml.decodeFileThrow @_ @OpenApi openApiSchema
                    let componentSchemas = openApi ^?! components . schemas
                    let maybeHeadsSchema = do
                            path <- openApi ^. paths . at "/heads/{hydraVersion}"
                            endpoint <- path ^. get
                            res <- endpoint ^. responses . at 200
                            -- XXX: _Inline here assumes that no $ref is used within the
                            -- openapi Operation
                            jsonContent <- res ^. _Inline . content . at "application/json"
                            s <- jsonContent ^. schema
                            pure $ s ^. _Inline
                    case maybeHeadsSchema of
                        Nothing -> liftIO . failure $ "Failed to find schema for GET /heads/1 endpoint"
                        Just headsSchema -> do
                            liftIO $ headsSchema `shouldNotBe` mempty
                            Wai.get "heads/1"
                                `shouldRespondWith` matchingJSONSchema componentSchemas headsSchema

            describe "GET /tick/{hydraVersion}"
                $ it "matches schema"
                $ do
                    let openApiSchema = "json-schemas" </> "hydra-explorer-api.yaml"
                    openApi <- liftIO $ Yaml.decodeFileThrow @_ @OpenApi openApiSchema
                    let componentSchemas = openApi ^?! components . schemas
                    let maybeTickSchema = do
                            path <- openApi ^. paths . at "/tick/{hydraVersion}"
                            endpoint <- path ^. get
                            res <- endpoint ^. responses . at 200
                            -- XXX: _Inline here assumes that no $ref is used within the
                            -- openapi Operation
                            jsonContent <- res ^. _Inline . content . at "application/json"
                            s <- jsonContent ^. schema
                            pure $ s ^. _Inline
                    case maybeTickSchema of
                        Nothing -> liftIO . failure $ "Failed to find schema for GET /tick/1 endpoint"
                        Just tickSchema -> do
                            liftIO $ tickSchema `shouldNotBe` mempty
                            Wai.get "tick/1"
                                `shouldRespondWith` matchingJSONSchema componentSchemas tickSchema
  where
    webServer = httpApp nullTracer "static" [("1", getRandomExplorerState)]

    getRandomExplorerState = generate arbitrary

-- paramHeadId =
--     mempty
--         & (name .~ "headId")
--         & (in_ .~ ParamPath)
--         & (required .~ True)
--         & (description ?~ "The ID of the head")
--         & ((schema . type_) ?~ OpenApiString)

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
                            Just
                                . toString
                                . unlines
                                $ map toText errs
                                <> [ "Expected schema: " <> show s
                                   , "Actual value: " <> show value
                                   ]
        }
