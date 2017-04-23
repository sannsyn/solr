{-|
Command line arguments API.
-}
module Solr.Args
(
  managedResource,
)
where

import Solr.Prelude
import OptparseApplicative.Simple.Parser
import qualified Attoparsec.Data as A
import qualified Data.Text.Encoding as B
import qualified Control.Monad.Managed.Safe as C
import qualified Solr.Managed as D
import qualified Solr.Effect.Effect as E


baseURL :: Maybe Text -> (Text -> Text) -> Parser ByteString
baseURL collectionNameMaybe updateName =
  argument (updateName "base-url") Nothing description def parser
  where
    description =
      Just "Base URL of Solr REST API"
    def =
      case collectionNameMaybe of
        Just collectionName ->
          Just (value, text)
          where
            text =
              "http://127.0.0.1:8983/solr/" <> collectionName
            value =
              B.encodeUtf8 text
        Nothing ->
          Nothing
    parser =
      A.utf8Bytes

{-|
Arguments parser, which produces the managed resource,
which can then be used to execute sessions.
-}
managedResource :: Maybe Text {-^ Possible default collection name-} -> (Text -> Text) {-^ Updater of the argument names-} -> Parser (C.Managed E.Resource)
managedResource collectionNameMaybe updateArgumentName =
  D.resource <$> baseURL collectionNameMaybe updateArgumentName
