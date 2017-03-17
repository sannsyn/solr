{-|
Command line arguments API.
-}
module Solr.Args
where

import Solr.Prelude
import OptparseApplicative.Simple.Parser
import qualified Attoparsec.Data.Explicit as A
import qualified Data.Text.Encoding as B


baseURL :: (Text -> Text) -> Maybe Text -> Parser ByteString
baseURL updateName collectionNameMaybe =
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
