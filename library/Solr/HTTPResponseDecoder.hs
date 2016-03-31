module Solr.HTTPResponseDecoder
(
  json,
  okay,
  module HTTPResponseDecoder,
)
where

import Solr.Prelude
import HTTPResponseDecoder
import qualified Data.ByteString.Lazy
import qualified JSONIncrementalDecoder


response :: Matcher ByteString () -> Matcher Int () -> Matcher Data.ByteString.Lazy.ByteString a -> Response a
response contentTypeMatcher statusCodeMatcher bodyMatcher =
  fmap snd $
  headAndBody head body
  where
    head =
      statusCode statusCodeMatcher *>
      headers (header "content-type" contentTypeMatcher)
    body =
      bodyLazyBytes bodyMatcher

statusIsOkay :: Matcher Int ()
statusIsOkay =
  equals 200

contentTypeIsJSON :: Matcher ByteString ()
contentTypeIsJSON =
  equals "application/json"

jsonBodyMatcher :: JSONIncrementalDecoder.Value a -> Matcher Data.ByteString.Lazy.ByteString a
jsonBodyMatcher spec =
  converts (JSONIncrementalDecoder.valueToLazyByteStringToEither spec)

json :: JSONIncrementalDecoder.Value a -> Response a
json spec =
  response whatever statusIsOkay (jsonBodyMatcher spec)
      
okay :: Response ()
okay =
  response whatever statusIsOkay whatever

