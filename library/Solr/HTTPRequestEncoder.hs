module Solr.HTTPRequestEncoder where

import Solr.Prelude
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Char8
import qualified Data.CaseInsensitive
import qualified JSONEncoder
import qualified Network.HTTP.Client
import qualified ByteString.TreeBuilder


encoder_request :: Request a -> a -> Network.HTTP.Client.Request
encoder_request (Request (Op impl)) input =
  appEndo (impl input) init
  where
    init =
      either (error . show) id $
      Network.HTTP.Client.parseUrl "http://www"


newtype Request a =
  Request (Op (Endo Network.HTTP.Client.Request) a)
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

request_body :: Body a -> Request a
request_body (Body impl) =
  Request $
  Op $
  \input ->
    Endo $
    \request ->
      request {
        Network.HTTP.Client.requestBody = (impl input)
      }

request_method :: ByteString -> Request a
request_method method =
  Request $
  Op $
  const $
  Endo $
  \request ->
    request {
      Network.HTTP.Client.method = method
    }

request_header :: ByteString -> ByteString -> Request a
request_header name value =
  Request $
  Op $
  const $
  Endo $
  \request ->
    request {
      Network.HTTP.Client.requestHeaders =
        (Data.CaseInsensitive.mk name, value) :
        Network.HTTP.Client.requestHeaders request
    }

request_url :: ByteString -> Request a
request_url url =
  Request $
  Op $
  const $
  Endo $
  case parsedRequestMaybe of
    Just parsedRequest ->
      \request ->
        request {
          Network.HTTP.Client.secure =
            Network.HTTP.Client.secure parsedRequest,
          Network.HTTP.Client.host =
            Network.HTTP.Client.host parsedRequest,
          Network.HTTP.Client.port =
            Network.HTTP.Client.port parsedRequest,
          Network.HTTP.Client.path =
            Network.HTTP.Client.path parsedRequest,
          Network.HTTP.Client.queryString =
            Network.HTTP.Client.queryString parsedRequest
        }
    Nothing -> id
  where
    parsedRequestMaybe =
      Network.HTTP.Client.parseUrl $
      Data.ByteString.Char8.unpack $
      url

{-|
Set the timeout in μs.
-}
request_timeout :: Int -> Request a
request_timeout timeout =
  Request $
  Op $
  const $
  Endo $
  \request ->
    request {
      Network.HTTP.Client.responseTimeout =
        Network.HTTP.Client.responseTimeoutMicro timeout
    }

newtype Body a =
  Body (a -> Network.HTTP.Client.RequestBody)

instance Contravariant Body where
  {-# INLINE contramap #-}
  contramap f (Body impl) =
    Body (\input -> impl (f input))

body_unit :: Body ()
body_unit =
  Body $
  const $
  Network.HTTP.Client.RequestBodyBS mempty

body_json :: JSONEncoder.Value a -> Body a
body_json spec =
  Body $
  \input ->
    Network.HTTP.Client.RequestBodyLBS $
    ByteString.TreeBuilder.toLazyByteString $
    JSONEncoder.run spec input


-- * Specific
-------------------------

request_postJSON :: ByteString -> JSONEncoder.Value a -> Request a
request_postJSON url jsonEncoder =
  request_url url <>
  request_method "POST" <>
  request_header "content-type" "application/json" <>
  request_timeout (30 * (10 ^ 6)) <>
  request_body (body_json jsonEncoder)

