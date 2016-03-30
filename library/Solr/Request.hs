-- |
-- Request declaration DSL.
module Solr.Request where

import Solr.Prelude
import qualified Solr.HTTPResponseDecoder
import qualified Solr.HTTPRequestEncoder
import qualified Solr.Parameters
import qualified Solr.Request.Encoder
import qualified Solr.Request.Decoder
import qualified ByteString.TreeBuilder


-- |
-- Solr request specification.
data Request a b =
  Request
    !(ByteString -> Solr.HTTPRequestEncoder.Request a)
    !(Solr.HTTPResponseDecoder.Response b)

instance Functor (Request a) where
  {-# INLINE fmap #-}
  fmap f (Request requestEncoderProducer responseDecoder) =
    Request requestEncoderProducer (fmap f responseDecoder)

instance Profunctor Request where
  {-# INLINE dimap #-}
  dimap f1 f2 (Request requestEncoderProducer responseDecoder) =
    Request ((fmap . contramap) f1 requestEncoderProducer) (fmap f2 responseDecoder)


select :: Solr.Request.Encoder.Select a -> Solr.Request.Decoder.Select b -> Request a b
select selectEncoder selectDecoder =
  Request requestEncoderProducer responseDecoder
  where
    requestEncoderProducer baseURL =
      Solr.HTTPRequestEncoder.request_postJSON url jsonEncoder
      where
        url =
          baseURL <> "select/?wt=json"
        jsonEncoder =
          Solr.Request.Encoder.value_select selectEncoder
    responseDecoder =
      Solr.HTTPResponseDecoder.json $
      Solr.Request.Decoder.value_select $
      selectDecoder

count :: Request Text Int
count =
  select encoder decoder
  where
    encoder =
      Solr.Request.Encoder.select_query <>
      contramap (const 0) Solr.Request.Encoder.select_limit
    decoder =
      Solr.Request.Decoder.select_response $
      Solr.Request.Decoder.response_numFound

update :: Solr.Request.Encoder.Update a -> Request a ()
update updateEncoder =
  Request requestEncoderProducer responseDecoder
  where
    requestEncoderProducer baseURL =
      Solr.HTTPRequestEncoder.request_postJSON url jsonEncoder
      where
        url =
          baseURL <> "update/?commit=true"
        jsonEncoder =
          Solr.Request.Encoder.value_update updateEncoder
    responseDecoder =
      Solr.HTTPResponseDecoder.okay
