-- |
-- Request declaration DSL.
module Solr.Request
(
  -- * Request
  Request(..),
  request_select,
  request_update,
  -- * Encoders
  Encoder_Select,
  encoder_select_query,
  encoder_select_filter,
  encoder_select_offset,
  encoder_select_limit,
  Encoder_Update,
  encoder_update_add,
  encoder_update_delete,
  Encoder_Add,
  encoder_add_doc,
  encoder_add_boost,
  encoder_add_overwrite,
  encoder_add_commitWithin,
  Encoder_Delete,
  encoder_delete_id,
  encoder_delete_query,
  encoder_delete_commitWithin,
  -- * Decoders
  Decoder_Select,
  decoder_select_response,
  Decoder_Response,
  decoder_response_numFound,
  decoder_response_docs,
  Decoder_Docs,
  decoder_docs_doc,
)
where

import Solr.Prelude
import qualified Solr.HTTPResponseDecoder
import qualified Solr.HTTPRequestEncoder
import qualified Solr.Parameters
import qualified JSONEncoder
import qualified JSONIncrementalDecoder


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


request_select :: Encoder_Select a -> Decoder_Select b -> Request a b
request_select selectEncoder selectDecoder =
  Request requestEncoderProducer responseDecoder
  where
    requestEncoderProducer baseURL =
      Solr.HTTPRequestEncoder.request_postJSON url jsonEncoder
      where
        url =
          baseURL <> "select/?wt=json"
        jsonEncoder =
          encoder_value_select selectEncoder
    responseDecoder =
      Solr.HTTPResponseDecoder.json $
      decoder_value_select $
      selectDecoder

request_update :: Encoder_Update a -> Request a ()
request_update updateEncoder =
  Request requestEncoderProducer responseDecoder
  where
    requestEncoderProducer baseURL =
      Solr.HTTPRequestEncoder.request_postJSON url jsonEncoder
      where
        url =
          baseURL <> "update/?commit=true"
        jsonEncoder =
          encoder_value_update updateEncoder
    responseDecoder =
      Solr.HTTPResponseDecoder.okay




encoder_value_select :: Encoder_Select a -> JSONEncoder.Value a
encoder_value_select (Encoder_Select spec) =
  JSONEncoder.object spec

encoder_value_update :: Encoder_Update a -> JSONEncoder.Value a
encoder_value_update (Encoder_Update spec) =
  JSONEncoder.object spec


-- q = query
-- fq = filter
-- fl = JSONEncoder.fields
-- start = offset
-- rows = limit
-- sort = sort
newtype Encoder_Select a =
  Encoder_Select (JSONEncoder.Object a)
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

encoder_select_query :: Encoder_Select Text
encoder_select_query =
  Encoder_Select (JSONEncoder.field "query" JSONEncoder.string)

encoder_select_filter :: Encoder_Select [Text]
encoder_select_filter =
  Encoder_Select (JSONEncoder.field "filter" (JSONEncoder.array (JSONEncoder.homo foldl' JSONEncoder.string)))

encoder_select_offset :: Encoder_Select Int
encoder_select_offset =
  Encoder_Select (JSONEncoder.field "offset" JSONEncoder.number_integral)

encoder_select_limit :: Encoder_Select Int
encoder_select_limit =
  Encoder_Select (JSONEncoder.field "limit" JSONEncoder.number_integral)



newtype Encoder_Update a =
  Encoder_Update (JSONEncoder.Object a)
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

encoder_update_add :: Encoder_Add a -> Encoder_Update a
encoder_update_add (Encoder_Add spec) =
  Encoder_Update (JSONEncoder.field "add" (JSONEncoder.object spec))

encoder_update_delete :: Encoder_Delete a -> Encoder_Update a
encoder_update_delete (Encoder_Delete spec) =
  Encoder_Update (JSONEncoder.field "delete" (JSONEncoder.object spec))


newtype Encoder_Add a =
  Encoder_Add (JSONEncoder.Object a)
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

encoder_add_doc :: JSONEncoder.Value a -> Encoder_Add a
encoder_add_doc spec =
  Encoder_Add (JSONEncoder.field "doc" spec)

encoder_add_boost :: Encoder_Add Double
encoder_add_boost =
  Encoder_Add (JSONEncoder.field "boost" (contramap realToFrac JSONEncoder.number_scientific))

encoder_add_overwrite :: Encoder_Add Bool
encoder_add_overwrite =
  Encoder_Add (JSONEncoder.field "overwrite" JSONEncoder.boolean)

-- |
-- Specifies the amount of milliseconds (10^-3).
encoder_add_commitWithin :: Encoder_Add Int
encoder_add_commitWithin =
  Encoder_Add (JSONEncoder.field "commitWithin" JSONEncoder.number_integral)


newtype Encoder_Delete a =
  Encoder_Delete (JSONEncoder.Object a)
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

encoder_delete_id :: Encoder_Delete Text
encoder_delete_id =
  Encoder_Delete (JSONEncoder.field "id" JSONEncoder.string)

encoder_delete_query :: Encoder_Delete Text
encoder_delete_query =
  Encoder_Delete (JSONEncoder.field "query" JSONEncoder.string)

-- |
-- Specifies the amount of milliseconds (10^-3).
encoder_delete_commitWithin :: Encoder_Delete Int
encoder_delete_commitWithin =
  Encoder_Delete (JSONEncoder.field "commitWithin" JSONEncoder.number_integral)


-- * Decoders
-------------------------

decoder_value_select :: Decoder_Select a -> JSONIncrementalDecoder.Value a
decoder_value_select (Decoder_Select spec) =
  JSONIncrementalDecoder.objectLookup spec


newtype Decoder_Select a =
  Decoder_Select (JSONIncrementalDecoder.ObjectLookup a)
  deriving (Functor, Applicative)

decoder_select_response :: Decoder_Response a -> Decoder_Select a
decoder_select_response (Decoder_Response spec) =
  Decoder_Select (JSONIncrementalDecoder.atKey "response" (JSONIncrementalDecoder.objectLookup spec))


-- |
-- JSON decoder in the context of the \"response\" schema.
newtype Decoder_Response a =
  Decoder_Response (JSONIncrementalDecoder.ObjectLookup a)
  deriving (Functor, Applicative)

decoder_response_numFound :: Decoder_Response Int
decoder_response_numFound =
  Decoder_Response $
  JSONIncrementalDecoder.atKey "numFound" JSONIncrementalDecoder.numberAsInt

decoder_response_docs :: Decoder_Docs a -> Decoder_Response a
decoder_response_docs (Decoder_Docs spec) =
  Decoder_Response (JSONIncrementalDecoder.atKey "docs" (JSONIncrementalDecoder.arrayElements spec))


newtype Decoder_Docs a =
  Decoder_Docs (JSONIncrementalDecoder.ArrayElements a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

decoder_docs_doc :: JSONIncrementalDecoder.Value a -> Decoder_Docs a
decoder_docs_doc spec =
  Decoder_Docs (JSONIncrementalDecoder.element spec)

