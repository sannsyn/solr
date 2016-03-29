module Solr.JSONDecoder
(
  value_select,
  Select,
  select_response,
  Response,
  response_numFound,
  response_docs,
  Docs,
  docs_doc,
  module JSONIncrementalDecoder,
)
where

import Solr.Prelude
import JSONIncrementalDecoder


value_select :: Select a -> Value a
value_select (Select spec) =
  objectLookup spec


newtype Select a =
  Select (ObjectLookup a)
  deriving (Functor, Applicative)

select_response :: Response a -> Select a
select_response (Response spec) =
  Select (atKey "response" (objectLookup spec))


-- |
-- JSON decoder in the context of the \"response\" schema.
newtype Response a =
  Response (ObjectLookup a)
  deriving (Functor, Applicative)

response_numFound :: Response Int
response_numFound =
  Response $
  atKey "numFound" numberAsInt

response_docs :: Docs a -> Response a
response_docs (Docs spec) =
  Response (atKey "docs" (arrayElements spec))


newtype Docs a =
  Docs (ArrayElements a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

docs_doc :: Value a -> Docs a
docs_doc spec =
  Docs (element spec)

