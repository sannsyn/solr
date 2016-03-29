-- |
-- For details see <http://wiki.apache.org/solr/CommonParametersParameters the Solr documentation>.
module Solr.Parameters
(
  builderEncoder_parameters,
  Parameters,
  parameters_select,
  Select,
  select_q,
  select_start,
  select_rows,
)
where

import Solr.Prelude
import qualified Network.URI.Encode
import qualified ByteString.TreeBuilder


builderEncoder_parameters :: Parameters a -> a -> ByteString.TreeBuilder.Builder
builderEncoder_parameters (Parameters (Op impl)) =
  \input ->
    fold $
    intersperse "&" $
    fmap pairMapping $
    impl input []
  where
    pairMapping (name, value) =
      if ByteString.TreeBuilder.length value == 0
        then name
        else name <> "=" <> value


newtype Parameters a =
  Parameters (Op ([(ByteString.TreeBuilder.Builder, ByteString.TreeBuilder.Builder)] -> [(ByteString.TreeBuilder.Builder, ByteString.TreeBuilder.Builder)]) a)
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

{-# INLINE parameters_parameter #-}
parameters_parameter :: Parameters (ByteString.TreeBuilder.Builder, ByteString.TreeBuilder.Builder)
parameters_parameter =
  Parameters $
  Op $
  \(name, value) ->
    (:) (name, value)

{-# INLINE parameters_select #-}
parameters_select :: Select a -> Parameters a
parameters_select (Select spec) =
  spec


-- |
-- Select parameters encoder.
newtype Select a =
  Select (Parameters a)
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

-- |
-- The actual search-query.
-- See <http://wiki.apache.org/solr/SolrQuerySyntax>.
{-# INLINE select_q #-}
select_q :: Select Text
select_q =
  Select $
  contramap (ByteString.TreeBuilder.byteString . Network.URI.Encode.encodeTextToBS) $
  contramap (\x -> ("q", x)) $
  parameters_parameter

{-# INLINE select_start #-}
select_start :: Select Int
select_start =
  Select $
  contramap (ByteString.TreeBuilder.asciiIntegral) $
  contramap (\x -> ("start", x)) $
  parameters_parameter

{-# INLINE select_rows #-}
select_rows :: Select Int
select_rows =
  Select $
  contramap (ByteString.TreeBuilder.asciiIntegral) $
  contramap (\x -> ("rows", x)) $
  parameters_parameter

