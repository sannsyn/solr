-- |
-- Request encoder DSL.
module Solr.Request.Encoder
(
  value_select,
  value_update,
  Select,
  select_query,
  select_filter,
  select_offset,
  select_limit,
  Update,
  update_add,
  update_delete,
  Add,
  add_doc,
  add_boost,
  add_overwrite,
  add_commitWithin,
  Delete,
  delete_id,
  delete_query,
  delete_commitWithin,
  module JSONEncoder,
)
where

import Solr.Prelude
import JSONEncoder


value_select :: Select a -> Value a
value_select (Select spec) =
  object spec

value_update :: Update a -> Value a
value_update (Update spec) =
  object spec


-- q = query
-- fq = filter
-- fl = fields
-- start = offset
-- rows = limit
-- sort = sort
newtype Select a =
  Select (Object a)
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

select_query :: Select Text
select_query =
  Select (field "query" string)

select_filter :: Select [Text]
select_filter =
  Select (field "filter" (array (homo foldl' string)))

select_offset :: Select Int
select_offset =
  Select (field "offset" number_integral)

select_limit :: Select Int
select_limit =
  Select (field "limit" number_integral)



newtype Update a =
  Update (Object a)
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

update_add :: Add a -> Update a
update_add (Add spec) =
  Update (field "add" (object spec))

update_delete :: Delete a -> Update a
update_delete (Delete spec) =
  Update (field "delete" (object spec))


newtype Add a =
  Add (Object a)
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

add_doc :: Value a -> Add a
add_doc spec =
  Add (field "doc" spec)

add_boost :: Add Double
add_boost =
  Add (field "boost" (contramap realToFrac number_scientific))

add_overwrite :: Add Bool
add_overwrite =
  Add (field "overwrite" boolean)

-- |
-- Specifies the amount of milliseconds (10^-3).
add_commitWithin :: Add Int
add_commitWithin =
  Add (field "commitWithin" number_integral)


newtype Delete a =
  Delete (Object a)
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

delete_id :: Delete Text
delete_id =
  Delete (field "id" string)

delete_query :: Delete Text
delete_query =
  Delete (field "query" string)

-- |
-- Specifies the amount of milliseconds (10^-3).
delete_commitWithin :: Delete Int
delete_commitWithin =
  Delete (field "commitWithin" number_integral)
