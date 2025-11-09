-- | This library provides vocabulary to generate encoders and decoders for URL
-- path segments, URL query strings and HTTP headers, from a single
-- bidirectional definition.
--
-- For example, consider this 'QueryCodec':
--
-- @
-- example :: 'QueryCodec' ('Int', ['String']) ('Int', ['String'])
-- example = '(,)' '<$>' 'query' \"id\" 'fst'
--                '<*>' 'queryMany' \"tag\" 'snd'
-- @
--
-- We could encode a @('Int', ['String'])@ pair into a
-- 'Network.HTTP.Types.QueryText' (from the @http-types@ library):
--
-- @
-- > 'queryEncode' example (4, [])
-- /[(\"id\", Just \"4\")]/
--
-- > 'queryEncode' example (5, ["hello", "world"])
-- /[(\"id\", Just \"5\"), (\"tag\", Just \"hello\"), (\"tag\", Just \"world\")]/
-- @
--
-- Or we could decode a 'Network.HTTP.Types.QueryText' (from the @http-types@
-- library) into a @('Int', ['String'])@ pair:
--
-- @
-- > 'queryDecode' example [(\"id\", Just \"4\")]
-- /Right ((4, []), [])/
--
-- > 'queryDecode' example [(\"id\", Just \"5\"), (\"tag\", Just \"hello\"), (\"tag\", Just \"world\")]
-- /Right ((5, [\"hello\", \"world\"]), [])/
-- @
--
-- Similarly, with 'HeaderCodec' and 'PathCodec'.
--
-- Usually, this is the only module you have to import from this library,
-- unless you are trying to extend its vocabulary somehow.
module Waypoint
   ( -- * Query
    QueryCodec

    -- ** Introduction
   , query
   , queryMaybe
   , querySome
   , queryMany
   , ToQueryValue (..)
   , FromQueryValue (..)

    -- ** Elimination
   , queryEncode
   , queryDecode
   , ErrQuery (..)

    -- * Header
   , HeaderCodec

    -- ** Introduction
   , header
   , headerMaybe
   , headerSome
   , headerMany
   , ToHeaderValue (..)
   , FromHeaderValue (..)

    -- ** Elimination
   , headerEncode
   , headerDecode
   , ErrHeader (..)

    -- * Path
   , PathCodec

    -- ** Introduction
   , path
   , pathLiteral
   , ToPathValue (..)
   , FromPathValue (..)

    -- ** Elimination
   , pathEncode
   , pathDecode
   , ErrPath (..)
   )
where

import Waypoint.Header
import Waypoint.Instances ()
import Waypoint.Path
import Waypoint.Query
