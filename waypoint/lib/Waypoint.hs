{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Waypoint
   ( -- * Query
    Q.QueryCodec

    -- ** Introduction
   , Q.query
   , Q.queryMaybe
   , Q.querySome
   , Q.queryMany
   , Q.ToQueryValue (..)
   , Q.FromQueryValue (..)

    -- ** Elimination
   , Q.queryEncode
   , Q.queryDecode
   , Q.ErrQuery (..)

    -- * Header
   , H.HeaderCodec

    -- ** Introduction
   , H.header
   , H.headerMaybe
   , H.headerSome
   , H.headerMany
   , H.ToHeaderValue (..)
   , H.FromHeaderValue (..)

    -- ** Elimination
   , H.headerEncode
   , H.headerDecode
   , H.ErrHeader (..)

    -- * Path
   , P.PathCodec

    -- ** Introduction
   , P.path
   , P.pathLiteral
   , P.ToPathValue (..)
   , P.FromPathValue (..)

    -- ** Elimination
   , P.pathEncode
   , P.pathDecode
   , P.ErrPath (..)
   )
where

import Waypoint.Header qualified as H
import Waypoint.Instances ()
import Waypoint.Path qualified as P
import Waypoint.Query qualified as Q
