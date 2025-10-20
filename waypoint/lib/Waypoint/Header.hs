{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Usually, you don't need to import this module unless
-- you are trying to reuse some of 'Header''s underlying primitives.
-- Just import "Waypoint" instead.
module Waypoint.Header
   ( -- * HeaderValue
    HeaderValue (..)
   , ToHeaderValue (..)
   , FromHeaderValue (..)

    -- * HeaderValues
   , HeaderValues
   , headerValuesOne
   , headerValuesMaybe
   , headerValuesMany
   , headerValuesSome

    -- * HeaderF
   , HeaderF (..)
   , headerFDecode
   , HeaderFDecodeState
   , headerFEncode
   , headerFNames

    -- * Header
   , HeaderCodec (..)
   , headerDecode
   , headerEncode
   , headerNames
   , header
   , headerMaybe
   , headerSome
   , headerMany

    -- * ErrHeader
   , ErrHeader (..)
   )
where

import Control.Applicative
import Control.Applicative.Free.Fast
import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.ByteString qualified as B
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Monoid (Endo (..))
import Data.Profunctor
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Tuple
import Network.HTTP.Types qualified as H
import Text.Read (readMaybe)
import Witherable qualified as W
import Prelude

--------------------------------------------------------------------------------

-- | How to encode and decode a single header value
-- (e.g., the @v@ in @k: v@).
data HeaderValue i o = HeaderValue
   { encode :: i -> B.ByteString
   , decode :: B.ByteString -> Maybe o
   }

instance W.Filterable (HeaderValue i) where
   mapMaybe f (HeaderValue i o) = HeaderValue i (o >=> f)

instance Functor (HeaderValue i) where
   fmap = rmap

instance Profunctor HeaderValue where
   dimap f g (HeaderValue i o) = HeaderValue (i . f) (fmap g . o)

-- | 'HeaderValue' based on 'ToHeaderValue' and 'FromHeaderValue'.
headerValue :: (ToHeaderValue i, FromHeaderValue o) => HeaderValue i o
headerValue = HeaderValue toHeaderValue fromHeaderValue

--------------------------------------------------------------------------------

-- | How to encode and decode potentially many header line values
-- (e.g., both @v0@ and @v1@ in multiple lines @k: v0@ and @k: v1@).
data HeaderValues i o = HeaderValues
   { encode :: i -> [B.ByteString]
   , decode :: [B.ByteString] -> Maybe o
   }

instance Functor (HeaderValues i) where
   fmap = rmap

instance Profunctor HeaderValues where
   dimap f g (HeaderValues i o) = HeaderValues (i . f) (fmap g . o)

instance W.Filterable (HeaderValues i) where
   mapMaybe f (HeaderValues i o) = HeaderValues i (o >=> f)

headerValuesOne :: HeaderValue i o -> HeaderValues i o
headerValuesOne v = HeaderValues (pure . v.encode) \case
   [t] -> v.decode t
   _ -> Nothing

headerValuesMaybe :: HeaderValue i o -> HeaderValues (Maybe i) (Maybe o)
headerValuesMaybe v = HeaderValues (maybe [] (pure . v.encode)) \case
   [] -> Just Nothing
   [t] -> Just <$> v.decode t
   _ -> Nothing

headerValuesSome
   :: HeaderValue i o -> HeaderValues (NEL.NonEmpty i) (NEL.NonEmpty o)
headerValuesSome v = HeaderValues (fmap v.encode . NEL.toList) \case
   t : ts -> traverse v.decode (t NEL.:| ts)
   _ -> Nothing

headerValuesMany :: HeaderValue i o -> HeaderValues [i] [o]
headerValuesMany v = HeaderValues (fmap v.encode) (traverse v.decode)

--------------------------------------------------------------------------------

-- | Like 'HeaderValues', but with the added @name@.
data HeaderF i o = HeaderF
   { name :: H.HeaderName
   , values :: HeaderValues i o
   }

instance Functor (HeaderF i) where
   fmap = rmap

instance Profunctor HeaderF where
   dimap f g (HeaderF k v) = HeaderF k (dimap f g v)

instance W.Filterable (HeaderF i) where
   mapMaybe f (HeaderF k v) = HeaderF k (W.mapMaybe f v)

--------------------------------------------------------------------------------

-- | See 'headerFDecode'.
data HeaderFDecodeState = HeaderFDecodeState
   { seen :: Set.Set H.HeaderName
   -- ^ Previously seen header names.
   , input :: [H.Header]
   -- ^ Available raw input to decode.
   }
   deriving (Eq, Show)

-- | You probably don't need to use this unless you are building an
-- 'Applicative' on top of 'HeaderF' yourself.
headerFDecode
   :: HeaderF i o
   -> HeaderFDecodeState
   -> Either ErrHeader (o, HeaderFDecodeState)
headerFDecode q = \s ->
   if Set.member q.name s.seen
      then Left $ ErrHeaderBadDecoder q.name
      else do
         let (xs1, xs2) = List.partition (\(k, _) -> k == q.name) s.input
         case q.values.decode (fmap snd xs1) of
            Just a -> Right (a, s{seen = Set.insert q.name s.seen, input = xs2})
            Nothing -> Left $ ErrHeaderDecode q.name

-- | You probably don't need to use this unless you are building an
-- 'Applicative' on top of 'HeaderF' yourself.
headerFEncode :: HeaderF i o -> i -> Endo [H.Header]
headerFEncode q = \i ->
   foldMap
      (\v -> Endo ((q.name, v) :))
      (q.values.encode i)
      <> Endo (filter (\(k, _) -> k /= q.name))

-- | You probably don't need to use this unless you are building an
-- 'Applicative' on top of 'HeaderF' yourself.
headerFNames :: HeaderF i o -> Endo [H.HeaderName]
headerFNames q = Endo ((q.name :) . filter (/= q.name))

--------------------------------------------------------------------------------

data ErrHeader
   = -- | Error decoding the header value at the given name.
     ErrHeaderDecode H.HeaderName
   | -- | This error means the decoder itself is incorrect. It tried to
     -- decode values associated to the same header name more than once.
     ErrHeaderBadDecoder H.HeaderName
   deriving stock (Eq, Show)
   deriving anyclass (Exception)

--------------------------------------------------------------------------------

newtype HeaderCodec i o = HeaderCodec (Ap (HeaderF i) o)
   deriving newtype (Functor, Applicative)

instance Profunctor HeaderCodec where
   rmap = fmap
   lmap f (HeaderCodec a) = HeaderCodec $ hoistAp (lmap f) a

-- | Consumes as much of the 'H.Header's as necessary in order to produce @o@.
-- Returns any leftovers in the original order.
headerDecode :: HeaderCodec i o -> [H.Header] -> Either ErrHeader (o, [H.Header])
headerDecode (HeaderCodec af) = \xs0 -> do
   let s0 = HeaderFDecodeState{seen = mempty, input = xs0}
   (o, s1) <- runStateT (runAp (StateT . headerFDecode) af) s0
   pure (o, s1.input)

headerEncode :: HeaderCodec i o -> i -> [H.Header]
headerEncode (HeaderCodec af) = flip appEndo [] . runAp_ headerFEncode af

-- | Names in the order they are encoded and decoded.
headerNames :: HeaderCodec i o -> [H.HeaderName]
headerNames (HeaderCodec af) = appEndo (runAp_ headerFNames af) []

--------------------------------------------------------------------------------

-- | Encode and decode the one the header value for a particular name.
--
-- E.g., @__name__: __value__@.
header
   :: (ToHeaderValue x, FromHeaderValue o)
   => H.HeaderName
   -- ^ Header name.
   -> (i -> x)
   -- ^ @'header' k f == 'lmap' f ('header' k 'id')@, provided just for
   -- convenience.
   -> HeaderCodec i o
header k f =
   HeaderCodec $ liftAp $ HeaderF k $ lmap f $ headerValuesOne headerValue

-- | Encode and decode zero or one header string values for a particular name.
--
-- E.g., @__name__: __value__@, or just no mention of @__name__@ altogether.
headerMaybe
   :: (ToHeaderValue x, FromHeaderValue o)
   => H.HeaderName
   -- ^ Header name.
   -> (i -> Maybe x)
   -- ^ @'headerMaybe' k f == 'lmap' f ('headerMaybe' k 'id')@, provided just for
   -- convenience.
   -> HeaderCodec i (Maybe o)
headerMaybe k f =
   HeaderCodec $ liftAp $ HeaderF k $ lmap f $ headerValuesMaybe headerValue

-- | Encode and decode zero or more header line values for a particular name.
--
-- E.g., @__name__: __value0__@ and @__name__: __value1__@, or just no mention
-- of @__name__@ at all.
headerMany
   :: (ToHeaderValue x, FromHeaderValue o)
   => H.HeaderName
   -- ^ Header name.
   -> (i -> [x])
   -- ^ @'headerMany' k f == 'lmap' f ('headerMany' k 'id')@, provided just for
   -- convenience.
   -> HeaderCodec i [o]
headerMany k f =
   HeaderCodec $ liftAp $ HeaderF k $ lmap f $ headerValuesMany headerValue

-- | Encode and decode one or more header string values for a particular name.
--
-- E.g., @__name__: __value0__@ and @__name__: __value1__@.
headerSome
   :: (ToHeaderValue x, FromHeaderValue o)
   => H.HeaderName
   -- ^ Header name.
   -> (i -> NEL.NonEmpty x)
   -- ^ @'headerSome' k f == 'lmap' f ('headerSome' k 'id')@, provided just for
   -- convenience.
   -> HeaderCodec i (NEL.NonEmpty o)
headerSome k f =
   HeaderCodec $ liftAp $ HeaderF k $ lmap f $ headerValuesSome headerValue

--------------------------------------------------------------------------------

class ToHeaderValue i where
   -- | Render @i@ into a single literal header value.
   --
   -- __WARNING__: The 'B.ByteString' is literally as it is on the header value.
   toHeaderValue :: i -> B.ByteString

class FromHeaderValue o where
   -- | Parse a single literal header value into an @o@.
   --
   -- __WARNING__: The 'B.ByteString' is literally as it is on the header value.
   fromHeaderValue :: B.ByteString -> Maybe o
   default fromHeaderValue :: (Read o) => B.ByteString -> Maybe o
   fromHeaderValue = T.decodeASCII' >=> \t -> readMaybe (T.unpack t)
