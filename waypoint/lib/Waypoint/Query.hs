{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Usually, you don't need to import this module unless
-- you are trying to reuse some of 'Query''s underlying primitives.
-- Just import "Waypoint" instead.
module Waypoint.Query
   ( -- * QueryValue
    QueryValue (..)
   , ToQueryValue (..)
   , FromQueryValue (..)

    -- * QueryValues
   , QueryValues
   , queryValuesOne
   , queryValuesMaybe
   , queryValuesMany
   , queryValuesSome

    -- * QueryF
   , QueryF (..)
   , queryFDecode
   , QueryFDecodeState
   , queryFEncode
   , queryFKeys

    -- * QueryCodec
   , QueryCodec (..)
   , queryDecode
   , queryEncode
   , queryKeys
   , query
   , queryMaybe
   , querySome
   , queryMany

    -- * ErrQuery
   , ErrQuery (..)
   )
where

import Control.Applicative
import Control.Applicative.Free.Fast
import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Monoid (Endo (..))
import Data.Profunctor
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Tuple
import Network.HTTP.Types qualified as H
import Text.Read (readMaybe)
import Witherable qualified as W
import Prelude

--------------------------------------------------------------------------------

-- | How to encode and decode a single querystring value
-- (e.g., the @v@ in @?k=v@).
data QueryValue i o = QueryValue
   { encode :: i -> T.Text
   , decode :: T.Text -> Maybe o
   }

instance W.Filterable (QueryValue i) where
   mapMaybe f (QueryValue i o) = QueryValue i (o >=> f)

instance Functor (QueryValue i) where
   fmap = rmap

instance Profunctor QueryValue where
   dimap f g (QueryValue i o) = QueryValue (i . f) (fmap g . o)

-- | 'QueryValue' based on 'ToQueryValue' and 'FromQueryValue'.
queryValue :: (ToQueryValue i, FromQueryValue o) => QueryValue i o
queryValue = QueryValue toQueryValue fromQueryValue

--------------------------------------------------------------------------------

-- | How to encode and decode potentially many querystring values
-- (e.g., both @v0@ and @v1@ in @?k=v0&k=v1@).
data QueryValues i o = QueryValues
   { encode :: i -> [T.Text]
   , decode :: [T.Text] -> Maybe o
   }

instance Functor (QueryValues i) where
   fmap = rmap

instance Profunctor QueryValues where
   dimap f g (QueryValues i o) = QueryValues (i . f) (fmap g . o)

instance W.Filterable (QueryValues i) where
   mapMaybe f (QueryValues i o) = QueryValues i (o >=> f)

queryValuesOne :: QueryValue i o -> QueryValues i o
queryValuesOne v = QueryValues (pure . v.encode) \case
   [t] -> v.decode t
   _ -> Nothing

queryValuesMaybe :: QueryValue i o -> QueryValues (Maybe i) (Maybe o)
queryValuesMaybe v = QueryValues (maybe [] (pure . v.encode)) \case
   [] -> Just Nothing
   [t] -> Just <$> v.decode t
   _ -> Nothing

queryValuesSome
   :: QueryValue i o -> QueryValues (NEL.NonEmpty i) (NEL.NonEmpty o)
queryValuesSome v = QueryValues (fmap v.encode . NEL.toList) \case
   t : ts -> traverse v.decode (t NEL.:| ts)
   _ -> Nothing

queryValuesMany :: QueryValue i o -> QueryValues [i] [o]
queryValuesMany v = QueryValues (fmap v.encode) (traverse v.decode)

--------------------------------------------------------------------------------

data QueryF i o = QueryF
   { key :: T.Text
   , values :: QueryValues i o
   }

instance Functor (QueryF i) where
   fmap = rmap

instance Profunctor QueryF where
   dimap f g (QueryF k v) = QueryF k (dimap f g v)

instance W.Filterable (QueryF i) where
   mapMaybe f (QueryF k v) = QueryF k (W.mapMaybe f v)

--------------------------------------------------------------------------------

-- | See 'queryDecodeF'.
data QueryFDecodeState = QueryFDecodeState
   { seen :: Set.Set T.Text
   -- ^ Previously seen keys.
   , input :: H.QueryText
   -- ^ Available raw input to decode.
   }
   deriving (Eq, Show)

-- | You probably don't need to use this unless you are building an
-- 'Applicative' on top of 'QueryF' yourself.
queryFDecode
   :: QueryF i o
   -> QueryFDecodeState
   -> Either ErrQuery (o, QueryFDecodeState)
queryFDecode q = \s ->
   if Set.member q.key s.seen
      then Left $ ErrQueryBadDecoder q.key
      else do
         let (xs1, xs2) = List.partition (\(k, _) -> k == q.key) s.input
             vs1 = fmap (maybe "" id . snd) xs1
         case q.values.decode vs1 of
            Just a -> Right (a, s{seen = Set.insert q.key s.seen, input = xs2})
            Nothing -> Left $ ErrQueryDecode q.key

-- | You probably don't need to use this unless you are building an
-- 'Applicative' on top of 'QueryF' yourself.
queryFEncode :: QueryF i o -> i -> Endo H.QueryText
queryFEncode q = \i ->
   foldMap
      (\v -> Endo ((q.key, if T.null v then Nothing else Just v) :))
      (q.values.encode i)
      <> Endo (filter (\(k, _) -> k /= q.key))

-- | You probably don't need to use this unless you are building an
-- 'Applicative' on top of 'QueryF' yourself.
queryFKeys :: QueryF i o -> Endo [T.Text]
queryFKeys q = Endo ((q.key :) . filter (/= q.key))

--------------------------------------------------------------------------------

data ErrQuery
   = -- | Error decoding the query value at the given key.
     ErrQueryDecode T.Text
   | -- | This error means the decoder itself is incorrect. It tried to
     -- decode values associated to the same query string key more than
     -- once.
     ErrQueryBadDecoder T.Text
   deriving stock (Eq, Show)
   deriving anyclass (Exception)

--------------------------------------------------------------------------------

newtype QueryCodec i o = QueryCodec (Ap (QueryF i) o)
   deriving newtype (Functor, Applicative)

instance Profunctor QueryCodec where
   rmap = fmap
   lmap f (QueryCodec a) = QueryCodec $ hoistAp (lmap f) a

-- | Consumes as much of the 'H.QueryText' as necessary in order to produce @o@.
-- Returns any leftovers in the original order.
queryDecode :: QueryCodec i o -> H.QueryText -> Either ErrQuery (o, H.QueryText)
queryDecode (QueryCodec af) = \xs0 -> do
   let s0 = QueryFDecodeState{seen = mempty, input = xs0}
   (o, s1) <- runStateT (runAp (StateT . queryFDecode) af) s0
   pure (o, s1.input)

queryEncode :: QueryCodec i o -> i -> H.QueryText
queryEncode (QueryCodec af) = flip appEndo [] . runAp_ queryFEncode af

-- | Keys in the order they are encoded and decoded.
queryKeys :: QueryCodec i o -> [T.Text]
queryKeys (QueryCodec af) = appEndo (runAp_ queryFKeys af) []

--------------------------------------------------------------------------------

-- | Encode and decode the one the query string value for a particular key.
--
-- E.g., @?__key__=__value__&...@.
query
   :: (ToQueryValue x, FromQueryValue o)
   => T.Text
   -- ^ Query string key.
   -> (i -> x)
   -- ^ @'query' k f == 'lmap' f ('query' k 'id')@, provided just for
   -- convenience.
   -> QueryCodec i o
query k f =
   QueryCodec $ liftAp $ QueryF k $ lmap f $ queryValuesOne queryValue

-- | Encode and decode zero or one query string values for a particular key.
--
-- E.g., @?__key__=__value__&...@, or just no mention of @__key__@ altogether.
queryMaybe
   :: (ToQueryValue x, FromQueryValue o)
   => T.Text
   -- ^ Query string key.
   -> (i -> Maybe x)
   -- ^ @'queryMaybe' k f == 'lmap' f ('queryMaybe' k 'id')@, provided just for
   -- convenience.
   -> QueryCodec i (Maybe o)
queryMaybe k f =
   QueryCodec $ liftAp $ QueryF k $ lmap f $ queryValuesMaybe queryValue

-- | Encode and decode zero or more query string values for a particular key.
--
-- E.g., @?__key__=__value0__&__key__=__value1__&...@.
queryMany
   :: (ToQueryValue x, FromQueryValue o)
   => T.Text
   -- ^ Query string key.
   -> (i -> [x])
   -- ^ @'queryMany' k f == 'lmap' f ('queryMany' k 'id')@, provided just for
   -- convenience.
   -> QueryCodec i [o]
queryMany k f =
   QueryCodec $ liftAp $ QueryF k $ lmap f $ queryValuesMany queryValue

-- | Encode and decode one or more query string values for a particular key.
--
-- E.g., @?__key__=__value0__&__key__=__value1__&...@.
querySome
   :: (ToQueryValue x, FromQueryValue o)
   => T.Text
   -- ^ Query string key.
   -> (i -> NEL.NonEmpty x)
   -- ^ @'querySome' k f == 'lmap' f ('querySome' k 'id')@, provided just for
   -- convenience.
   -> QueryCodec i (NEL.NonEmpty o)
querySome k f =
   QueryCodec $ liftAp $ QueryF k $ lmap f $ queryValuesSome queryValue

--------------------------------------------------------------------------------

class ToQueryValue i where
   -- | Encode @i@ into a query string value.
   toQueryValue :: i -> T.Text
   default toQueryValue :: (Show i) => i -> T.Text
   {-# INLINE toQueryValue #-}
   toQueryValue = T.pack . show

class FromQueryValue o where
   -- | Decode from one query string value into @o@.
   --
   -- E.g., from @?__key__=__value__&...@.
   fromQueryValue :: T.Text -> Maybe o
   default fromQueryValue :: (Read o) => T.Text -> Maybe o
   {-# INLINE fromQueryValue #-}
   fromQueryValue = readMaybe . T.unpack
