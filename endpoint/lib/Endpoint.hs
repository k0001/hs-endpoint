{-# LANGUAGE NoFieldSelectors #-}

module Endpoint {--}
   ( -- * Path
    Path
   , path
   , pathLiteral
   , mkPath
   , encodePath
   , decodePath
   , ErrPath (..)

    -- * Query
   , Query
   , query
   , queryMaybe
   , querySome
   , queryMany
   , mkQuery
   , encodeQuery
   , decodeQuery
   , ErrQuery (..)

    -- ** @http-types@ compat
   , queryToHttpTypes
   , queryFromHttpTypes

    -- ** Header
   , Header
   , header
   , headerMaybe
   , headerSome
   , headerMany
   , mkHeader
   , encodeHeader
   , decodeHeader
   , ErrHeader (..)

    -- ** @http-types@ compat
   , headerToHttpTypes
   , headerFromHttpTypes

    -- * Classes
   , ToPath (..)
   , FromPath (..)
   , ToQuery (..)
   , FromQuery (..)
   , ToHeader (..)
   , FromHeader (..)
   ) -- }
where

import Control.Applicative.Free.Fast
import Control.Exception (Exception)
import Control.Monad.Trans.State.Strict
import Data.ByteString qualified as B
import Data.Functor
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Profunctor
import Data.Set qualified as Set
import Data.Text qualified as T
import Network.HTTP.Types qualified as H
import Prelude

import Endpoint.Class

--------------------------------------------------------------------------------

-- | Compatible with the query string representation used by
-- "Network.HTTP.Types.URI".
queryFromHttpTypes :: H.QueryText -> Map.Map T.Text (NEL.NonEmpty T.Text)
queryFromHttpTypes = \qs ->
   Map.fromListWith (flip (<>)) do
      (k, yv) <- qs
      pure (k, pure (maybe "" id yv))

-- | Compatible with the query string representation used by
-- "Network.HTTP.Types.URI".
--
-- They keys will remain sorted by @'Ord' 'T.Text'@.
queryToHttpTypes :: Map.Map T.Text (NEL.NonEmpty T.Text) -> H.QueryText
queryToHttpTypes = \m -> do
   (k, vs) <- Map.toAscList m
   v <- NEL.toList vs
   pure (k, if T.null v then Nothing else Just v)

-- | Compatible with the header representation used by
-- "Network.HTTP.Types.URI".
headerFromHttpTypes
   :: [H.Header] -> Map.Map H.HeaderName (NEL.NonEmpty B.ByteString)
headerFromHttpTypes = Map.fromListWith (flip (<>)) . fmap (fmap pure)

-- | Compatible with the header representation used by
-- "Network.HTTP.Types.URI".
--
-- They keys will remain sorted by @'Ord' 'H.HeaderName'@.
headerToHttpTypes
   :: Map.Map H.HeaderName (NEL.NonEmpty B.ByteString) -> [H.Header]
headerToHttpTypes = \m -> do
   (k, vs) <- Map.toAscList m
   v <- NEL.toList vs
   pure (k, v)

--------------------------------------------------------------------------------

data PathF i o = PathF (i -> T.Text) (T.Text -> Either T.Text o)
   deriving stock (Functor)

instance Profunctor PathF where
   dimap fi fo (PathF gi go) = PathF (gi . fi) (fmap fo . go)

newtype Path i o = Path (Ap (PathF i) o)
   deriving newtype (Functor, Applicative)

instance Profunctor Path where
   rmap = fmap
   lmap f (Path a) = Path $ hoistAp (lmap f) a

encodePath :: Path i o -> i -> [T.Text]
encodePath (Path af) = runAp_ (\(PathF f _) -> pure . f) af

-- | Decodes the given path segments into @o@, if possible.
--
-- Parses from left to right, returning leftover segments, if any.
decodePath :: Path i o -> [T.Text] -> Either ErrPath ([T.Text], o)
decodePath = \(Path af) ts0 -> do
   (o, (_, ts1)) <- runStateT (runAp g af) (0, ts0)
   pure (ts1, o)
  where
   g :: PathF x a -> StateT (Word, [T.Text]) (Either ErrPath) a
   g (PathF _ f) = StateT \(!ix, !ts0) -> case ts0 of
      t : ts1 -> case f t of
         Right a -> Right (a, (ix + 1, ts1))
         Left e -> Left $ ErrPath ix e
      [] -> Left $ ErrPath ix "Missing"

data ErrPath
   = -- | Error parsing the path element at the given index.
     ErrPath Word T.Text
   deriving stock (Eq, Show)

instance Exception ErrPath

-- | Most general way to encode and decode a single path segment.
--
-- You should probably use one of 'path' or 'pathLiteral' instead of this lower
-- level function.
mkPath
   :: (i -> T.Text)
   -- ^ Encode @i@ into an URL path segment.
   -> (T.Text -> Either T.Text o)
   -- ^ Decode a path segment into @o@, if possible.
   -> Path i o
mkPath f g = Path $ liftAp $ PathF f g

-- | Like 'mkCodec', but relies on 'ToPath' and 'FromPath'.
path
   :: (ToPath x, FromPath o)
   => (i -> x)
   -- ^ @'path' f == 'lmap' f ('path' 'id')@, provided just for convenience.
   -> Path i o
path f = mkPath (toPath . f) (note "No parse" . fromPath)

-- | Literal segment in a URL path.
pathLiteral :: T.Text -> Path i ()
pathLiteral t = mkPath (const t) \x ->
   if x == t then Right () else Left "No parse"

--------------------------------------------------------------------------------

data QueryF i o = QueryF T.Text (i -> [T.Text]) ([T.Text] -> Either T.Text o)
   deriving stock (Functor)

instance Profunctor QueryF where
   dimap fi fo (QueryF k gi go) = QueryF k (gi . fi) (fmap fo . go)

newtype Query i o = Query (Ap (QueryF i) o)
   deriving newtype (Functor, Applicative)

instance Profunctor Query where
   rmap = fmap
   lmap f (Query a) = Query $ hoistAp (lmap f) a

encodeQuery :: Query i o -> i -> Map.Map T.Text (NEL.NonEmpty T.Text)
encodeQuery (Query af) =
   runAp_
      ( \(QueryF k f _) ->
         maybe mempty (Map.singleton k) . NEL.nonEmpty . f
      )
      af

-- | Decodes the given query string key to values 'Map.Map' into @o@, if
-- possible.
--
-- Returns leftover query string keys whose values weren't attempted to be
-- parsed, if any.
decodeQuery
   :: Query i o
   -> Map.Map T.Text (NEL.NonEmpty T.Text)
   -> Either ErrQuery (Set.Set T.Text, o)
decodeQuery = \(Query af) m0 -> do
   (o, m1) <- runStateT (runAp g af) m0
   pure (Map.keysSet m1, o)
  where
   g
      :: QueryF x a
      -> StateT (Map.Map T.Text (NEL.NonEmpty T.Text)) (Either ErrQuery) a
   g (QueryF k _ f) = StateT \ !m0 -> do
      let (yvs, m1) = Map.updateLookupWithKey (\_ _ -> Nothing) k m0
      case f (maybe [] NEL.toList yvs) of
         Right a -> Right (a, m1)
         Left e -> Left $ ErrQuery k e

data ErrQuery
   = -- | @'ErrQuery' /key/ /message/@.
     -- Error parsing the query value at the given key.
     ErrQuery T.Text T.Text
   deriving stock (Eq, Show)

instance Exception ErrQuery

-- | Most general way to encode and decode a single URL @query@ string key
-- and its values, if any.
--
-- You should probably use one of 'query', 'queryMaybe', 'querySome' or
-- 'queryMany' instead of this lower level function.
mkQuery
   :: T.Text
   -- ^ Query string key.
   -> (i -> [T.Text])
   -- ^ Encode @i@ into zero or more values.
   -- If the list is empty, the @key@ won't be mentioned
   -- in the query string at all.
   -> ([T.Text] -> Either T.Text o)
   -- ^ Decode from zero or more values into @o@.
   -- E.g., @?__key__=__value0__&__key__=__value1__&...@, or just no mention of
   -- @__key__@ altogether.
   -> Query i o
mkQuery k f g = Query $ liftAp $ QueryF k f g

-- | Encode and decode the one the query string value for a particular key.
--
-- E.g., @?__key__=__value__&...@.
query
   :: (ToQuery x, FromQuery o)
   => T.Text
   -- ^ Query string key.
   -> (i -> x)
   -- ^ @'query' k f == 'lmap' f ('query' k 'id')@, provided just for convenience.
   -> Query i o
query k f = mkQuery k (pure . toQuery . f) \case
   [] -> Left "Too few"
   [t] -> note "No parse" $ fromQuery t
   _ -> Left "Too many"

-- | Encode and decode zero or one query string values for a particular key.
--
-- E.g., @?__key__=__value__&...@, or just no mention of @__key__@ altogether.
queryMaybe
   :: (ToQuery x, FromQuery o)
   => T.Text
   -- ^ Query string key.
   -> (i -> Maybe x)
   -- ^ @'queryMaybe' k f == 'lmap' f ('queryMaybe' k 'id')@, provided just for convenience.
   -> Query i (Maybe o)
queryMaybe k f = mkQuery k (maybe [] (pure . toQuery) . f) \case
   [] -> Right Nothing
   [t] -> Just <$> note "No parse" (fromQuery t)
   _ -> Left "Too many"

-- | Encode and decode one or more query string values for a particular key.
--
-- E.g., @?__key__=__value0__&__key__=__value1__&...@.
querySome
   :: (ToQuery x, FromQuery o)
   => T.Text
   -- ^ Query string key.
   -> (i -> NEL.NonEmpty x)
   -- ^ @'querySome' k f == 'lmap' f ('querySome' k 'id')@, provided just for convenience.
   -> Query i (NEL.NonEmpty o)
querySome k f = mkQuery k (fmap toQuery . NEL.toList . f) \case
   t : ts -> note "No parse" $ traverse fromQuery (t NEL.:| ts)
   [] -> Left "Too few"

-- | Encode and decode zero or more query string values for a particular key.
--
-- E.g., @?__key__=__value0__&__key__=__value1__&...@.
queryMany
   :: (ToQuery x, FromQuery o)
   => T.Text
   -- ^ Query string key.
   -> (i -> [x])
   -- ^ @'queryMany' k f == 'lmap' f ('queryMany' k 'id')@, provided just for convenience.
   -> Query i [o]
queryMany k f =
   mkQuery k (fmap toQuery . f) (note "No parse" . traverse fromQuery)

--------------------------------------------------------------------------------

data HeaderF i o
   = HeaderF
      H.HeaderName
      (i -> [B.ByteString])
      ([B.ByteString] -> Either T.Text o)
   deriving stock (Functor)

instance Profunctor HeaderF where
   dimap fi fo (HeaderF k gi go) = HeaderF k (gi . fi) (fmap fo . go)

newtype Header i o = Header (Ap (HeaderF i) o)
   deriving newtype (Functor, Applicative)

instance Profunctor Header where
   rmap = fmap
   lmap f (Header a) = Header $ hoistAp (lmap f) a

encodeHeader
   :: Header i o -> i -> Map.Map H.HeaderName (NEL.NonEmpty B.ByteString)
encodeHeader (Header af) =
   runAp_
      ( \(HeaderF k f _) ->
         maybe mempty (Map.singleton k) . NEL.nonEmpty . f
      )
      af

-- | Decodes the given header name to values 'Map.Map' into @o@, if possible.
--
-- Returns leftover header names whose values weren't attempted to be
-- parsed, if any.
decodeHeader
   :: Header i o
   -> Map.Map H.HeaderName (NEL.NonEmpty B.ByteString)
   -> Either ErrHeader (Set.Set H.HeaderName, o)
decodeHeader = \(Header af) m0 -> do
   (o, m1) <- runStateT (runAp g af) m0
   pure (Map.keysSet m1, o)
  where
   g (HeaderF k _ f) = StateT \ !m0 -> do
      let (yvs, m1) = Map.updateLookupWithKey (\_ _ -> Nothing) k m0
      case f (maybe [] NEL.toList yvs) of
         Right a -> Right (a, m1)
         Left e -> Left $ ErrHeader k e

data ErrHeader
   = -- | @'ErrHeader' /key/ /message/@.
     -- Error parsing the header value with the given name.
     ErrHeader H.HeaderName T.Text
   deriving stock (Eq, Show)

instance Exception ErrHeader

-- | Most general way to encode and decode a single header name and all its
-- values, if any.
--
-- You should probably use one of 'header', 'headerMaybe', 'headerSome' or
-- 'headerMany' instead of this lower level function.
mkHeader
   :: H.HeaderName
   -- ^ Header name.
   -> (i -> [B.ByteString])
   -- ^ Encode @i@ into zero or more header @__name__: __value__@ lines.
   -- If the list is empty, no header line will be rendered at all.
   -> ([B.ByteString] -> Either T.Text o)
   -- ^ Decode from zero or more header values into @o@.  E.g., multiple header
   -- lines @__value__: __value0__@, @__value__: __value1__@, etc., or just no
   -- mention of @__value__@ altogether.
   -> Header i o
mkHeader k f g = Header $ liftAp $ HeaderF k f g

-- | Encode and decode the one the header value for a particular header name.
--
-- E.g., @__key__: __value__@.
header
   :: (ToHeader x, FromHeader o)
   => H.HeaderName
   -- ^ Header name.
   -> (i -> x)
   -- ^ @'header' k f == 'lmap' f ('header' k 'id')@, provided just for
   -- convenience.
   -> Header i o
header k f = mkHeader k (pure . toHeader . f) \case
   [] -> Left "Too few"
   [t] -> note "No parse" $ fromHeader t
   _ -> Left "Too many"

-- | Encode and decode zero or one header values for a particular header name.
--
-- E.g., @__value__: __value0__@, or just no mention of @__value__@ altogether.
headerMaybe
   :: (ToHeader x, FromHeader o)
   => H.HeaderName
   -- ^ Header name.
   -> (i -> Maybe x)
   -- ^ @'headerMaybe' k f == 'lmap' f ('headerMaybe' k 'id')@, provided just
   -- for convenience.
   -> Header i (Maybe o)
headerMaybe k f = mkHeader k (maybe [] (pure . toHeader) . f) \case
   [] -> Right Nothing
   [t] -> Just <$> note "No parse" (fromHeader t)
   _ -> Left "Too many"

-- | Encode and decode one or more header values for a particular header name.
--
-- E.g., multiple header lines @__name__: __value0__@, @__name__: __value1__@, etc.
headerSome
   :: (ToHeader x, FromHeader o)
   => H.HeaderName
   -- ^ Header name.
   -> (i -> NEL.NonEmpty x)
   -- ^ @'headerSome' k f == 'lmap' f ('headerSome' k 'id')@, provided just for
   -- convenience.
   -> Header i (NEL.NonEmpty o)
headerSome k f = mkHeader k (fmap toHeader . NEL.toList . f) \case
   t : ts -> note "No parse" $ traverse fromHeader (t NEL.:| ts)
   [] -> Left "Too few"

-- | Encode and decode zero or more header values for a particular header name.
--
-- E.g., multiple header lines @__name__: __value0__@, @__name__: __value1__@,
-- etc., or just no mention of @__name__@ altogether.
headerMany
   :: (ToHeader x, FromHeader o)
   => H.HeaderName
   -- ^ Header name.
   -> (i -> [x])
   -- ^ @'headerMany' k f == 'lmap' f ('headerMany' k 'id')@, provided just for
   -- convenience.
   -> Header i [o]
headerMany k f =
   mkHeader k (fmap toHeader . f) (note "No parse" . traverse fromHeader)

--------------------------------------------------------------------------------

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right
{-# INLINE note #-}
