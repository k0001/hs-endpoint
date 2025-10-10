{-# LANGUAGE NoFieldSelectors #-}

module Route {--}
   ( -- * Route
    Route (..)

    -- * Codec
   , Codec

    -- ** Path
   , path
   , pathLiteral
   , pathCodec

    -- ** Query
   , query
   , queryMaybe
   , querySome
   , queryMany
   , queryCodec

    -- * Decoder
   , Decoder
   , decoder
   , decode

    -- * Encoder
   , Encoder
   , encoder
   , encode

    -- * Classes
   , ToPath (..)
   , FromPath (..)
   , ToQuery (..)
   , FromQuery (..)
   ) -- }
where

import Control.Applicative.Free.Fast
import Control.Exception (Exception)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Functor
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Profunctor
import Data.Text qualified as T
import GHC.Generics (Generic)
import Prelude

import Route.Class

--------------------------------------------------------------------------------

data Route = Route
   { path :: [T.Text]
   -- ^ The path segments in an URL.
   , query :: Map.Map T.Text (NEL.NonEmpty T.Text)
   -- ^ The query string key and values in an URL.
   }
   deriving stock (Eq, Ord, Show, Generic)

-- | * @path@s are concatenated.
--
-- * Left @query@ value is preserved in case of duplicate keys.
instance Semigroup Route where
   l <> r = Route{path = l.path <> r.path, query = l.query <> r.query}

instance Monoid Route where
   mempty = Route{path = mempty, query = mempty}

--------------------------------------------------------------------------------

data Path i o = Path (i -> T.Text) (T.Text -> Either T.Text o)
   deriving stock (Functor)

instance Profunctor Path where
   dimap fi fo (Path gi go) = Path (gi . fi) (fmap fo . go)

--------------------------------------------------------------------------------

data Query i o = Query (i -> [T.Text]) ([T.Text] -> Either T.Text o)
   deriving stock (Functor)

instance Profunctor Query where
   dimap fi fo (Query gi go) = Query (gi . fi) (fmap fo . go)

--------------------------------------------------------------------------------

data CodecF i o
   = CodecPath (Path i o)
   | CodecQuery T.Text (Query i o)
   deriving stock (Functor)

instance Profunctor CodecF where
   dimap f g = \case
      CodecPath p -> CodecPath (dimap f g p)
      CodecQuery k q -> CodecQuery k (dimap f g q)

-- | How to go from @i@ to 'Route', and from 'Route' to @o@.
--
-- * Build with 'path', 'pathLiteral', 'pathCodec', 'query', 'queryMaybe',
-- 'querySome', 'queryMaybe', 'querySome'.
--
-- * Obtain the encoder with 'encoder'.
--
-- * Obtain the decoder with 'decoder'.
--
-- Example for encoding and decoding @\/users\/__foo__?sort=__bar__@
--
-- @
-- example1 :: 'Codec' ('T.Text', 'Int') ('Int', 'T.Text')
-- example1 = do
--   'pathLiteral' \"users\"
--   foo <- 'path' 'snd'
--   'pathLiteral' \"orders\"
--   bar <- 'query' \"sort\" 'fst'
--   'pure' (foo, bar)
-- @
newtype Codec i o = Codec (Ap (CodecF i) o)
   deriving newtype (Functor, Applicative)

instance Profunctor Codec where
   rmap = fmap
   lmap f (Codec a) = Codec $ hoistAp (lmap f) a

-- | Most general way to encode and decode a single path segment.
--
-- You should probably use one of v'path' or 'pathLiteral' instead of this lower
-- level function.
pathCodec
   :: (i -> T.Text)
   -- ^ Encode @i@ into a 'Route' @path@ segment.
   -> (T.Text -> Either T.Text o)
   -- ^ Decode a path segment into @o@, if possible.
   -> Codec i o
pathCodec f g = Codec $ liftAp $ CodecPath $ Path f g

-- | Most general way to encode and decode a single 'Route' @query@ string key
-- and its values, if any.
--
-- You should probably use one of v'query', 'queryMaybe', 'querySome' or
-- 'queryMany' instead of this lower level function.
queryCodec
   :: T.Text
   -- ^ 'Route' @query@ string key.
   -> (i -> [T.Text])
   -- ^ Encode @i@ into zero or more values.
   -- If the list is empty, the @key@ won't be mentioned
   -- in the query string at all.
   -> ([T.Text] -> Either T.Text o)
   -- ^ Decode from zero or more values into @o@.
   -- E.g., @?__key__=__value0__&__key__=__value1__&...@, or just no mention of
   -- @__key__@ altogether.
   -> Codec i o
queryCodec k f g = Codec $ liftAp $ CodecQuery k $ Query f g

-- | Literal segment in a 'Route' path.
pathLiteral :: T.Text -> Codec i ()
pathLiteral t = pathCodec (const t) \x ->
   if x == t then Right () else Left "Mismatch"

-- | Like 'pathCodec', but relies on 'ToPath' and 'FromPath'.
path
   :: (ToPath x, FromPath o)
   => (i -> x)
   -- ^ @'path' f == 'lmap' f ('path' 'id')@, provided just for convenience.
   -> Codec i o
path f = pathCodec (toPath . f) fromPath

-- | Encode and decode the one the query string value for a particular key.
--
-- E.g., @?__key__=__value__&...@.
query
   :: (ToQuery x, FromQuery o)
   => T.Text
   -- ^ 'Route' @query@ string key.
   -> (i -> x)
   -- ^ @'query' k f == 'lmap' f ('query' k 'id')@, provided just for convenience.
   -> Codec i o
query k f = queryCodec k (pure . toQuery . f) \case
   [] -> Left "Too few"
   [t] -> fromQuery t
   _ -> Left "Too many"

-- | Encode and decode zero or one query string values for a particular key.
--
-- E.g., @?__key__=__value__&...@, or just no mention of @__key__@ altogether.
queryMaybe
   :: (ToQuery x, FromQuery o)
   => T.Text
   -- ^ 'Route' @query@ string key.
   -> (i -> Maybe x)
   -- ^ @'queryMaybe' k f == 'lmap' f ('queryMaybe' k 'id')@, provided just for convenience.
   -> Codec i (Maybe o)
queryMaybe k f = queryCodec k (maybe [] (pure . toQuery) . f) \case
   [] -> Right Nothing
   [t] -> Just <$> fromQuery t
   _ -> Left "Too many"

-- | Encode and decode one or more query string values for a particular key.
--
-- E.g., @?__key__=__value0__&__key__=__value1__&...@, or just no mention of
-- @__key__@ altogether.
querySome
   :: (ToQuery x, FromQuery o)
   => T.Text
   -- ^ 'Route' @query@ string key.
   -> (i -> NEL.NonEmpty x)
   -- ^ @'querySome' k f == 'lmap' f ('querySome' k 'id')@, provided just for convenience.
   -> Codec i (NEL.NonEmpty o)
querySome k f = queryCodec k (fmap toQuery . NEL.toList . f) \case
   t : ts -> traverse fromQuery (t NEL.:| ts)
   [] -> Left "Too few"

-- | Encode and decode zero or more query string values for a particular key.
--
-- E.g., @?__key__=__value0__&__key__=__value1__&...@.
queryMany
   :: (ToQuery x, FromQuery o)
   => T.Text
   -- ^ 'Route' @query@ string key.
   -> (i -> [x])
   -- ^ @'queryMany' k f == 'lmap' f ('queryMany' k 'id')@, provided just for convenience.
   -> Codec i [o]
queryMany k f = queryCodec k (fmap toQuery . f) (traverse fromQuery)

--------------------------------------------------------------------------------

-- | How to parse a 'Route' into an @o@.
--
-- Build with 'decoder', run with 'decode'.
newtype Decoder o = Decoder (Ap DecoderF o)
   deriving newtype (Functor, Applicative)

data DecoderF o
   = DecoderPath (T.Text -> Either T.Text o)
   | DecoderQuery T.Text ([T.Text] -> Either T.Text o)
   deriving stock (Functor)

decoder :: Codec i o -> Decoder o
decoder = \(Codec c) -> Decoder $ hoistAp g c
  where
   g = \case
      CodecPath (Path _ f) -> DecoderPath f
      CodecQuery k (Query _ f) -> DecoderQuery k f

decode :: Decoder o -> Route -> Either Err o
decode (Decoder af) = \r0 -> do
   (o, (ix, r1)) <- runStateT (runAp g af) (0, r0)
   case r1.path of
      [] -> Right o
      _ -> Left $ ErrPath ix "Leftover"
  where
   g :: DecoderF a -> StateT (Word, Route) (Either Err) a
   g df = StateT \(!ix, !r) -> case df of
      DecoderPath f -> case r.path of
         x : xs -> case f x of
            Right a -> Right (a, (ix + 1, r{path = xs}))
            Left e -> Left $ ErrPath ix e
         [] -> Left $ ErrPath ix "Missing"
      DecoderQuery k f -> do
         let (yvs, q1) = Map.updateLookupWithKey (\_ _ -> Nothing) k r.query
         case f (maybe [] NEL.toList yvs) of
            Right a -> Right (a, (ix, r{query = q1}))
            Left e -> Left $ ErrQuery k e

data Err
   = -- | Error parsing the 'Route' @path@ element at the given index.
     ErrPath Word T.Text
   | -- | Error parsing the 'Route' @query@ element at the given key.
     ErrQuery T.Text T.Text
   | ErrOther T.Text
   deriving stock (Eq, Show)

instance Exception Err

--------------------------------------------------------------------------------

-- | How to encode an @i@ as a 'Route'.
--
-- Build with 'encoder', run with 'encode'.
newtype Encoder i = Encoder (i -> Route)
   deriving (Contravariant, Divisible, Decidable) via (Op Route)
   deriving newtype (Semigroup, Monoid)

encoder :: Codec i o -> Encoder i
encoder = \(Codec af) -> Encoder $ runAp_ g af
  where
   g = \case
      CodecPath (Path f _) -> \i ->
         mempty{path = [f i]}
      CodecQuery k (Query f _) -> \i ->
         mempty{query = maybe mempty (Map.singleton k) (NEL.nonEmpty (f i))}

encode :: Encoder i -> i -> Route
encode (Encoder f) = f
{-# INLINE encode #-}
