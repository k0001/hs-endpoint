{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Usually, you don't need to import this module unless
-- you are trying to reuse some of 'Path''s underlying primitives.
-- Just import "Waypoint" instead.
module Waypoint.Path
   ( -- * PathValue
    PathValue (..)
   , ToPathValue (..)
   , FromPathValue (..)

    -- * PathF
   , PathF (..)
   , pathFDecode
   , pathFEncode

    -- * PathCodec
   , PathCodec (..)
   , pathDecode
   , pathEncode
   , path
   , pathLiteral

    -- * ErrPath
   , ErrPath (..)
   )
where

import Control.Applicative
import Control.Applicative.Free.Fast
import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Monoid (Endo (..))
import Data.Profunctor
import Data.Text qualified as T
import Text.Read (readMaybe)
import Witherable qualified as W
import Prelude

--------------------------------------------------------------------------------

-- | How to encode and decode a single path segment.
-- (e.g., the @hello@ in @\/hello\/world@.
data PathValue i o = PathValue
   { encode :: i -> T.Text
   , decode :: T.Text -> Maybe o
   }

instance W.Filterable (PathValue i) where
   mapMaybe f (PathValue i o) = PathValue i (o >=> f)

instance Functor (PathValue i) where
   fmap = rmap

instance Profunctor PathValue where
   dimap f g (PathValue i o) = PathValue (i . f) (fmap g . o)

-- | 'PathValue' based on 'ToPathValue' and 'FromPathValue'.
pathValue :: (ToPathValue i, FromPathValue o) => PathValue i o
pathValue = PathValue toPathValue fromPathValue

--------------------------------------------------------------------------------

-- | This datatype seems useless, and it is, but naming things is hard, so we
-- add this name for consistency with 'QueryF' and 'HeaderF'. Also, we will add
-- metadata here someday.
newtype PathF i o = PathF
   { segment :: PathValue i o
   }
   deriving newtype (Functor, Profunctor, W.Filterable)

-- | See 'pathDecodeF'.
data PathFDecodeState = PathFDecodeState
   { index :: Int
   -- ^ Next index.
   , input :: [T.Text]
   -- ^ Available raw input to decode.
   }
   deriving (Eq, Show)

-- | You probably don't need to use this unless you are building an
-- 'Applicative' on top of 'PathF' yourself.
pathFDecode
   :: PathF x a -> PathFDecodeState -> Either ErrPath (a, PathFDecodeState)
pathFDecode (PathF ps) = \s ->
   case s.input of
      t : ts1
         | Just a <- ps.decode t ->
            Right (a, s{index = s.index + 1, input = ts1})
      _ -> Left $ ErrPath s.index

-- | You probably don't need to use this unless you are building an
-- 'Applicative' on top of 'PathF' yourself.
pathFEncode :: PathF i o -> i -> Endo [T.Text]
pathFEncode p = \i -> Endo (p.segment.encode i :)

--------------------------------------------------------------------------------

newtype PathCodec i o = PathCodec (Ap (PathF i) o)
   deriving newtype (Functor, Applicative)

instance Profunctor PathCodec where
   rmap = fmap
   lmap f (PathCodec a) = PathCodec $ hoistAp (lmap f) a

pathEncode :: PathCodec i o -> i -> [T.Text]
pathEncode (PathCodec af) = flip appEndo [] . runAp_ pathFEncode af

-- | Decodes the given path segments into @o@, if possible.
--
-- Parses from left to right, returning leftover segments, if any.
pathDecode :: PathCodec i o -> [T.Text] -> Either ErrPath (o, [T.Text])
pathDecode = \(PathCodec af) -> \ !ts0 -> do
   let s0 = PathFDecodeState{index = 0, input = ts0}
   (o, s1) <- runStateT (runAp (StateT . pathFDecode) af) s0
   pure (o, s1.input)

data ErrPath
   = -- | Error parsing the path element at the given index.
     ErrPath Int
   deriving stock (Eq, Show)

instance Exception ErrPath

--------------------------------------------------------------------------------

-- | Encode and decode a path segment.
path
   :: (ToPathValue x, FromPathValue o)
   => (i -> x)
   -- ^ @'path' f == 'lmap' f ('path' 'id')@, provided just for convenience.
   -> PathCodec i o
path f = PathCodec $ liftAp $ PathF $ lmap f pathValue

-- | Literal segment in a URL path.
pathLiteral :: T.Text -> PathCodec i ()
pathLiteral t =
   PathCodec $ liftAp $ PathF $ PathValue (const t) \x -> guard (x == t)

--------------------------------------------------------------------------------

class ToPathValue i where
   -- | Render @i@ into a single path segment.
   toPathValue :: i -> T.Text
   default toPathValue :: (Show i) => i -> T.Text
   {-# INLINE toPathValue #-}
   toPathValue = T.pack . show

class FromPathValue o where
   -- | Parse a single path segment into an @o@.
   fromPathValue :: T.Text -> Maybe o
   default fromPathValue :: (Read o) => T.Text -> Maybe o
   {-# INLINE fromPathValue #-}
   fromPathValue = readMaybe . T.unpack
