module Route.Class
   ( ToPath (..)
   , FromPath (..)
   , ToQuery (..)
   , FromQuery (..)
   ) where

import Control.Monad
import Data.Int
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as Time
import Data.UUID.Types qualified as UUID
import Data.Word
import Numeric.Natural
import Text.Read (readMaybe)
import Prelude

--------------------------------------------------------------------------------

class ToPath i where
   -- | Render @i@ into a single 'Route' @path@ segment.
   toPath :: i -> T.Text
   default toPath :: (Show i) => i -> T.Text
   {-# INLINE toPath #-}
   toPath = T.pack . show

-- | Like 'ToQuery'.
instance ToPath T.Text where
   {-# INLINE toPath #-}
   toPath = toQuery

-- | Like 'ToQuery'.
instance ToPath TL.Text where
   {-# INLINE toPath #-}
   toPath = toQuery

-- | Like 'ToQuery'.
instance ToPath String where
   {-# INLINE toPath #-}
   toPath = toQuery

-- | Like 'ToQuery'.
instance ToPath Char where
   {-# INLINE toPath #-}
   toPath = toQuery

-- | Like 'ToQuery'.
instance ToPath Int

-- | Like 'ToQuery'.
instance ToPath Int8

-- | Like 'ToQuery'.
instance ToPath Int16

-- | Like 'ToQuery'.
instance ToPath Int32

-- | Like 'ToQuery'.
instance ToPath Int64

-- | Like 'ToQuery'.
instance ToPath Integer

-- | Like 'ToQuery'.
instance ToPath Word

-- | Like 'ToQuery'.
instance ToPath Word8

-- | Like 'ToQuery'.
instance ToPath Word16

-- | Like 'ToQuery'.
instance ToPath Word32

-- | Like 'ToQuery'.
instance ToPath Word64

-- | Like 'ToQuery'.
instance ToPath Natural

-- | Like 'ToQuery'.
instance ToPath Double

-- | Like 'ToQuery'.
instance ToPath Float

-- | Like 'ToQuery'.
instance ToPath UUID.UUID where
   toPath = toQuery
   {-# INLINE toPath #-}

-- | Like 'ToQuery'.
instance ToPath Time.UTCTime where
   toPath = toQuery
   {-# INLINE toPath #-}

-- | Like 'ToQuery'.
instance ToPath Time.ZonedTime where
   toPath = toQuery
   {-# INLINE toPath #-}

-- | Like 'ToQuery'.
instance ToPath Time.LocalTime where
   toPath = toQuery
   {-# INLINE toPath #-}

-- | Like 'ToQuery'.
instance ToPath Time.Day where
   toPath = toQuery
   {-# INLINE toPath #-}

-- | Like 'ToQuery'.
instance ToPath Time.TimeOfDay where
   toPath = toQuery
   {-# INLINE toPath #-}

-- | Like 'ToQuery'.
instance ToPath Time.CalendarDiffDays where
   toPath = toQuery
   {-# INLINE toPath #-}

-- | Like 'ToQuery'.
instance ToPath Time.CalendarDiffTime where
   toPath = toQuery
   {-# INLINE toPath #-}

-- | Like 'ToQuery'.
instance ToPath Time.TimeZone where
   toPath = toQuery
   {-# INLINE toPath #-}

--------------------------------------------------------------------------------

class FromPath o where
   -- | Parse a single 'Route' @path@ segment into an @a@.
   fromPath :: T.Text -> Either T.Text o
   default fromPath :: (Read o) => T.Text -> Either T.Text o
   {-# INLINE fromPath #-}
   fromPath = note "No parse" . readMaybe . T.unpack

-- | Like 'FromQuery'.
instance FromPath T.Text where
   {-# INLINE fromPath #-}
   fromPath = fromQuery

-- | Like 'FromQuery'.
instance FromPath TL.Text where
   {-# INLINE fromPath #-}
   fromPath = fromQuery

-- | Like 'FromQuery'.
instance FromPath String where
   {-# INLINE fromPath #-}
   fromPath = fromQuery

-- | Like 'FromQuery'.
instance FromPath Char where
   {-# INLINE fromPath #-}
   fromPath = fromQuery

-- | Like 'FromQuery'.
instance FromPath Int

-- | Like 'FromQuery'.
instance FromPath Int8

-- | Like 'FromQuery'.
instance FromPath Int16

-- | Like 'FromQuery'.
instance FromPath Int32

-- | Like 'FromQuery'.
instance FromPath Int64

-- | Like 'FromQuery'.
instance FromPath Integer

-- | Like 'FromQuery'.
instance FromPath Word

-- | Like 'FromQuery'.
instance FromPath Word8

-- | Like 'FromQuery'.
instance FromPath Word16

-- | Like 'FromQuery'.
instance FromPath Word32

-- | Like 'FromQuery'.
instance FromPath Word64

-- | Like 'FromQuery'.
instance FromPath Natural

-- | Like 'FromQuery'.
instance FromPath Double

-- | Like 'FromQuery'.
instance FromPath Float

-- | Like 'FromQuery'.
instance FromPath UUID.UUID where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

-- | Like 'FromQuery'.
instance FromPath Time.UTCTime where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

-- | Like 'FromQuery'.
instance FromPath Time.ZonedTime where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

-- | Like 'FromQuery'.
instance FromPath Time.LocalTime where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

-- | Like 'FromQuery'.
instance FromPath Time.Day where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

-- | Like 'FromQuery'.
instance FromPath Time.TimeOfDay where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

-- | Like 'FromQuery'.
instance FromPath Time.CalendarDiffDays where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

-- | Like 'FromQuery'.
instance FromPath Time.CalendarDiffTime where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

-- | Like 'FromQuery'.
instance FromPath Time.TimeZone where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

--------------------------------------------------------------------------------

class ToQuery i where
   -- | Encode @i@ into zero or more 'Route' @query@ string values.
   --
   -- If the list is empty, the @key@ won't be mentioned in the @query@ string
   -- at all.
   toQuery :: i -> T.Text
   default toQuery :: (Show i) => i -> T.Text
   {-# INLINE toQuery #-}
   toQuery = T.pack . show

instance ToQuery T.Text where
   toQuery = id
   {-# INLINE toQuery #-}

instance ToQuery TL.Text where
   toQuery = TL.toStrict
   {-# INLINE toQuery #-}

instance ToQuery String where
   toQuery = T.pack
   {-# INLINE toQuery #-}

-- | Like 'ToQuery'.
instance ToQuery Char where
   {-# INLINE toQuery #-}
   toQuery = T.singleton

instance ToQuery Int
instance ToQuery Int8
instance ToQuery Int16
instance ToQuery Int32
instance ToQuery Int64
instance ToQuery Integer
instance ToQuery Word
instance ToQuery Word8
instance ToQuery Word16
instance ToQuery Word32
instance ToQuery Word64
instance ToQuery Natural
instance ToQuery Double
instance ToQuery Float

instance ToQuery UUID.UUID where
   toQuery = UUID.toText
   {-# INLINE toQuery #-}

-- | 'Time.ISO8601'. @yyyy-mm-ddThh:mm:ss/[.ssssssssssss]/+00:00@
instance ToQuery Time.UTCTime where
   toQuery = toQuery . Time.utcToZonedTime Time.utc
   {-# INLINE toQuery #-}

-- | 'Time.ISO8601'. @yyyy-mm-ddThh:mm:ss/[.ssssssssssss]/±hh:mm@
instance ToQuery Time.ZonedTime where
   toQuery = T.pack . Time.iso8601Show
   {-# INLINE toQuery #-}

-- | 'Time.ISO8601'. @yyyy-mm-ddThh:mm:ss/[.ssssssssssss]/@
instance ToQuery Time.LocalTime where
   toQuery = T.pack . Time.iso8601Show
   {-# INLINE toQuery #-}

-- | 'Time.ISO8601'. @yyyy-mm-dd@
instance ToQuery Time.Day where
   toQuery = T.pack . Time.iso8601Show
   {-# INLINE toQuery #-}

-- | 'Time.ISO8601'. @hh:mm:ss/[.ssssssssssss]/@
instance ToQuery Time.TimeOfDay where
   toQuery = T.pack . Time.iso8601Show
   {-# INLINE toQuery #-}

-- | 'Time.ISO8601'. @PyYmMdD@
instance ToQuery Time.CalendarDiffDays where
   toQuery = T.pack . Time.iso8601Show
   {-# INLINE toQuery #-}

-- | 'Time.ISO8601'. @PyYmMdDThHmMs/[.ssssssssssss]/S@
instance ToQuery Time.CalendarDiffTime where
   toQuery = T.pack . Time.iso8601Show
   {-# INLINE toQuery #-}

-- | 'Time.ISO8601'. @±hh:mm@
instance ToQuery Time.TimeZone where
   toQuery = T.pack . Time.iso8601Show
   {-# INLINE toQuery #-}

--------------------------------------------------------------------------------

class FromQuery o where
   -- | Decode from one 'Route' @query@ string value into @o@.
   --
   -- E.g., from @?__key__=__value__&...@.
   fromQuery :: T.Text -> Either T.Text o
   default fromQuery :: (Read o) => T.Text -> Either T.Text o
   {-# INLINE fromQuery #-}
   fromQuery = \t -> case readMaybe (T.unpack t) of
      Just o -> Right o
      Nothing -> Left "No parse"

instance FromQuery T.Text where
   fromQuery = Right
   {-# INLINE fromQuery #-}

instance FromQuery TL.Text where
   {-# INLINE fromQuery #-}
   fromQuery = Right . TL.fromStrict

-- | Like 'FromQuery'.
instance FromQuery String where
   {-# INLINE fromQuery #-}
   fromQuery = Right . T.unpack

-- | Like 'FromQuery'.
instance FromQuery Char where
   {-# INLINE fromQuery #-}
   fromQuery = \t -> case T.uncons t of
      Just (c, rest) | T.null rest -> Right c
      _ -> Left "No parse"

instance FromQuery Int
instance FromQuery Int8
instance FromQuery Int16
instance FromQuery Int32
instance FromQuery Int64
instance FromQuery Integer
instance FromQuery Word
instance FromQuery Word8
instance FromQuery Word16
instance FromQuery Word32
instance FromQuery Word64
instance FromQuery Natural
instance FromQuery Double
instance FromQuery Float

instance FromQuery UUID.UUID where
   fromQuery = note "No UUID" . UUID.fromText
   {-# INLINE fromQuery #-}

-- | Like for 'Time.ZonedTime'. Compatible with 'ToQuery'.
instance FromQuery Time.UTCTime where
   fromQuery = fmap Time.zonedTimeToUTC . fromQuery
   {-# INLINE fromQuery #-}

-- | 'Time.ISO8601', or seconds since Epoch with optional decimal
-- part of up to picosecond precission.  Compatible with 'ToQuery'.
--
-- TODO: Currently precission over picoseconds is successfully parsed but
-- silently floored. This is an issue in "Data.Time.Format.ISO8601". Fix.
instance FromQuery Time.ZonedTime where
   fromQuery (T.unpack -> s)
      | Just zt <- Time.iso8601ParseM s = Right zt
      | Just u <- Time.iso8601ParseM s =
         Right $ Time.utcToZonedTime Time.utc u
      | Just u <- Time.parseTimeM False Time.defaultTimeLocale "%s%Q" s =
         Right $ Time.utcToZonedTime Time.utc u
      | otherwise = Left "No timestamp"

-- | 'Time.ISO8601'. Compatible with 'ToQuery'.
instance FromQuery Time.LocalTime where
   fromQuery = fromQuery >=> note "No timestamp" . Time.iso8601ParseM
   {-# INLINE fromQuery #-}

-- | 'Time.ISO8601'. Compatible with 'ToQuery'.
instance FromQuery Time.Day where
   fromQuery = fromQuery >=> note "No timestamp" . Time.iso8601ParseM
   {-# INLINE fromQuery #-}

-- | 'Time.ISO8601'. Compatible with 'ToQuery'.
--
-- TODO: Currently precission over picoseconds is successfully parsed but
-- silently floored. This is an issue in "Data.Time.Format.ISO8601". Fix.
instance FromQuery Time.TimeOfDay where
   fromQuery = fromQuery >=> note "No timestamp" . Time.iso8601ParseM
   {-# INLINE fromQuery #-}

-- | 'Time.ISO8601'. Compatible with 'ToQuery'.
--
-- TODO: Currently precission over picoseconds is successfully parsed but
-- silently floored. This is an issue in "Data.Time.Format.ISO8601". Fix.
instance FromQuery Time.CalendarDiffDays where
   fromQuery = fromQuery >=> note "No timestamp" . Time.iso8601ParseM
   {-# INLINE fromQuery #-}

-- | 'Time.ISO8601'. Compatible with 'ToQuery'.
--
-- TODO: Currently precission over picoseconds is successfully parsed but
-- silently floored. This is an issue in "Data.Time.Format.ISO8601". Fix.
instance FromQuery Time.CalendarDiffTime where
   fromQuery = fromQuery >=> note "No timestamp" . Time.iso8601ParseM
   {-# INLINE fromQuery #-}

-- | 'Time.ISO8601'. Compatible with 'ToQuery'.
instance FromQuery Time.TimeZone where
   fromQuery = fromQuery >=> note "No timestamp" . Time.iso8601ParseM
   {-# INLINE fromQuery #-}

--------------------------------------------------------------------------------

note :: a -> Maybe b -> Either a b
note a = \case
   Just b -> Right b
   Nothing -> Left a
{-# INLINE note #-}
