module Route.Class
   ( ToPath (..)
   , FromPath (..)
   , ToQuery (..)
   , FromQuery (..)
   ) where

import Control.Monad
import Data.Int
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as Time
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

instance ToPath T.Text where
   {-# INLINE toPath #-}
   toPath = id

instance ToPath String where
   {-# INLINE toPath #-}
   toPath = T.pack

instance ToPath Int
instance ToPath Int8
instance ToPath Int16
instance ToPath Int32
instance ToPath Int64
instance ToPath Integer
instance ToPath Word
instance ToPath Word8
instance ToPath Word16
instance ToPath Word32
instance ToPath Word64
instance ToPath Natural

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

instance FromPath T.Text where
   {-# INLINE fromPath #-}
   fromPath = Right

instance FromPath String where
   {-# INLINE fromPath #-}
   fromPath = Right . T.unpack

instance FromPath Int
instance FromPath Int8
instance FromPath Int16
instance FromPath Int32
instance FromPath Int64
instance FromPath Integer
instance FromPath Word
instance FromPath Word8
instance FromPath Word16
instance FromPath Word32
instance FromPath Word64
instance FromPath Natural

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

instance ToQuery String where
   toQuery = T.pack
   {-# INLINE toQuery #-}

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

instance FromQuery String where
   fromQuery = \t -> Right (T.unpack t)
   {-# INLINE fromQuery #-}

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
