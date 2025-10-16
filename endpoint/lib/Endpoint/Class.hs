module Endpoint.Class
   ( ToHeader (..)
   , FromHeader (..)
   , ToPath (..)
   , FromPath (..)
   , ToQuery (..)
   , FromQuery (..)
   ) where

import Control.Applicative
import Control.Monad
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as Time
import Data.UUID.Types qualified as UUID
import Data.Word
import Numeric.Natural
import Text.Read (readMaybe)
import Prelude

--------------------------------------------------------------------------------

class ToHeader i where
   -- | Render @i@ into a single literal header value.
   --
   -- __WARNING__: The 'B.ByteString' is literally as it is on the header value.
   toHeader :: i -> B.ByteString

instance ToHeader B.ByteString where
   {-# INLINE toHeader #-}
   toHeader = id

instance ToHeader BL.ByteString where
   {-# INLINE toHeader #-}
   toHeader = BL.toStrict

instance ToHeader BB.Builder where
   {-# INLINE toHeader #-}
   toHeader = BL.toStrict . BB.toLazyByteString

instance ToHeader Int where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader Int8 where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader Int16 where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader Int32 where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader Int64 where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader Integer where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader Word where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader Word8 where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader Word16 where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader Word32 where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader Word64 where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader Natural where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader Double where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader Float where
   {-# INLINE toHeader #-}
   toHeader = fromString . show

instance ToHeader UUID.UUID where
   toHeader = UUID.toASCIIBytes
   {-# INLINE toHeader #-}

instance ToHeader Time.UTCTime where
   toHeader = toHeader . Time.utcToZonedTime Time.utc
   {-# INLINE toHeader #-}

instance ToHeader Time.ZonedTime where
   toHeader = fromString . Time.iso8601Show
   {-# INLINE toHeader #-}

instance ToHeader Time.LocalTime where
   toHeader = fromString . Time.iso8601Show
   {-# INLINE toHeader #-}

instance ToHeader Time.Day where
   toHeader = fromString . Time.iso8601Show
   {-# INLINE toHeader #-}

instance ToHeader Time.TimeOfDay where
   toHeader = fromString . Time.iso8601Show
   {-# INLINE toHeader #-}

instance ToHeader Time.CalendarDiffDays where
   toHeader = fromString . Time.iso8601Show
   {-# INLINE toHeader #-}

instance ToHeader Time.CalendarDiffTime where
   toHeader = fromString . Time.iso8601Show
   {-# INLINE toHeader #-}

instance ToHeader Time.TimeZone where
   toHeader = fromString . Time.iso8601Show
   {-# INLINE toHeader #-}

--------------------------------------------------------------------------------

class FromHeader o where
   -- | Parse a single literal header value into an @o@.
   --
   -- __WARNING__: The 'B.ByteString' is literally as it is on the header value.
   fromHeader :: B.ByteString -> Maybe o
   default fromHeader :: (Read o) => B.ByteString -> Maybe o
   fromHeader = T.decodeASCII' >=> readMaybe . T.unpack

instance FromHeader B.ByteString where
   {-# INLINE fromHeader #-}
   fromHeader = Just

instance FromHeader BL.ByteString where
   {-# INLINE fromHeader #-}
   fromHeader = Just . BL.fromStrict

instance FromHeader Int

instance FromHeader Int8

instance FromHeader Int16

instance FromHeader Int32

instance FromHeader Int64

instance FromHeader Integer

instance FromHeader Word

instance FromHeader Word8

instance FromHeader Word16

instance FromHeader Word32

instance FromHeader Word64

instance FromHeader Natural

instance FromHeader Double

instance FromHeader Float

instance FromHeader UUID.UUID where
   fromHeader = UUID.fromASCIIBytes
   {-# INLINE fromHeader #-}

instance FromHeader Time.UTCTime where
   {-# INLINE fromHeader #-}
   fromHeader = T.decodeASCII' >=> fromQuery

instance FromHeader Time.ZonedTime where
   fromHeader = T.decodeASCII' >=> fromQuery
   {-# INLINE fromHeader #-}

instance FromHeader Time.LocalTime where
   fromHeader = T.decodeASCII' >=> fromQuery
   {-# INLINE fromHeader #-}

instance FromHeader Time.Day where
   fromHeader = T.decodeASCII' >=> fromQuery
   {-# INLINE fromHeader #-}

instance FromHeader Time.TimeOfDay where
   fromHeader = T.decodeASCII' >=> fromQuery
   {-# INLINE fromHeader #-}

instance FromHeader Time.CalendarDiffDays where
   fromHeader = T.decodeASCII' >=> fromQuery
   {-# INLINE fromHeader #-}

instance FromHeader Time.CalendarDiffTime where
   fromHeader = T.decodeASCII' >=> fromQuery
   {-# INLINE fromHeader #-}

instance FromHeader Time.TimeZone where
   fromHeader = T.decodeASCII' >=> fromQuery
   {-# INLINE fromHeader #-}

--------------------------------------------------------------------------------

class ToPath i where
   -- | Render @i@ into a single path segment.
   toPath :: i -> T.Text
   default toPath :: (Show i) => i -> T.Text
   {-# INLINE toPath #-}
   toPath = T.pack . show

instance ToPath T.Text where
   {-# INLINE toPath #-}
   toPath = toQuery

instance ToPath TL.Text where
   {-# INLINE toPath #-}
   toPath = toQuery

instance ToPath TB.Builder where
   toPath = TL.toStrict . TB.toLazyText
   {-# INLINE toPath #-}

instance ToPath String where
   {-# INLINE toPath #-}
   toPath = toQuery

instance ToPath Char where
   {-# INLINE toPath #-}
   toPath = toQuery

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

instance ToPath Double

instance ToPath Float

instance ToPath UUID.UUID where
   toPath = toQuery
   {-# INLINE toPath #-}

instance ToPath Time.UTCTime where
   toPath = toQuery
   {-# INLINE toPath #-}

instance ToPath Time.ZonedTime where
   toPath = toQuery
   {-# INLINE toPath #-}

instance ToPath Time.LocalTime where
   toPath = toQuery
   {-# INLINE toPath #-}

instance ToPath Time.Day where
   toPath = toQuery
   {-# INLINE toPath #-}

instance ToPath Time.TimeOfDay where
   toPath = toQuery
   {-# INLINE toPath #-}

instance ToPath Time.CalendarDiffDays where
   toPath = toQuery
   {-# INLINE toPath #-}

instance ToPath Time.CalendarDiffTime where
   toPath = toQuery
   {-# INLINE toPath #-}

instance ToPath Time.TimeZone where
   toPath = toQuery
   {-# INLINE toPath #-}

--------------------------------------------------------------------------------

class FromPath o where
   -- | Parse a single path segment into an @o@.
   fromPath :: T.Text -> Maybe o
   default fromPath :: (Read o) => T.Text -> Maybe o
   {-# INLINE fromPath #-}
   fromPath = readMaybe . T.unpack

instance FromPath T.Text where
   {-# INLINE fromPath #-}
   fromPath = fromQuery

instance FromPath TL.Text where
   {-# INLINE fromPath #-}
   fromPath = fromQuery

instance FromPath String where
   {-# INLINE fromPath #-}
   fromPath = fromQuery

instance FromPath Char where
   {-# INLINE fromPath #-}
   fromPath = fromQuery

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

instance FromPath Double

instance FromPath Float

instance FromPath UUID.UUID where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

instance FromPath Time.UTCTime where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

instance FromPath Time.ZonedTime where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

instance FromPath Time.LocalTime where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

instance FromPath Time.Day where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

instance FromPath Time.TimeOfDay where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

instance FromPath Time.CalendarDiffDays where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

instance FromPath Time.CalendarDiffTime where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

instance FromPath Time.TimeZone where
   fromPath = fromQuery
   {-# INLINE fromPath #-}

--------------------------------------------------------------------------------

class ToQuery i where
   -- | Encode @i@ into a query string value.
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

instance ToQuery TB.Builder where
   toQuery = TL.toStrict . TB.toLazyText
   {-# INLINE toQuery #-}

instance ToQuery String where
   toQuery = T.pack
   {-# INLINE toQuery #-}

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
   -- | Decode from one query string value into @o@.
   --
   -- E.g., from @?__key__=__value__&...@.
   fromQuery :: T.Text -> Maybe o
   default fromQuery :: (Read o) => T.Text -> Maybe o
   {-# INLINE fromQuery #-}
   fromQuery = readMaybe . T.unpack

instance FromQuery T.Text where
   fromQuery = Just
   {-# INLINE fromQuery #-}

instance FromQuery TL.Text where
   {-# INLINE fromQuery #-}
   fromQuery = Just . TL.fromStrict

instance FromQuery String where
   {-# INLINE fromQuery #-}
   fromQuery = Just . T.unpack

instance FromQuery Char where
   {-# INLINE fromQuery #-}
   fromQuery = \t -> case T.uncons t of
      Just (c, rest) | T.null rest -> Just c
      _ -> Nothing

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
   fromQuery = UUID.fromText
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
   fromQuery (T.unpack -> s) =
      asum
         [ Time.iso8601ParseM s
         , Time.utcToZonedTime Time.utc
            <$> Time.iso8601ParseM s
         , Time.utcToZonedTime Time.utc
            <$> Time.parseTimeM False Time.defaultTimeLocale "%s%Q" s
         ]

-- | 'Time.ISO8601'. Compatible with 'ToQuery'.
instance FromQuery Time.LocalTime where
   fromQuery = fromQuery >=> Time.iso8601ParseM
   {-# INLINE fromQuery #-}

-- | 'Time.ISO8601'. Compatible with 'ToQuery'.
instance FromQuery Time.Day where
   fromQuery = fromQuery >=> Time.iso8601ParseM
   {-# INLINE fromQuery #-}

-- | 'Time.ISO8601'. Compatible with 'ToQuery'.
--
-- TODO: Currently precission over picoseconds is successfully parsed but
-- silently floored. This is an issue in "Data.Time.Format.ISO8601". Fix.
instance FromQuery Time.TimeOfDay where
   fromQuery = fromQuery >=> Time.iso8601ParseM
   {-# INLINE fromQuery #-}

-- | 'Time.ISO8601'. Compatible with 'ToQuery'.
--
-- TODO: Currently precission over picoseconds is successfully parsed but
-- silently floored. This is an issue in "Data.Time.Format.ISO8601". Fix.
instance FromQuery Time.CalendarDiffDays where
   fromQuery = fromQuery >=> Time.iso8601ParseM
   {-# INLINE fromQuery #-}

-- | 'Time.ISO8601'. Compatible with 'ToQuery'.
--
-- TODO: Currently precission over picoseconds is successfully parsed but
-- silently floored. This is an issue in "Data.Time.Format.ISO8601". Fix.
instance FromQuery Time.CalendarDiffTime where
   fromQuery = fromQuery >=> Time.iso8601ParseM
   {-# INLINE fromQuery #-}

-- | 'Time.ISO8601'. Compatible with 'ToQuery'.
instance FromQuery Time.TimeZone where
   fromQuery = fromQuery >=> Time.iso8601ParseM
   {-# INLINE fromQuery #-}
