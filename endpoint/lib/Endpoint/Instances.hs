{-# OPTIONS_GHC -Wno-orphans #-}
module Endpoint.Instances () where

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
import Prelude

import Endpoint.Header
import Endpoint.Path
import Endpoint.Query

--------------------------------------------------------------------------------

instance ToHeaderValue B.ByteString where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = id

instance ToHeaderValue BL.ByteString where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = BL.toStrict

instance ToHeaderValue BB.Builder where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = BL.toStrict . BB.toLazyByteString

instance ToHeaderValue Int where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue Int8 where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue Int16 where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue Int32 where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue Int64 where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue Integer where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue Word where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue Word8 where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue Word16 where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue Word32 where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue Word64 where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue Natural where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue Double where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue Float where
   {-# INLINE toHeaderValue #-}
   toHeaderValue = fromString . show

instance ToHeaderValue UUID.UUID where
   toHeaderValue = UUID.toASCIIBytes
   {-# INLINE toHeaderValue #-}

instance ToHeaderValue Time.UTCTime where
   toHeaderValue = toHeaderValue . Time.utcToZonedTime Time.utc
   {-# INLINE toHeaderValue #-}

instance ToHeaderValue Time.ZonedTime where
   toHeaderValue = fromString . Time.iso8601Show
   {-# INLINE toHeaderValue #-}

instance ToHeaderValue Time.LocalTime where
   toHeaderValue = fromString . Time.iso8601Show
   {-# INLINE toHeaderValue #-}

instance ToHeaderValue Time.Day where
   toHeaderValue = fromString . Time.iso8601Show
   {-# INLINE toHeaderValue #-}

instance ToHeaderValue Time.TimeOfDay where
   toHeaderValue = fromString . Time.iso8601Show
   {-# INLINE toHeaderValue #-}

instance ToHeaderValue Time.CalendarDiffDays where
   toHeaderValue = fromString . Time.iso8601Show
   {-# INLINE toHeaderValue #-}

instance ToHeaderValue Time.CalendarDiffTime where
   toHeaderValue = fromString . Time.iso8601Show
   {-# INLINE toHeaderValue #-}

instance ToHeaderValue Time.TimeZone where
   toHeaderValue = fromString . Time.iso8601Show
   {-# INLINE toHeaderValue #-}

--------------------------------------------------------------------------------

instance FromHeaderValue B.ByteString where
   {-# INLINE fromHeaderValue #-}
   fromHeaderValue = Just

instance FromHeaderValue BL.ByteString where
   {-# INLINE fromHeaderValue #-}
   fromHeaderValue = Just . BL.fromStrict

instance FromHeaderValue Int

instance FromHeaderValue Int8

instance FromHeaderValue Int16

instance FromHeaderValue Int32

instance FromHeaderValue Int64

instance FromHeaderValue Integer

instance FromHeaderValue Word

instance FromHeaderValue Word8

instance FromHeaderValue Word16

instance FromHeaderValue Word32

instance FromHeaderValue Word64

instance FromHeaderValue Natural

instance FromHeaderValue Double

instance FromHeaderValue Float

instance FromHeaderValue UUID.UUID where
   fromHeaderValue = UUID.fromASCIIBytes
   {-# INLINE fromHeaderValue #-}

instance FromHeaderValue Time.UTCTime where
   {-# INLINE fromHeaderValue #-}
   fromHeaderValue = T.decodeASCII' >=> fromQueryValue

instance FromHeaderValue Time.ZonedTime where
   fromHeaderValue = T.decodeASCII' >=> fromQueryValue
   {-# INLINE fromHeaderValue #-}

instance FromHeaderValue Time.LocalTime where
   fromHeaderValue = T.decodeASCII' >=> fromQueryValue
   {-# INLINE fromHeaderValue #-}

instance FromHeaderValue Time.Day where
   fromHeaderValue = T.decodeASCII' >=> fromQueryValue
   {-# INLINE fromHeaderValue #-}

instance FromHeaderValue Time.TimeOfDay where
   fromHeaderValue = T.decodeASCII' >=> fromQueryValue
   {-# INLINE fromHeaderValue #-}

instance FromHeaderValue Time.CalendarDiffDays where
   fromHeaderValue = T.decodeASCII' >=> fromQueryValue
   {-# INLINE fromHeaderValue #-}

instance FromHeaderValue Time.CalendarDiffTime where
   fromHeaderValue = T.decodeASCII' >=> fromQueryValue
   {-# INLINE fromHeaderValue #-}

instance FromHeaderValue Time.TimeZone where
   fromHeaderValue = T.decodeASCII' >=> fromQueryValue
   {-# INLINE fromHeaderValue #-}

--------------------------------------------------------------------------------

instance ToPathValue T.Text where
   {-# INLINE toPathValue #-}
   toPathValue = toQueryValue

instance ToPathValue TL.Text where
   {-# INLINE toPathValue #-}
   toPathValue = toQueryValue

instance ToPathValue TB.Builder where
   toPathValue = TL.toStrict . TB.toLazyText
   {-# INLINE toPathValue #-}

instance ToPathValue String where
   {-# INLINE toPathValue #-}
   toPathValue = toQueryValue

instance ToPathValue Char where
   {-# INLINE toPathValue #-}
   toPathValue = toQueryValue

instance ToPathValue Int

instance ToPathValue Int8

instance ToPathValue Int16

instance ToPathValue Int32

instance ToPathValue Int64

instance ToPathValue Integer

instance ToPathValue Word

instance ToPathValue Word8

instance ToPathValue Word16

instance ToPathValue Word32

instance ToPathValue Word64

instance ToPathValue Natural

instance ToPathValue Double

instance ToPathValue Float

instance ToPathValue UUID.UUID where
   toPathValue = toQueryValue
   {-# INLINE toPathValue #-}

instance ToPathValue Time.UTCTime where
   toPathValue = toQueryValue
   {-# INLINE toPathValue #-}

instance ToPathValue Time.ZonedTime where
   toPathValue = toQueryValue
   {-# INLINE toPathValue #-}

instance ToPathValue Time.LocalTime where
   toPathValue = toQueryValue
   {-# INLINE toPathValue #-}

instance ToPathValue Time.Day where
   toPathValue = toQueryValue
   {-# INLINE toPathValue #-}

instance ToPathValue Time.TimeOfDay where
   toPathValue = toQueryValue
   {-# INLINE toPathValue #-}

instance ToPathValue Time.CalendarDiffDays where
   toPathValue = toQueryValue
   {-# INLINE toPathValue #-}

instance ToPathValue Time.CalendarDiffTime where
   toPathValue = toQueryValue
   {-# INLINE toPathValue #-}

instance ToPathValue Time.TimeZone where
   toPathValue = toQueryValue
   {-# INLINE toPathValue #-}

--------------------------------------------------------------------------------

instance FromPathValue T.Text where
   {-# INLINE fromPathValue #-}
   fromPathValue = fromQueryValue

instance FromPathValue TL.Text where
   {-# INLINE fromPathValue #-}
   fromPathValue = fromQueryValue

instance FromPathValue String where
   {-# INLINE fromPathValue #-}
   fromPathValue = fromQueryValue

instance FromPathValue Char where
   {-# INLINE fromPathValue #-}
   fromPathValue = fromQueryValue

instance FromPathValue Int

instance FromPathValue Int8

instance FromPathValue Int16

instance FromPathValue Int32

instance FromPathValue Int64

instance FromPathValue Integer

instance FromPathValue Word

instance FromPathValue Word8

instance FromPathValue Word16

instance FromPathValue Word32

instance FromPathValue Word64

instance FromPathValue Natural

instance FromPathValue Double

instance FromPathValue Float

instance FromPathValue UUID.UUID where
   fromPathValue = fromQueryValue
   {-# INLINE fromPathValue #-}

instance FromPathValue Time.UTCTime where
   fromPathValue = fromQueryValue
   {-# INLINE fromPathValue #-}

instance FromPathValue Time.ZonedTime where
   fromPathValue = fromQueryValue
   {-# INLINE fromPathValue #-}

instance FromPathValue Time.LocalTime where
   fromPathValue = fromQueryValue
   {-# INLINE fromPathValue #-}

instance FromPathValue Time.Day where
   fromPathValue = fromQueryValue
   {-# INLINE fromPathValue #-}

instance FromPathValue Time.TimeOfDay where
   fromPathValue = fromQueryValue
   {-# INLINE fromPathValue #-}

instance FromPathValue Time.CalendarDiffDays where
   fromPathValue = fromQueryValue
   {-# INLINE fromPathValue #-}

instance FromPathValue Time.CalendarDiffTime where
   fromPathValue = fromQueryValue
   {-# INLINE fromPathValue #-}

instance FromPathValue Time.TimeZone where
   fromPathValue = fromQueryValue
   {-# INLINE fromPathValue #-}

--------------------------------------------------------------------------------

instance ToQueryValue T.Text where
   toQueryValue = id
   {-# INLINE toQueryValue #-}

instance ToQueryValue TL.Text where
   toQueryValue = TL.toStrict
   {-# INLINE toQueryValue #-}

instance ToQueryValue TB.Builder where
   toQueryValue = TL.toStrict . TB.toLazyText
   {-# INLINE toQueryValue #-}

instance ToQueryValue String where
   toQueryValue = T.pack
   {-# INLINE toQueryValue #-}

instance ToQueryValue Char where
   {-# INLINE toQueryValue #-}
   toQueryValue = T.singleton

instance ToQueryValue Int
instance ToQueryValue Int8
instance ToQueryValue Int16
instance ToQueryValue Int32
instance ToQueryValue Int64
instance ToQueryValue Integer
instance ToQueryValue Word
instance ToQueryValue Word8
instance ToQueryValue Word16
instance ToQueryValue Word32
instance ToQueryValue Word64
instance ToQueryValue Natural
instance ToQueryValue Double
instance ToQueryValue Float

instance ToQueryValue UUID.UUID where
   toQueryValue = UUID.toText
   {-# INLINE toQueryValue #-}

-- | 'Time.ISO8601'. @yyyy-mm-ddThh:mm:ss/[.ssssssssssss]/+00:00@
instance ToQueryValue Time.UTCTime where
   toQueryValue = toQueryValue . Time.utcToZonedTime Time.utc
   {-# INLINE toQueryValue #-}

-- | 'Time.ISO8601'. @yyyy-mm-ddThh:mm:ss/[.ssssssssssss]/±hh:mm@
instance ToQueryValue Time.ZonedTime where
   toQueryValue = T.pack . Time.iso8601Show
   {-# INLINE toQueryValue #-}

-- | 'Time.ISO8601'. @yyyy-mm-ddThh:mm:ss/[.ssssssssssss]/@
instance ToQueryValue Time.LocalTime where
   toQueryValue = T.pack . Time.iso8601Show
   {-# INLINE toQueryValue #-}

-- | 'Time.ISO8601'. @yyyy-mm-dd@
instance ToQueryValue Time.Day where
   toQueryValue = T.pack . Time.iso8601Show
   {-# INLINE toQueryValue #-}

-- | 'Time.ISO8601'. @hh:mm:ss/[.ssssssssssss]/@
instance ToQueryValue Time.TimeOfDay where
   toQueryValue = T.pack . Time.iso8601Show
   {-# INLINE toQueryValue #-}

-- | 'Time.ISO8601'. @PyYmMdD@
instance ToQueryValue Time.CalendarDiffDays where
   toQueryValue = T.pack . Time.iso8601Show
   {-# INLINE toQueryValue #-}

-- | 'Time.ISO8601'. @PyYmMdDThHmMs/[.ssssssssssss]/S@
instance ToQueryValue Time.CalendarDiffTime where
   toQueryValue = T.pack . Time.iso8601Show
   {-# INLINE toQueryValue #-}

-- | 'Time.ISO8601'. @±hh:mm@
instance ToQueryValue Time.TimeZone where
   toQueryValue = T.pack . Time.iso8601Show
   {-# INLINE toQueryValue #-}

--------------------------------------------------------------------------------

instance FromQueryValue T.Text where
   fromQueryValue = Just
   {-# INLINE fromQueryValue #-}

instance FromQueryValue TL.Text where
   {-# INLINE fromQueryValue #-}
   fromQueryValue = Just . TL.fromStrict

instance FromQueryValue String where
   {-# INLINE fromQueryValue #-}
   fromQueryValue = Just . T.unpack

instance FromQueryValue Char where
   {-# INLINE fromQueryValue #-}
   fromQueryValue = \t -> case T.uncons t of
      Just (c, rest) | T.null rest -> Just c
      _ -> Nothing

instance FromQueryValue Int
instance FromQueryValue Int8
instance FromQueryValue Int16
instance FromQueryValue Int32
instance FromQueryValue Int64
instance FromQueryValue Integer
instance FromQueryValue Word
instance FromQueryValue Word8
instance FromQueryValue Word16
instance FromQueryValue Word32
instance FromQueryValue Word64
instance FromQueryValue Natural
instance FromQueryValue Double
instance FromQueryValue Float

instance FromQueryValue UUID.UUID where
   fromQueryValue = UUID.fromText
   {-# INLINE fromQueryValue #-}

-- | Like for 'Time.ZonedTime'. Compatible with 'ToQueryValue'.
instance FromQueryValue Time.UTCTime where
   fromQueryValue = fmap Time.zonedTimeToUTC . fromQueryValue
   {-# INLINE fromQueryValue #-}

-- | 'Time.ISO8601', or seconds since Epoch with optional decimal
-- part of up to picosecond precission.  Compatible with 'ToQueryValue'.
--
-- TODO: Currently precission over picoseconds is successfully parsed but
-- silently floored. This is an issue in "Data.Time.Format.ISO8601". Fix.
instance FromQueryValue Time.ZonedTime where
   fromQueryValue (T.unpack -> s) =
      asum
         [ Time.iso8601ParseM s
         , Time.utcToZonedTime Time.utc
            <$> Time.iso8601ParseM s
         , Time.utcToZonedTime Time.utc
            <$> Time.parseTimeM False Time.defaultTimeLocale "%s%Q" s
         ]

-- | 'Time.ISO8601'. Compatible with 'ToQueryValue'.
instance FromQueryValue Time.LocalTime where
   fromQueryValue = fromQueryValue >=> Time.iso8601ParseM
   {-# INLINE fromQueryValue #-}

-- | 'Time.ISO8601'. Compatible with 'ToQueryValue'.
instance FromQueryValue Time.Day where
   fromQueryValue = fromQueryValue >=> Time.iso8601ParseM
   {-# INLINE fromQueryValue #-}

-- | 'Time.ISO8601'. Compatible with 'ToQueryValue'.
--
-- TODO: Currently precission over picoseconds is successfully parsed but
-- silently floored. This is an issue in "Data.Time.Format.ISO8601". Fix.
instance FromQueryValue Time.TimeOfDay where
   fromQueryValue = fromQueryValue >=> Time.iso8601ParseM
   {-# INLINE fromQueryValue #-}

-- | 'Time.ISO8601'. Compatible with 'ToQueryValue'.
--
-- TODO: Currently precission over picoseconds is successfully parsed but
-- silently floored. This is an issue in "Data.Time.Format.ISO8601". Fix.
instance FromQueryValue Time.CalendarDiffDays where
   fromQueryValue = fromQueryValue >=> Time.iso8601ParseM
   {-# INLINE fromQueryValue #-}

-- | 'Time.ISO8601'. Compatible with 'ToQueryValue'.
--
-- TODO: Currently precission over picoseconds is successfully parsed but
-- silently floored. This is an issue in "Data.Time.Format.ISO8601". Fix.
instance FromQueryValue Time.CalendarDiffTime where
   fromQueryValue = fromQueryValue >=> Time.iso8601ParseM
   {-# INLINE fromQueryValue #-}

-- | 'Time.ISO8601'. Compatible with 'ToQueryValue'.
instance FromQueryValue Time.TimeZone where
   fromQueryValue = fromQueryValue >=> Time.iso8601ParseM
   {-# INLINE fromQueryValue #-}
