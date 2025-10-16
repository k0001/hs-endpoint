module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString qualified as B
import Data.CaseInsensitive qualified as CI
import Data.Fixed
import Data.Functor.Contravariant
import Data.Int
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.String
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time qualified as Time
import Data.Time.Calendar.OrdinalDate qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import Data.Time.Format.ISO8601 qualified as Time
import Data.Typeable
import Data.UUID.Types qualified as UUID
import Data.Word
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as HR
import Numeric.Natural
import Test.Tasty (testGroup)
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hedgehog qualified as Tasty
import Test.Tasty.Runners (TestTree)
import Test.Tasty.Runners qualified as Tasty

import Endpoint qualified as E

--------------------------------------------------------------------------------
main :: IO ()
main = do
   Tasty.defaultMainWithIngredients
      [ Tasty.consoleTestReporter
      , Tasty.listingTests
      ]
      $ Tasty.localOption (Tasty.HedgehogTestLimit (Just 1000))
      $ tree

tree :: TestTree
tree =
   testGroup
      "Endpoint"
      [ treeHttpTypes
      , treeQueryPath
      , treeHeader
      ]

treeHttpTypes :: TestTree
treeHttpTypes =
   testGroup
      "treeHttpTypes"
      [ testProperty "queryToHttpTypes/queryFromHttpTypes" $
         H.property do
            r0 <- H.forAll query
            r0 H.=== E.queryFromHttpTypes (E.queryToHttpTypes r0)
      , testProperty "headerToHttpTypes/headerFromHttpTypes" $
         H.property do
            r0 <- H.forAll header
            r0 H.=== E.headerFromHttpTypes (E.headerToHttpTypes r0)
      ]

treeQueryPath :: TestTree
treeQueryPath =
   testGroup
      "ToQuery/FromQuery - ToPath/FromPath"
      [ t @UUID.UUID uuid4
      , t @Int $ H.integral HR.constantBounded
      , t @Int8 $ H.integral HR.constantBounded
      , t @Int16 $ H.integral HR.constantBounded
      , t @Int32 $ H.integral HR.constantBounded
      , t @Int64 $ H.integral HR.constantBounded
      , t @Word $ H.integral HR.constantBounded
      , t @Word8 $ H.integral HR.constantBounded
      , t @Word16 $ H.integral HR.constantBounded
      , t @Word32 $ H.integral HR.constantBounded
      , t @Word64 $ H.integral HR.constantBounded
      , t @Word64 $ H.integral HR.constantBounded
      , t @Natural $ H.integral $ HR.constant 0 maxNatural
      , t @Integer $ H.integral $ HR.constantFrom 0 minInteger maxInteger
      , t @Char H.unicode
      , t @String $ H.string (HR.constant 0 50) H.unicode
      , t @T.Text $ H.text (HR.constant 0 50) H.unicode
      , t @TL.Text $ fmap TL.fromStrict $ H.text (HR.constant 0 50) H.unicode
      , t @Time.UTCTime $ genUTCTime (HR.constantFrom epochUTCTime minUTCTime maxUTCTime)
      , t @Time.LocalTime $ Time.utcToLocalTime Time.utc <$> genUTCTime (HR.constantFrom epochUTCTime minUTCTime maxUTCTime)
      , t @Time.CalendarDiffDays $ calendarDiffDays
      , t @Time.CalendarDiffTime $ Time.calendarTimeDays <$> calendarDiffDays
      , t @Time.TimeOfDay $ timeOfDay
      , t @Time.TimeZone $ timeZone
      , t @Time.Day $ day
      , t @Double $ H.double (HR.constantFrom 0 (fromIntegral minInteger) (fromIntegral maxInteger))
      , t @Float $ H.float (HR.constantFrom 0 (fromIntegral minInteger) (fromIntegral maxInteger))
      ]
  where
   t
      :: forall a
       . ( Typeable a
         , Eq a
         , Show a
         , E.ToQuery a
         , E.FromQuery a
         , E.ToPath a
         , E.FromPath a
         )
      => H.Gen a
      -> TestTree
   t ga =
      testGroup
         (tyConName (typeRepTyCon (typeRep ga)))
         [ testProperty "query" $ H.property do
            a <- H.forAll ga
            Just a H.=== E.fromQuery (E.toQuery a)
         , testProperty "path" $ H.property do
            a <- H.forAll ga
            Just a H.=== E.fromPath (E.toPath a)
         ]

treeHeader :: TestTree
treeHeader =
   testGroup
      "ToHeader/FromHeader"
      [ t @UUID.UUID uuid4
      , t @Int $ H.integral HR.constantBounded
      , t @Int8 $ H.integral HR.constantBounded
      , t @Int16 $ H.integral HR.constantBounded
      , t @Int32 $ H.integral HR.constantBounded
      , t @Int64 $ H.integral HR.constantBounded
      , t @Word $ H.integral HR.constantBounded
      , t @Word8 $ H.integral HR.constantBounded
      , t @Word16 $ H.integral HR.constantBounded
      , t @Word32 $ H.integral HR.constantBounded
      , t @Word64 $ H.integral HR.constantBounded
      , t @Word64 $ H.integral HR.constantBounded
      , t @Natural $ H.integral $ HR.constant 0 maxNatural
      , t @Integer $ H.integral $ HR.constantFrom 0 minInteger maxInteger
      , t @Time.UTCTime $ genUTCTime (HR.constantFrom epochUTCTime minUTCTime maxUTCTime)
      , t @Time.LocalTime $ Time.utcToLocalTime Time.utc <$> genUTCTime (HR.constantFrom epochUTCTime minUTCTime maxUTCTime)
      , t @Time.CalendarDiffDays $ calendarDiffDays
      , t @Time.CalendarDiffTime $ Time.calendarTimeDays <$> calendarDiffDays
      , t @Time.TimeOfDay $ timeOfDay
      , t @Time.TimeZone $ timeZone
      , t @Time.Day $ day
      , t @Double $ H.double (HR.constantFrom 0 (fromIntegral minInteger) (fromIntegral maxInteger))
      , t @Float $ H.float (HR.constantFrom 0 (fromIntegral minInteger) (fromIntegral maxInteger))
      ]
  where
   t
      :: forall a
       . ( Typeable a
         , Eq a
         , Show a
         , E.ToHeader a
         , E.FromHeader a
         )
      => H.Gen a
      -> TestTree
   t ga =
      testGroup
         (tyConName (typeRepTyCon (typeRep ga)))
         [ testProperty "header" $ H.property do
            a <- H.forAll ga
            Just a H.=== E.fromHeader (E.toHeader a)
         ]

uuid4 :: (H.MonadGen m) => m UUID.UUID
uuid4 =
   UUID.fromWords64
      <$> H.integral HR.constantBounded
      <*> H.integral HR.constantBounded

calendarDiffDays :: (H.MonadGen m) => m Time.CalendarDiffDays
calendarDiffDays =
   Time.CalendarDiffDays
      <$> H.integral (HR.constantFrom 0 minInteger maxInteger)
      <*> H.integral (HR.constantFrom 0 minInteger maxInteger)

timeZone :: (H.MonadGen m) => m Time.TimeZone
timeZone =
   Time.minutesToTimeZone
      <$> H.integral (HR.constantFrom 0 (negate 3599) 3600)

timeOfDay :: (H.MonadGen m) => m Time.TimeOfDay
timeOfDay =
   Time.TimeOfDay
      <$> H.integral (HR.constantFrom 0 0 23)
      <*> H.integral (HR.constantFrom 0 0 59)
      <*> fmap MkFixed (H.integral (HR.constantFrom 0 0 60_999_999_999_999))

day :: (H.MonadGen m) => m Time.Day
day =
   Time.fromOrdinalDate
      <$> H.integral (HR.constantFrom 0 (-9999) 9999)
      <*> H.integral (HR.constantFrom 1 1 366)

maxNatural :: Natural
maxNatural = 2 ^ (256 :: Int) - 1

maxInteger :: Integer
maxInteger = 2 ^ (255 :: Int) - 1

minInteger :: Integer
minInteger = complement maxInteger

minUTCTime :: Time.UTCTime
minUTCTime = fromJust $ Time.iso8601ParseM "-9999-01-01T00:00:00Z"

maxUTCTime :: Time.UTCTime
maxUTCTime = fromJust $ Time.iso8601ParseM "9999-12-31T24:00:00Z"

epochUTCTime :: Time.UTCTime
epochUTCTime = posixPicoSecondsToUTCTime 0

genUTCTime :: (H.MonadGen m) => H.Range Time.UTCTime -> m Time.UTCTime
genUTCTime =
   fmap posixPicoSecondsToUTCTime
      . H.integral
      . fmap utcTimeToPOSIXPicoSeconds

utcTimeToPOSIXPicoSeconds :: Time.UTCTime -> Integer
utcTimeToPOSIXPicoSeconds t = i
  where
   MkFixed i = Time.nominalDiffTimeToSeconds $ Time.utcTimeToPOSIXSeconds t

posixPicoSecondsToUTCTime :: Integer -> Time.UTCTime
posixPicoSecondsToUTCTime =
   Time.posixSecondsToUTCTime . Time.secondsToNominalDiffTime . MkFixed

path :: (H.MonadGen m) => m [T.Text]
path = H.list (HR.constant 0 9) (H.text (HR.constant 0 9) H.unicode)

query :: (H.MonadGen m) => m (Map.Map T.Text (NEL.NonEmpty T.Text))
query =
   Map.fromListWith (<>) <$> do
      ks <- H.list (HR.constant 0 9) (H.text (HR.constant 0 3) H.unicode)
      forM ks \k -> do
         vs <- H.nonEmpty (HR.constant 1 9) (H.text (HR.constant 0 9) H.unicode)
         pure (k, vs)

header
   :: forall m
    . (H.MonadGen m)
   => m (Map.Map (CI.CI B.ByteString) (NEL.NonEmpty B.ByteString))
header =
   Map.fromListWith (<>) <$> do
      ks <- H.list (HR.constant 0 9) (CI.mk <$> H.bytes (HR.constant 1 9))
      forM ks \k -> do
         vs <- H.nonEmpty (HR.constant 1 9) (H.bytes (HR.constant 1 9))
         pure (k, vs)
