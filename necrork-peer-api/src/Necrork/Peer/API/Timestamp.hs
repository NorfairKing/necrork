{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Necrork.Peer.API.Timestamp
  ( Timestamp (..),
    mkTimestamp,
    timestampUTCTime,
    getCurrentTimestamp,
    addTimestamp,
    diffTimestamp,
    nominalDiffTimeToNanos,
  )
where

import Autodocodec
import Control.Monad.IO.Class
import Data.Int
import Data.Proxy
import Data.Time
import Data.Time.Clock.POSIX
import Data.Validity
import Database.Persist.Sql
import GHC.Generics (Generic)

-- UTC timestamp, nanoseconds since the unix epoch.
--
-- We use Int64 instead of Word64 because:
-- 1. Sqlite can only store Ints and will store a big Word64 as a real.
-- 2. There is this persistent bug, which makes ordering Word64 incorrect:
--    https://github.com/yesodweb/persistent/issues/1552
-- 3. 2^63 nanoseconds is about 300 years, which is fine.
newtype Timestamp = Timestamp {unTimestamp :: Int64}
  deriving stock (Show, Eq, Ord, Generic)

instance HasCodec Timestamp where
  codec = dimapCodec Timestamp unTimestamp codec

mkTimestamp :: UTCTime -> Timestamp
mkTimestamp = Timestamp . nominalDiffTimeToNanos . utcTimeToPOSIXSeconds

nominalDiffTimeToNanos :: NominalDiffTime -> Int64
nominalDiffTimeToNanos = floor . (1e9 *) . nominalDiffTimeToSeconds

timestampUTCTime :: Timestamp -> UTCTime
timestampUTCTime = posixSecondsToUTCTime . nominalDiffTimeFromNanos . unTimestamp

nominalDiffTimeFromNanos :: Int64 -> NominalDiffTime
nominalDiffTimeFromNanos = (/ 1e9) . fromIntegral

addTimestamp :: NominalDiffTime -> Timestamp -> Timestamp
addTimestamp ndt (Timestamp u2) = Timestamp $ u2 + nominalDiffTimeToNanos ndt

diffTimestamp :: Timestamp -> Timestamp -> NominalDiffTime
diffTimestamp (Timestamp u1) (Timestamp u2) = nominalDiffTimeFromNanos $ u1 - u2

getCurrentTimestamp :: (MonadIO m) => m Timestamp
getCurrentTimestamp = liftIO $ Timestamp . nominalDiffTimeToNanos <$> getPOSIXTime

instance Validity Timestamp

instance PersistField Timestamp where
  toPersistValue = toPersistValue . unTimestamp
  fromPersistValue pv = case fromPersistValue pv of
    Right i -> Right (Timestamp i)
    Left err -> case fromPersistValue pv of
      Left _ -> Left err
      Right u -> Right $ mkTimestamp u

instance PersistFieldSql Timestamp where
  sqlType Proxy = sqlType (Proxy :: Proxy Int64)
