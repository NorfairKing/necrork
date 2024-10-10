{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Necrork.Peer.API
  ( module Necrork.Peer.API,
    module Necrork.Peer.API.SwitchName,
    module Necrork.Peer.API.Timestamp,
  )
where

import Autodocodec
import Control.Monad
import Data.Aeson as JSON (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Data.Word
import Database.Persist.Sql
import Necrork.Peer.API.SwitchName
import Necrork.Peer.API.Timestamp
import OptEnvConf
import Servant.API
import Servant.API.Generic
import Servant.Client

instance Validity Scheme

instance Validity BaseUrl where
  validate burl =
    mconcat
      [ genericValidate burl,
        declare "it roundtrips through parseBaseUrl" $
          parseBaseUrl (showBaseUrl burl) == Just burl
      ]

instance HasCodec BaseUrl where
  codec =
    bimapCodec
      ( \s -> case parseBaseUrl s of
          Left err -> Left (show err)
          Right burl -> Right burl
      )
      showBaseUrl
      codec

instance PersistField BaseUrl where
  toPersistValue = toPersistValue . showBaseUrl
  fromPersistValue =
    fromPersistValue
      >=> ( \s -> case parseBaseUrl s of
              Left err -> Left (T.pack (show err))
              Right burl -> Right burl
          )

instance PersistFieldSql BaseUrl where
  sqlType Proxy = sqlType (Proxy :: Proxy String)

data FooBarRoutes route = FooBarRoutes
  { postSync :: !(route :- PostSync),
    putSwitch :: !(route :- PutSwitch),
    putAlive :: !(route :- PutAlive),
    getAlive :: !(route :- GetAlive)
  }
  deriving (Generic)

data TimestampTuple = TimestampTuple
  { timestampTupleHearing :: !Timestamp,
    timestampTupleHeard :: !Timestamp
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TimestampTuple)

instance Validity TimestampTuple

instance HasCodec TimestampTuple where
  codec = bimapCodec f g $ listCodec codec
    where
      f = \case
        [timestampTupleHearing, timestampTupleHeard] -> Right TimestampTuple {..}
        _ -> Left "Couldn't parse timestamp tuple"
      g TimestampTuple {..} = [timestampTupleHearing, timestampTupleHeard]

type PostSync =
  "sync"
    :> ReqBody '[JSON] PostSyncRequest
    :> Post '[JSON] PostSyncResponse

-- Must stay backwards compatible because nodes upgrade at different rates.
data PostSyncRequest = PostSyncRequest
  { -- Base url of the sending node
    postSyncRequestNodeBaseUrl :: !BaseUrl,
    -- Timestamp according to the sending node.
    postSyncRequestTimestamp :: !Timestamp,
    postSyncRequestTimeout :: !(Maybe Word32),
    postSyncRequestNotifySettings :: !(Maybe NotifySettings),
    postSyncRequestNeedNotifySettings :: !Bool,
    postSyncRequestNodeTimestamps :: !(Map BaseUrl (Map BaseUrl TimestampTuple)),
    postSyncRequestSwitchTimestamps :: !(Map SwitchName (Map BaseUrl Timestamp))
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec PostSyncRequest)

instance Validity PostSyncRequest

instance HasCodec PostSyncRequest where
  codec =
    object "PostSyncRequest" $
      PostSyncRequest
        <$> requiredField "node" "Sending node base url" .= postSyncRequestNodeBaseUrl
        <*> requiredField "time" "Timestamp of request according to the sending node" .= postSyncRequestTimestamp
        <*> optionalField "timeout" "How long after hearing from the sending node to consider it dead" .= postSyncRequestTimeout
        <*> optionalField "notify" "How to notify the admin of the sending node" .= postSyncRequestNotifySettings
        <*> optionalFieldWithOmittedDefault "settings" False "Whether notify settings are requested" .= postSyncRequestNeedNotifySettings
        <*> optionalFieldWithOmittedDefault "nodes" M.empty "Node timestamps" .= postSyncRequestNodeTimestamps
        <*> optionalFieldWithOmittedDefault "switches" M.empty "Switch timestamps" .= postSyncRequestSwitchTimestamps

-- Keep this small, it is sent a lot.
-- Must stay backwards compatible because nodes upgrade at different rates.
data PostSyncResponse = PostSyncResponse
  { postSyncResponseTimestamp :: !Timestamp,
    postSyncResponseTimeout :: !(Maybe Word32),
    postSyncResponseNotifySettings :: !(Maybe NotifySettings),
    postSyncResponseNodeTimestamps :: !(Map BaseUrl (Map BaseUrl TimestampTuple)),
    postSyncResponseSwitchTimestamps :: !(Map SwitchName (Map BaseUrl Timestamp))
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec PostSyncResponse)

instance Validity PostSyncResponse

instance HasCodec PostSyncResponse where
  codec =
    object "PostSyncResponse" $
      PostSyncResponse
        <$> requiredField "time" "Timestamp of responding according to the receiving node" .= postSyncResponseTimestamp
        <*> optionalField "timeout" "How long after hearing from the responding node to consider it dead" .= postSyncResponseTimeout
        <*> optionalField "notify" "How to notify an administrator if the responding node is considered dead" .= postSyncResponseNotifySettings
        <*> optionalFieldWithOmittedDefault "nodes" M.empty "Node timestamps" .= postSyncResponseNodeTimestamps
        <*> optionalFieldWithOmittedDefault "switches" M.empty "Switch timestamps" .= postSyncResponseSwitchTimestamps

data NotifyIntraySettings = NotifyIntraySettings
  { notifyIntraySettingUsername :: Text,
    notifyIntraySettingAccessKeySecret :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec NotifyIntraySettings)

instance Validity NotifyIntraySettings

instance HasCodec NotifyIntraySettings where
  codec =
    object "NotifyIntraySettings" $
      typeField "intray" NotifyIntraySettings
        <*> requiredField "username" "Intray username" .= notifyIntraySettingUsername
        <*> requiredField "key" "Intray access key secret" .= notifyIntraySettingAccessKeySecret

instance HasParser NotifyIntraySettings where
  settingsParser = parseNotifyIntraySettings

{-# ANN parseNotifyIntraySettings ("NOCOVER" :: String) #-}
parseNotifyIntraySettings :: Parser NotifyIntraySettings
parseNotifyIntraySettings = do
  notifyIntraySettingUsername <-
    setting
      [ help "Username",
        name "username",
        reader str,
        metavar "USERNAME"
      ]
  notifyIntraySettingAccessKeySecret <-
    setting
      [ help "Access key",
        name "key",
        reader str,
        metavar "KEY"
      ]
  pure NotifyIntraySettings {..}

-- Must stay backwards compatible because:
--
-- 1. They'll be stored in the db
-- 2. They'll be sent in the API
data NotifySettings
  = NotifyIntray !NotifyIntraySettings
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec NotifySettings)

instance Validity NotifySettings

instance PersistField NotifySettings where
  fromPersistValue = fromPersistValueJSON
  toPersistValue = toPersistValueJSON

instance PersistFieldSql NotifySettings where
  sqlType Proxy = sqlType (Proxy :: Proxy ByteString)

instance HasCodec NotifySettings where
  codec = dimapCodec NotifyIntray (\(NotifyIntray is) -> is) codec

instance HasParser NotifySettings where
  settingsParser =
    choice
      [ subAll "intray" $ NotifyIntray <$> settingsParser
      ]

type PutSwitch =
  "switch"
    :> Capture "switch" SwitchName
    :> ReqBody '[JSON] PutSwitchRequest
    :> Put '[JSON] PutSwitchResponse

-- Must stay backwards compatible
data PutSwitchRequest = PutSwitchRequest
  { putSwitchRequestTimeout :: !Word32,
    putSwitchRequestNotifySettings :: !NotifySettings
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec PutSwitchRequest)

instance Validity PutSwitchRequest

instance HasCodec PutSwitchRequest where
  codec =
    object "PutSwitchRequest" $
      PutSwitchRequest
        <$> requiredField "timeout" "How long after last hearing from the switch to consider it dead" .= putSwitchRequestTimeout
        <*> requiredField "notify" "How to notify the administrator when the switch dies" .= putSwitchRequestNotifySettings

-- Parsing must stay backward compatible
data PutSwitchResponse = PutSwitchResponse
  { putSwitchResponsePeers :: !(Set BaseUrl)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec PutSwitchResponse)

instance Validity PutSwitchResponse

instance HasCodec PutSwitchResponse where
  codec =
    object "PutSwitchResponse" $
      PutSwitchResponse
        <$> optionalFieldWithOmittedDefault "peers" S.empty "Alternative peers that the switch can contact" .= putSwitchResponsePeers

type PutAlive =
  "alive"
    :> Capture "switch" SwitchName
    :> ReqBody '[JSON] PutAliveRequest
    :> Put '[JSON] PutAliveResponse

-- Must stay backwards compatible
data PutAliveRequest = PutAliveRequest
  {
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec PutAliveRequest)

instance Validity PutAliveRequest

instance HasCodec PutAliveRequest where
  codec =
    object "PutAliveRequest" $
      pure PutAliveRequest

-- Keep this small, it is sent a lot.
-- Parsing must stay backward compatible
data PutAliveResponse = PutAliveResponse
  {
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec PutAliveResponse)

instance Validity PutAliveResponse

instance HasCodec PutAliveResponse where
  codec =
    object "PutAliveResponse" $
      pure PutAliveResponse

type GetAlive =
  "alive"
    :> Capture "switch" SwitchName
    :> Get '[JSON] GetAliveResponse

-- Keep this small, it is sent a lot.
data GetAliveResponse = GetAliveResponse
  { getAliveResponseAlive :: Bool
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec GetAliveResponse)

instance Validity GetAliveResponse

instance HasCodec GetAliveResponse where
  codec =
    object "GetAliveResponse" $
      GetAliveResponse
        <$> requiredField "alive" "Guess for whether the switch is alive" .= getAliveResponseAlive

typeField :: Text -> a -> ObjectCodec b a
typeField typeName a =
  a <$ requiredFieldWith' "type" (literalTextCodec typeName) .= const typeName
