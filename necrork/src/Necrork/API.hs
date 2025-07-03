{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Necrork.API
  ( module Necrork.API,
    module Necrork.API.NodeUrl,
    module Necrork.API.SwitchName,
    module Necrork.API.Timestamp,
  )
where

import Autodocodec
import Data.Aeson as JSON (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Data.Word
import Database.Persist.Sql
import Necrork.API.NodeUrl
import Necrork.API.SwitchName
import Necrork.API.Timestamp
import OptEnvConf
import Servant.API
import Servant.API.Generic

data NecrorkRoutes route = NecrorkRoutes
  { getPeers :: !(route :- GetPeers),
    postSync :: !(route :- PostSync),
    putSwitch :: !(route :- PutSwitch),
    deleteSwitch :: !(route :- DeleteSwitch),
    putAlive :: !(route :- PutAlive),
    getAlive :: !(route :- GetAlive)
  }
  deriving (Generic)

type PostSync =
  "sync"
    :> ReqBody '[JSON] PostSyncRequest
    :> Post '[JSON] PostSyncResponse

-- Must stay backwards compatible because nodes upgrade at different rates.
data PostSyncRequest = PostSyncRequest
  { -- Base url of the sending node
    postSyncRequestNodeUrl :: !NodeUrl,
    -- Timestamp according to the sending node.
    postSyncRequestTimestamp :: !Timestamp,
    postSyncRequestTimeout :: !(Maybe Word32),
    postSyncRequestNotifySettings :: !(Maybe NotifySettings),
    postSyncRequestNeedNotifySettings :: !Bool,
    postSyncRequestSwitchTimestamps :: !(Map SwitchName (Map NodeUrl Timestamp))
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec PostSyncRequest)

instance Validity PostSyncRequest

instance HasCodec PostSyncRequest where
  codec =
    object "PostSyncRequest" $
      PostSyncRequest
        <$> requiredField "node" "Sending node base url" .= postSyncRequestNodeUrl
        <*> requiredField "time" "Timestamp of request according to the sending node" .= postSyncRequestTimestamp
        <*> optionalField "timeout" "How long after hearing from the sending node to consider it dead" .= postSyncRequestTimeout
        <*> optionalField "notify" "How to notify the admin of the sending node" .= postSyncRequestNotifySettings
        <*> optionalFieldWithOmittedDefault "settings" False "Whether notify settings are requested" .= postSyncRequestNeedNotifySettings
        <*> optionalFieldWithOmittedDefault "switches" M.empty "Switch timestamps" .= postSyncRequestSwitchTimestamps

-- Keep this small, it is sent a lot.
-- Must stay backwards compatible because nodes upgrade at different rates.
data PostSyncResponse = PostSyncResponse
  { postSyncResponseTimestamp :: !Timestamp,
    postSyncResponseTimeout :: !(Maybe Word32),
    postSyncResponseNotifySettings :: !(Maybe NotifySettings)
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
    secretTextFileOrBareSetting
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

type GetPeers =
  "peers"
    :> Get '[JSON] GetPeersResponse

-- Must stay backwards compatible
data GetPeersResponse = GetPeersResponse
  { getPeersResponsePeers :: !(Set NodeUrl)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec GetPeersResponse)

instance Validity GetPeersResponse

instance HasCodec GetPeersResponse where
  codec =
    object "GetPeersResponse" $
      GetPeersResponse
        <$> optionalFieldWithOmittedDefault "peers" S.empty "Set of peers that the node knows about" .= getPeersResponsePeers

-- Must stay small and backwards compatible.
type PutSwitch =
  "switch"
    :> Capture "switch" SwitchName
    :> ReqBody '[JSON] PutSwitchRequest
    :> PutNoContent

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
        <$> requiredField "timeout" "How long after last hearing from the switch to consider it dead, in seconds" .= putSwitchRequestTimeout
        <*> requiredField "notify" "How to notify the administrator when the switch dies" .= putSwitchRequestNotifySettings

instance HasParser PutSwitchRequest where
  settingsParser =
    PutSwitchRequest
      <$> setting
        [ help "Timeout in seconds after which the switch is considered dead",
          name "timeout",
          reader auto,
          metavar "TIMEOUT"
        ]
      <*> settingsParser

type DeleteSwitch =
  "switch"
    :> Capture "switch" SwitchName
    :> Delete '[JSON] DeleteSwitchResponse

-- Parsing must stay backward compatible
data DeleteSwitchResponse = DeleteSwitchResponse
  { deleteSwitchResponseDeleted :: Bool
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec DeleteSwitchResponse)

instance Validity DeleteSwitchResponse

instance HasCodec DeleteSwitchResponse where
  codec =
    object "DeleteSwitchResponse" $
      DeleteSwitchResponse
        <$> requiredField "deleted" "True if the switch existed and was deleted, false if it didn't exist." .= deleteSwitchResponseDeleted

-- Must stay small and backwards compatible.
type PutAlive =
  "alive"
    :> Capture "switch" SwitchName
    :> PutNoContent

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
