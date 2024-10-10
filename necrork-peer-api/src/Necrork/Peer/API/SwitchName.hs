{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Necrork.Peer.API.SwitchName
  ( SwitchName (..),
  )
where

import Autodocodec
import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist.Sql
import GHC.Generics (Generic)
import Web.HttpApiData

newtype SwitchName = SwitchName {unSwitchName :: Text}
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( Show,
      Read,
      IsString,
      PersistField,
      PersistFieldSql,
      ToHttpApiData,
      FromHttpApiData,
      FromJSONKey,
      ToJSONKey
    )

instance HasCodec SwitchName where
  codec = dimapCodec SwitchName unSwitchName codec

instance Validity SwitchName where
  validate sn@(SwitchName t) =
    mconcat
      [ genericValidate sn,
        declare "is not empty" $ not $ T.null t
      ]
