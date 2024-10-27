{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Necrork.API.NodeUrl
  ( NodeUrl (..),
    parseNodeUrl,
    showNodeUrl,
  )
where

import Autodocodec
import Control.Arrow (left)
import Control.Exception
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Char as Char
import Data.Proxy
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist.Sql
import GHC.Generics (Generic)
import Servant.Client

newtype NodeUrl = NodeUrl {unNodeUrl :: BaseUrl}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSONKey, ToJSONKey)
  deriving (FromJSON, ToJSON) via (Autodocodec NodeUrl)

instance Validity NodeUrl where
  validate (NodeUrl burl@BaseUrl {..}) =
    mconcat
      [ decorate "baseUrlScheme" $ trivialValidation baseUrlScheme,
        annotate baseUrlHost "baseUrlHost",
        annotate baseUrlPort "baseUrlPort",
        annotate baseUrlPath "baseUrlPath",
        declare "The hostname is not empty" $ not $ null baseUrlHost,
        declare "The port is positive" $ baseUrlPort >= 0,
        declare "The port is less than 65536" $ baseUrlPort <= 65536,
        declare "The hostname does not contain spaces" $ not $ any Char.isSpace baseUrlHost,
        declare "The host is entirely within Latin1" $ all Char.isLatin1 baseUrlHost,
        declare "The path does not contain spaces" $ not $ any Char.isSpace baseUrlPath,
        declare "The path is entirely within Latin1" $ all Char.isLatin1 baseUrlPath,
        declare "The path does not start with a slash" $
          case baseUrlPath of
            ('/' : _) -> False
            _ -> True,
        declare
          ( unlines
              [ "Parsing the url after rendering it yields the same url",
                "expected: "
                  <> show
                    (showBaseUrl <$> (parseBaseUrl (showBaseUrl burl) :: Either SomeException BaseUrl)),
                "actual:   " <> showBaseUrl burl
              ]
          )
          $ parseBaseUrl (showBaseUrl burl)
            == Just burl
      ]

instance PersistField NodeUrl where
  toPersistValue = PersistText . T.pack . showBaseUrl . unNodeUrl
  fromPersistValue pv = do
    s <- fromPersistValue pv
    fmap NodeUrl $ left (T.pack . show) $ parseBaseUrl s

instance PersistFieldSql NodeUrl where
  sqlType Proxy = sqlType (Proxy :: Proxy String)

instance HasCodec NodeUrl where
  codec =
    bimapCodec
      ( \s -> case parseBaseUrl s of
          Left err -> Left (show err)
          Right burl -> Right (NodeUrl burl)
      )
      (showBaseUrl . unNodeUrl)
      codec

parseNodeUrl :: String -> Maybe NodeUrl
parseNodeUrl = fmap NodeUrl . parseBaseUrl

showNodeUrl :: NodeUrl -> String
showNodeUrl = showBaseUrl . unNodeUrl
