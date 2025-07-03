{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Necrork.Cli.Env where

import Control.Monad
import Control.Monad.Reader
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version (showVersion)
import Necrork.Cli.OptParse
import Necrork.Client
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Paths_necrork_cli (version)
import Servant.Client
import UnliftIO

newtype CliM a = CliM
  { unCliM :: ReaderT Env IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader Env)

runCliM :: Settings -> CliM a -> IO a
runCliM settings func =
  withEnv settings $ \env ->
    runReaderT (unCliM func) env

data Env = Env
  { envHttpManager :: !HTTP.Manager,
    envPeers :: !(NE.NonEmpty NodeUrl)
  }

withEnv :: Settings -> (Env -> IO a) -> IO a
withEnv Settings {..} func = do
  let envPeers = settingPeers
  let managerSets =
        tlsManagerSettings
          { managerModifyRequest = \request -> do
              let headers =
                    ( "User-Agent",
                      TE.encodeUtf8 $ T.pack $ "necrork-" <> showVersion version
                    )
                      : HTTP.requestHeaders request
              pure $ request {HTTP.requestHeaders = headers}
          }
  envHttpManager <- liftIO $ HTTP.newManager managerSets
  func Env {..}

forEachPeer ::
  ClientM a ->
  CliM
    ( Either
        (NonEmpty (NodeUrl, ClientError), [(NodeUrl, a)])
        (NonEmpty (NodeUrl, a))
    )
forEachPeer func = do
  man <- asks envHttpManager
  peers <- asks envPeers
  results <- forConcurrently peers $ \peer -> do
    let cenv = mkClientEnv man (unNodeUrl peer)
    liftIO $ (,) peer <$> runClientM func cenv
  pure $ categoriseResults results

categoriseResults ::
  NonEmpty (NodeUrl, Either ClientError a) ->
  Either
    (NonEmpty (NodeUrl, ClientError), [(NodeUrl, a)])
    (NonEmpty (NodeUrl, a))
categoriseResults results =
  let (errs, oks) = partitionZipped (NE.toList results)
   in case NE.nonEmpty errs of
        Just ne -> Left (ne, oks)
        Nothing ->
          Right $ NE.fromList oks

-- Safe because oks must be non-empty if errs is empty.

partitionZipped :: [(a, Either b c)] -> ([(a, b)], [(a, c)])
partitionZipped =
  partitionEithers
    . map
      ( \(a, e) -> case e of
          Left b -> Left (a, b)
          Right c -> Right (a, c)
      )
