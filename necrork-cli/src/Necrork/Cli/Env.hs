{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Necrork.Cli.Env where

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as SB8
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version (showVersion)
import Necrork.Cli.OptParse
import Necrork.Client
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Paths_necrork_cli (version)
import Text.Colour
import UnliftIO

newtype CliM a = CliM
  { unCliM :: ReaderT Env (LoggingT IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadReader Env,
      MonadLogger,
      MonadLoggerIO
    )

runCliM :: Settings -> CliM a -> IO a
runCliM settings func =
  withEnv settings $ \env ->
    runMyLoggingT (runReaderT (unCliM func) env)

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
  peers <- discoverPeers
  forEachOfPeers peers func

discoverPeers :: CliM (NonEmpty NodeUrl)
discoverPeers = do
  peers <- asks envPeers
  errsOrResults <- forEachOfPeers peers $ do
    -- This is a placeholder for the actual discovery logic.
    -- Replace with the actual client call to discover peers.
    getPeersResponsePeers <$> getPeers necrorkClient
  discoverdPeers <- case errsOrResults of
    Left (errs, oks) -> do
      forM_ errs $ \(peer, err) ->
        logWarnN $
          T.pack $
            concat
              [ "Failed to discover peers from: ",
                show (showBaseUrl (unNodeUrl peer)),
                "\n",
                show err
              ]
      pure $ S.unions $ map snd oks
    Right peers' -> pure $ S.unions $ map snd $ NE.toList peers'
  -- Safe because the original nonempty list is part of this set.
  let originalSet = S.fromList (NE.toList peers)
  let newPeers = S.difference discoverdPeers originalSet
  when (not (null newPeers)) $
    forM_ newPeers $ \peer ->
      logInfoN $
        T.pack $
          concat
            [ "Discovered new peer: ",
              show (showBaseUrl (unNodeUrl peer))
            ]

  pure $ NE.fromList $ S.toList $ S.union discoverdPeers originalSet

forEachOfPeers ::
  NonEmpty NodeUrl ->
  ClientM a ->
  CliM
    ( Either
        (NonEmpty (NodeUrl, ClientError), [(NodeUrl, a)])
        (NonEmpty (NodeUrl, a))
    )
forEachOfPeers peers func = do
  man <- asks envHttpManager
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

runMyLoggingT :: LoggingT IO a -> IO a
runMyLoggingT func = runLoggingT func developmentLogFunc
  where
    developmentLogFunc loc source level msg =
      SB8.hPutStrLn stderr $ fromLogStr $ makeLogStr loc source level msg

makeLogStr :: Loc -> LogSource -> LogLevel -> LogStr -> LogStr
makeLogStr _ source level msg =
  mconcat
    [ toLogStr $
        renderChunksUtf8BSBuilder With24BitColours $
          map
            (logLevelColour level)
            [ "[",
              logLevelChunk level,
              if T.null source
                then ""
                else chunk $ "#" `mappend` source,
              "]"
            ],
      " ",
      msg
    ]

logLevelColour :: LogLevel -> (Chunk -> Chunk)
logLevelColour = \case
  LevelDebug -> fore white
  LevelInfo -> fore yellow
  LevelWarn -> fore orange
  LevelError -> fore red
  LevelOther _ -> id
  where
    orange = color256 214

logLevelChunk :: LogLevel -> Chunk
logLevelChunk = \case
  LevelDebug -> "DEBUG"
  LevelInfo -> "INFO"
  LevelWarn -> "WARNING"
  LevelError -> "ERROR"
  LevelOther t -> chunk t
