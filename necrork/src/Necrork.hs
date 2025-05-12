{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Necrork
  ( NotifierSettings (..),
    makeNotifierSettings,
    withNotifier,
    withMNotifier,
    defaultNodeUrl,
    defaultNotifierLooperSettings,
    NotifierEnv (..),
    makeNotifierEnv,
    notifier,
    notifierLooper,
    runNotifierOnce,
    module Necrork.Client,
  )
where

import Control.Concurrent.TokenLimiter.Concurrent
import Control.Monad
import Control.Monad.Logger
import Data.Foldable
import Data.Maybe
import Data.Sequence (Seq, ViewL (..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version (showVersion)
import Data.Void
import Data.Word
import Looper
import Necrork.Client
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import OptEnvConf
import Paths_necrork (version)
import UnliftIO

withMNotifier ::
  forall m a.
  (MonadUnliftIO m, MonadLogger m) =>
  Maybe NotifierSettings ->
  m a ->
  m a
withMNotifier = maybe id withNotifier

withNotifier ::
  forall m a.
  (MonadUnliftIO m, MonadLogger m) =>
  NotifierSettings ->
  m a ->
  m a
withNotifier settings func = do
  man <- liftIO $ HTTP.newManager tlsManagerSettings
  notifierEnv <- makeNotifierEnv man settings
  voidOrResult <- case notifier notifierEnv of
    Nothing -> Right <$> func
    Just notifierThread ->
      -- We use race because the notifier runs forever
      race notifierThread func
  case voidOrResult :: Either Void a of
    Left _ -> undefined
    Right a -> pure a

data NotifierSettings = NotifierSettings
  { notifierSettingSwitchName :: !SwitchName,
    notifierSettingNodeUrl :: !NodeUrl,
    notifierSettingLooperSettings :: !LooperSettings,
    notifierSettingTimeout :: !(Maybe Word32),
    notifierSettingNotifySettings :: !NotifySettings
  }
  deriving (Show, Eq)

makeNotifierSettings :: SwitchName -> NotifySettings -> NotifierSettings
makeNotifierSettings notifierSettingSwitchName notifierSettingNotifySettings =
  let notifierSettingNodeUrl = defaultNodeUrl
      notifierSettingLooperSettings = defaultNotifierLooperSettings
      notifierSettingTimeout = Nothing
   in NotifierSettings {..}

instance HasParser NotifierSettings where
  settingsParser = parseNotifierSettings

{-# ANN parseNotifierSettings ("NOCOVER" :: String) #-}
parseNotifierSettings :: OptEnvConf.Parser NotifierSettings
parseNotifierSettings = do
  notifierSettingSwitchName <-
    setting
      [ help "Name of the necrork switch",
        reader str,
        name "switch",
        metavar "SWITCH_NAME"
      ]
  notifierSettingNodeUrl <-
    setting
      [ help "Base url of the necrork server",
        reader $ maybeReader parseNodeUrl,
        name "url",
        metavar "URL",
        valueWithShown showNodeUrl defaultNodeUrl
      ]
  notifierSettingLooperSettings <-
    parseLooperSettings
      "notifier"
      (looperSetPhase defaultNotifierLooperSettings)
      (looperSetPeriod defaultNotifierLooperSettings)
  notifierSettingTimeout <-
    optional $
      setting
        [ help "How long after last hearing from this switch, nodes should consider it dead",
          reader auto,
          name "timeout",
          metavar "SECONDS"
        ]
  notifierSettingNotifySettings <- settingsParser
  pure NotifierSettings {..}

defaultNodeUrl :: NodeUrl
defaultNodeUrl = NodeUrl $ BaseUrl Https "necrork.cs-syd.eu" 443 ""

defaultNotifierLooperSettings :: LooperSettings
defaultNotifierLooperSettings =
  LooperSettings
    { looperSetEnabled = True,
      looperSetPhase = seconds 0,
      looperSetPeriod = seconds 300
    }

data NotifierEnv = NotifierEnv
  { notifierEnvSwitchName :: !SwitchName,
    notifierEnvHttpManager :: !HTTP.Manager,
    notifierEnvLooperSettings :: !LooperSettings,
    notifierEnvTimeout :: !Word32,
    notifierEnvNotifySettings :: !NotifySettings,
    -- To limit retries
    notifierEnvTokenLimiter :: !TokenLimiter,
    notifierEnvTokensPerDebit :: !Count,
    -- Peers and whether it's the first time we contact them.
    notifierEnvPeerQueue :: !(TVar (Seq (Bool, NodeUrl)))
  }

makeNotifierEnv :: (MonadIO m) => HTTP.Manager -> NotifierSettings -> m NotifierEnv
makeNotifierEnv man NotifierSettings {..} = do
  let notifierEnvSwitchName = notifierSettingSwitchName
      notifierEnvHttpManager = man
      notifierEnvLooperSettings = notifierSettingLooperSettings
      notifierEnvTimeout = fromMaybe (2 * ceiling (looperSetPeriod notifierSettingLooperSettings)) notifierSettingTimeout
      notifierEnvNotifySettings = notifierSettingNotifySettings
      notifierEnvTokensPerDebit = ceiling (looperSetPeriod notifierSettingLooperSettings)
      tokenLimitConfig =
        TokenLimitConfig
          { -- Try two requests immediately
            tokenLimitConfigInitialTokens = 2 * notifierEnvTokensPerDebit,
            -- Two requests in a row maximum
            tokenLimitConfigMaxTokens = 2 * notifierEnvTokensPerDebit,
            -- Maximum 5 requests per period
            tokenLimitConfigTokensPerSecond = 5
          }
  notifierEnvTokenLimiter <- liftIO $ makeTokenLimiter tokenLimitConfig
  notifierEnvPeerQueue <- newTVarIO (Seq.singleton (True, notifierSettingNodeUrl))
  pure NotifierEnv {..}

notifier :: (MonadUnliftIO m, MonadLogger m) => NotifierEnv -> Maybe (m void)
notifier notifierEnv =
  runLooperDef
    (\_ -> pure ()) -- No overrun handler
    looperDefFunc
    (notifierLooper notifierEnv)

notifierLooper ::
  (MonadIO m, MonadLogger m) =>
  NotifierEnv ->
  LooperDef m
notifierLooper notifierEnv =
  mkLooperDef "necrork-notifier" (notifierEnvLooperSettings notifierEnv) $
    runNotifierOnce notifierEnv

runNotifierOnce :: forall m. (MonadIO m, MonadLogger m) => NotifierEnv -> m ()
runNotifierOnce NotifierEnv {..} = go
  where
    go = do
      queue <- readTVarIO notifierEnvPeerQueue
      case Seq.viewl queue of
        -- Do nothing, the queue is empty.
        -- This should not happen because the queue can only grow, but fine
        -- if it does because this node will be considered dead.
        EmptyL -> logWarnN "No peers left. This indicates a bug in necrork-sdk."
        ((firstTime, peer) :< restPeers) -> do
          liftIO $ void $ waitDebit notifierEnvTokenLimiter notifierEnvTokensPerDebit
          (newQueue, success) <-
            if firstTime
              then do
                mNewPeers <- contactPeerTheFirstTime peer
                case mNewPeers of
                  Nothing -> pure (restPeers |> (True, peer), False)
                  Just newPeers -> pure (appendNewPeers restPeers newPeers |> (False, peer), True)
              else do
                mDone <- contactPeerToSayWeAreStillAlive peer
                pure (restPeers |> (isNothing mDone, peer), isJust mDone)
          atomically $ writeTVar notifierEnvPeerQueue newQueue
          when (not success) go
      where
        appendNewPeers ::
          Seq (Bool, NodeUrl) ->
          Set NodeUrl ->
          Seq (Bool, NodeUrl)
        appendNewPeers = foldl' appendNewPeer
        appendNewPeer ::
          Seq (Bool, NodeUrl) ->
          NodeUrl ->
          Seq (Bool, NodeUrl)
        appendNewPeer queue peer =
          if peer `elem` map snd (toList queue)
            then queue
            else queue |> (True, peer)

        makeNecrorkClientEnv :: NodeUrl -> ClientEnv
        makeNecrorkClientEnv (NodeUrl burl) =
          (mkClientEnv notifierEnvHttpManager burl)
            { makeClientRequest = \burl' request' -> do
                request <- defaultMakeClientRequest burl' request'
                let headers =
                      ( "User-Agent",
                        TE.encodeUtf8 $ T.pack $ "necrork-" <> showVersion version
                      )
                        : requestHeaders request
                pure $ request {requestHeaders = headers}
            }

        contactPeerTheFirstTime :: NodeUrl -> m (Maybe (Set NodeUrl))
        contactPeerTheFirstTime nurl = do
          let cenv = makeNecrorkClientEnv nurl
          let request =
                PutSwitchRequest
                  { putSwitchRequestTimeout = notifierEnvTimeout,
                    putSwitchRequestNotifySettings = notifierEnvNotifySettings
                  }
          logInfoN $
            T.pack $
              unwords
                [ "Configuring switch",
                  show notifierEnvSwitchName,
                  "on necrork at",
                  show (showNodeUrl nurl)
                ]
          errOrResult <-
            liftIO $
              runClientM
                (putSwitch necrorkClient notifierEnvSwitchName request)
                cenv
          case errOrResult of
            Left err -> do
              logWarnN $
                T.pack $
                  unlines
                    [ "Failed to configure necrork at",
                      showNodeUrl nurl,
                      show err
                    ]
              pure Nothing
            Right PutSwitchResponse {..} -> pure $ Just putSwitchResponsePeers
        contactPeerToSayWeAreStillAlive :: NodeUrl -> m (Maybe ())
        contactPeerToSayWeAreStillAlive nurl = do
          let cenv = makeNecrorkClientEnv nurl
          let request = PutAliveRequest
          logInfoN $
            T.pack $
              unwords
                [ "Notifying necrork at",
                  show (showNodeUrl nurl),
                  "that",
                  show notifierEnvSwitchName,
                  "is still alive."
                ]
          errOrResult <-
            liftIO $
              runClientM
                (putAlive necrorkClient notifierEnvSwitchName request)
                cenv
          case errOrResult of
            Left err -> do
              logWarnN $
                T.pack $
                  unlines
                    [ "Failed to notify necrork at",
                      showNodeUrl nurl,
                      show err
                    ]
              pure Nothing
            Right PutAliveResponse -> pure (Just ())
