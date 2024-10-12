{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Necrork
  ( NotifierSettings (..),
    makeNotifierSettings,
    withNotifier,
    withMNotifier,
    defaultBaseUrl,
    defaultNotifierLooperSettings,
    NotifierEnv (..),
    makeNotifierEnv,
    notifier,
    notifierLooper,
    runNotifierOnce,
    module Necrork.Client,
  )
where

import Autodocodec
import Control.Concurrent.TokenLimiter.Concurrent
import Control.Monad
import Control.Monad.Logger
import Data.Foldable
import Data.Maybe
import Data.Sequence (Seq, ViewL (..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Text as T
import Data.Void
import Data.Word
import Looper
import Necrork.Client
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import OptEnvConf
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
    notifierSettingBaseUrl :: !BaseUrl,
    notifierSettingLooperSettings :: !LooperSettings,
    notifierSettingTimeout :: !(Maybe Word32),
    notifierSettingNotifySettings :: !NotifySettings
  }
  deriving (Show)

makeNotifierSettings :: SwitchName -> NotifySettings -> NotifierSettings
makeNotifierSettings notifierSettingSwitchName notifierSettingNotifySettings =
  let notifierSettingBaseUrl = defaultBaseUrl
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
  notifierSettingBaseUrl <-
    setting
      [ help "Base url of the necrork server",
        reader $ maybeReader parseBaseUrl,
        option,
        long "url",
        confWith "url" $
          bimapCodec
            ( \s -> case parseBaseUrl s of
                Nothing -> Left $ "Could not parse BaseUrl: " <> s
                Just burl -> Right burl
            )
            showBaseUrl
            codec,
        env "URL",
        metavar "URL",
        valueWithShown defaultBaseUrl (showBaseUrl defaultBaseUrl)
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

defaultBaseUrl :: BaseUrl
defaultBaseUrl = BaseUrl Https "necrork.cs-syd.eu" 443 ""

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
    -- Peers and whether it's the first time we contact them.
    notifierEnvPeerQueue :: !(TVar (Seq (Bool, BaseUrl)))
  }

makeNotifierEnv :: (MonadIO m) => HTTP.Manager -> NotifierSettings -> m NotifierEnv
makeNotifierEnv man NotifierSettings {..} = do
  let notifierEnvSwitchName = notifierSettingSwitchName
      notifierEnvHttpManager = man
      notifierEnvLooperSettings = notifierSettingLooperSettings
      notifierEnvTimeout = fromMaybe (2 * ceiling (looperSetPeriod notifierSettingLooperSettings)) notifierSettingTimeout
      notifierEnvNotifySettings = notifierSettingNotifySettings
  notifierEnvTokenLimiter <-
    liftIO $
      makeTokenLimiter
        TokenLimitConfig
          { -- Try two requests immediately
            tokenLimitConfigInitialTokens = 20,
            tokenLimitConfigMaxTokens = 20,
            -- Try a total of 10 requests per period
            tokenLimitConfigTokensPerSecond =
              round $
                100 / looperSetPeriod notifierSettingLooperSettings
          }
  notifierEnvPeerQueue <- newTVarIO (Seq.singleton (True, notifierSettingBaseUrl))
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
          liftIO $ void $ waitDebit notifierEnvTokenLimiter 10
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
          Seq (Bool, BaseUrl) ->
          Set BaseUrl ->
          Seq (Bool, BaseUrl)
        appendNewPeers = foldl' appendNewPeer
        appendNewPeer ::
          Seq (Bool, BaseUrl) ->
          BaseUrl ->
          Seq (Bool, BaseUrl)
        appendNewPeer queue peer =
          if peer `elem` map snd (toList queue)
            then queue
            else queue |> (True, peer)

        contactPeerTheFirstTime :: BaseUrl -> m (Maybe (Set BaseUrl))
        contactPeerTheFirstTime burl = do
          let cenv = mkClientEnv notifierEnvHttpManager burl
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
                  show (showBaseUrl burl)
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
                      showBaseUrl (baseUrl cenv),
                      show err
                    ]
              pure Nothing
            Right PutSwitchResponse {..} -> do
              pure $ Just putSwitchResponsePeers
        contactPeerToSayWeAreStillAlive :: BaseUrl -> m (Maybe ())
        contactPeerToSayWeAreStillAlive burl = do
          let cenv = mkClientEnv notifierEnvHttpManager burl
          let request =
                PutAliveRequest
                  {
                  }
          logInfoN $
            T.pack $
              unwords
                [ "Notifying necrork at",
                  show (showBaseUrl burl),
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
                      showBaseUrl (baseUrl cenv),
                      show err
                    ]
              pure Nothing
            Right PutAliveResponse -> pure (Just ())
