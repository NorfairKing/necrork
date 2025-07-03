{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Necrork.Cli.OptParse where

import Autodocodec
import Control.Monad.Logger
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Necrork.API
import OptEnvConf
import Path.IO
import Paths_necrork_cli (version)

getInstructions :: IO Instructions
getInstructions = runSettingsParser version "Necrork cli."

data Instructions = Instructions !Settings !Dispatch

instance HasParser Instructions where
  settingsParser =
    withConfigurableYamlConfig (runIO $ resolveFile' "necrork.yaml") $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Settings = Settings
  { settingLogLevel :: !LogLevel,
    settingPeers :: !(NonEmpty NodeUrl)
  }
  deriving (Show)

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings = do
  settingLogLevel <- settingsParser
  settingPeers <-
    choice
      [ someNonEmpty $
          setting
            [ help "Peers to connect to",
              option,
              long "peer",
              reader $ maybeReader parseNodeUrl,
              metavar "URL"
            ],
        setting
          [ help "Comma-separated list of peers to connect to.",
            conf "peers",
            env "PEERS",
            reader $ commaSeparated $ maybeReader parseNodeUrl,
            metavar "URL"
          ]
      ]
  pure Settings {..}

data Dispatch
  = DispatchNotify !SwitchName
  | DispatchDelete !SwitchName
  deriving (Show)

instance HasParser Dispatch where
  settingsParser =
    commands
      [ command "notify" "Notify that this switch is alive" $
          DispatchNotify
            <$> setting
              [ help "Name of the switch to mark as alive.",
                argument,
                reader $ maybeReader $ mkSwitchName . T.pack,
                metavar "NAME"
              ],
        command "delete" "Delete a switch" $
          DispatchDelete
            <$> setting
              [ help "Name of the switch to delete.",
                argument,
                reader $ maybeReader $ mkSwitchName . T.pack,
                metavar "NAME"
              ]
      ]

instance HasParser LogLevel where
  settingsParser =
    setting
      [ help "Minimal severity of log messages",
        reader logLevelReader,
        value LevelInfo,
        name "log-level",
        metavar "LOG_LEVEL",
        example "Info"
      ]
    where
      logLevelReader = eitherReader $ \case
        "Debug" -> Right LevelDebug
        "Info" -> Right LevelInfo
        "Warn" -> Right LevelWarn
        "Error" -> Right LevelError
        s -> Left $ "Unknown LogLevel: " <> show s

instance HasCodec LogLevel where
  codec =
    stringConstCodec
      [ (LevelDebug, "Debug"),
        (LevelInfo, "Info"),
        (LevelWarn, "Warn"),
        (LevelError, "Error")
      ]
