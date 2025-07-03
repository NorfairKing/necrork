{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Necrork.Cli.Command.Notify (runNecrorkNotify) where

import Control.Monad
import Control.Monad.Logger
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Necrork.Cli.Env
import Necrork.Client
import System.Exit
import UnliftIO

runNecrorkNotify :: SwitchName -> CliM ()
runNecrorkNotify switchName = do
  errsOrResponses <-
    forEachPeer $
      putAlive necrorkClient switchName PutAliveRequest {..}

  let printResults =
        mapM_
          ( \(nurl, PutAliveResponse {}) ->
              logInfoN $
                T.pack $
                  unwords
                    [ "Notified peer",
                      show (showBaseUrl (unNodeUrl nurl)),
                      "that",
                      show (unSwitchName switchName),
                      "is alive."
                    ]
          )
  case errsOrResponses of
    Right results -> printResults $ NE.toList results
    Left (errs, results) -> do
      printResults results
      forM_ errs $ \(peer, err) ->
        logWarnN $
          T.pack $
            concat
              [ "Error notifying peer: ",
                show (showBaseUrl (unNodeUrl peer)),
                "\n",
                show err
              ]
      if null results
        then do
          logErrorN "Failed to notify at least one peer."
          liftIO exitFailure
        else logInfoN "Notified at least one peer successfully."
