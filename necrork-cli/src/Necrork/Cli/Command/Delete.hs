{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Necrork.Cli.Command.Delete (runNecrorkDelete) where

import Control.Monad
import Control.Monad.Logger
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Necrork.Cli.Env
import Necrork.Client
import System.Exit
import UnliftIO

runNecrorkDelete :: SwitchName -> CliM ()
runNecrorkDelete switchName = do
  errsOrResponses <-
    forEachPeer $
      deleteSwitch necrorkClient switchName DeleteSwitchRequest {..}

  let printResults =
        mapM_
          ( \(nurl, DeleteSwitchResponse {}) ->
              logInfoN $
                T.pack $
                  unwords
                    [ "Deleted switch",
                      show (unSwitchName switchName),
                      "from peer",
                      show (showBaseUrl (unNodeUrl nurl)),
                      "recursively."
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
          logErrorN "Failed to delete the switch at least one peer."
          liftIO exitFailure
        else logInfoN "Deleted the switch at least one peer successfully."
