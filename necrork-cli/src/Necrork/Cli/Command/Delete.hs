{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Necrork.Cli.Command.Delete (runNecrorkDelete) where

import Control.Monad
import Data.Either
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version (showVersion)
import Necrork.Cli.Env
import Necrork.Cli.OptParse
import Necrork.Client
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Paths_necrork_cli (version)
import System.Exit
import UnliftIO

runNecrorkDelete :: SwitchName -> CliM ()
runNecrorkDelete switchName = do
  errsOrResponses <-
    forEachPeer $
      deleteSwitch necrorkClient switchName DeleteSwitchRequest {..}

  let printResults =
        liftIO
          . putStr
          . unlines
          . map
            ( \(nurl, DeleteSwitchResponse {}) ->
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
      liftIO $ do
        putStrLn "Errors occurred while notifying peers:"
        die $ unlines $ map show $ NE.toList errs
