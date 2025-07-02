{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Necrork.Cli.Command.Delete (runNecrorkDelete) where

import Control.Monad
import Data.Either
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version (showVersion)
import Necrork.Cli.OptParse
import Necrork.Client
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Paths_necrork_cli (version)
import System.Exit
import UnliftIO

runNecrorkDelete :: Settings -> SwitchName -> IO ()
runNecrorkDelete Settings {..} switchName = do
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
  man <- liftIO $ HTTP.newManager managerSets
  responses <- forConcurrently (NE.toList settingPeers) $ \nurl -> do
    let cenv = mkClientEnv man (unNodeUrl nurl)
    flip runClientM cenv $
      deleteSwitch necrorkClient switchName DeleteSwitchRequest {..}

  let (errs, _) = partitionEithers responses
  when (not (null errs)) $ do
    putStrLn "Errors occurred while deleting switch:"
    die $ unlines $ map show errs
