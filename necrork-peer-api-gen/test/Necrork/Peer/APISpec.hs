{-# LANGUAGE TypeApplications #-}

module Necrork.Peer.APISpec (spec) where

import Necrork.Peer.API
import Necrork.Peer.API.Gen ()
import Servant.Client
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Persist

spec :: Spec
spec = do
  persistSpec @BaseUrl
  genValidSpec @Timestamp
  genValidSpec @SwitchName
  genValidSpec @PostSyncRequest
  genValidSpec @PostSyncResponse
  genValidSpec @NotifyIntraySettings
  genValidSpec @NotifySettings
  genValidSpec @PutSwitchRequest
  genValidSpec @PutSwitchResponse
  genValidSpec @PutAliveRequest
  genValidSpec @PutAliveResponse
  genValidSpec @GetAliveResponse
