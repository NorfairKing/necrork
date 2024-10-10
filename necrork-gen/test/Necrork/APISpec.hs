{-# LANGUAGE TypeApplications #-}

module Necrork.APISpec (spec) where

import Necrork.API
import Necrork.Gen ()
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
