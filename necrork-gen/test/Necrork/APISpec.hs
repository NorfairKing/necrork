{-# LANGUAGE TypeApplications #-}

module Necrork.APISpec (spec) where

import Necrork.API
import Necrork.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Persist

spec :: Spec
spec = do
  genValidSpec @Timestamp
  genValidSpec @NodeUrl
  persistSpec @NodeUrl
  genValidSpec @SwitchName
  genValidSpec @PostSyncRequest
  genValidSpec @PostSyncResponse
  genValidSpec @NotifyIntraySettings
  genValidSpec @NotifySettings
  genValidSpec @PutSwitchRequest
  genValidSpec @GetAliveResponse
