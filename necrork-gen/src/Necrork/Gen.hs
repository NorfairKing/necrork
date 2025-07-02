{-# OPTIONS_GHC -fno-warn-orphans #-}

module Necrork.Gen where

import Control.Monad
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Necrork.API
import Servant.Client
import Test.QuickCheck

instance GenValid NodeUrl where
  genValid =
    ( do
        scheme <- elements [Https, Http]
        hostPart <- NE.toList <$> genNonEmptyOf (choose ('a', 'z'))
        tld <- replicateM 3 (choose ('a', 'z'))
        let host = hostPart ++ "." ++ tld
        port <- choose (0, 65535)
        path <- genListOf (oneof [choose ('a', 'z'), pure '/'])
        let burl = NodeUrl $ BaseUrl scheme host port path
        pure $ showNodeUrl burl
    )
      `suchThatMap` parseNodeUrl
      `suchThat` isValid
  shrinkValid _ = [] -- Fine for now

instance GenValid Timestamp where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SwitchName where
  genValid = SwitchName . T.pack . NE.toList <$> genValid

instance GenValid TimestampTuple where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PostSyncRequest where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PostSyncResponse where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid NotifyIntraySettings where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid NotifySettings where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PutSwitchRequest where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PutSwitchResponse where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DeleteSwitchRequest where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DeleteSwitchResponse where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PutAliveRequest where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PutAliveResponse where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid GetAliveResponse where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
