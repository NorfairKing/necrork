{-# LANGUAGE TypeApplications #-}

module Necrork.OptParseSpec (spec) where

import Necrork
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  let p = withoutConfig $ optional $ subSettings @NotifierSettings "necrork"
  parserLintSpec p
  goldenParserReferenceDocumentationSpec p "documentation.txt" "necrork"
  goldenParserNixOptionsSpec p "options.nix"
