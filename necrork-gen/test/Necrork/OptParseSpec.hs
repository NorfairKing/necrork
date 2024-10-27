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
  it "parses no args as Nothing" $ parserArgsTest p mempty Nothing
  it "parses no env as Nothing" $ parserEnvTest p mempty Nothing
  it "parses no conf as Nothing" $ parserConfTest p mempty Nothing
