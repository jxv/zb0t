{-# LANGUAGE QuasiQuotes #-}
module Test.Zb0t.ControlSpec (spec) where

import Pregame
import Test.Hspec
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

import Zb0t.Control
import Zb0t.Logger

mkFixture "Fixture" [ts|Logger|]

spec :: Spec
spec =
  describe "main" $
    it "should log the starting message" $ do
      calls <- logTestFixtureT main Fixture
        { _logInfo = \msg -> do
            log "logInfo"
            lift $ msg `shouldBe` startMessage
        }
      calls `shouldBe` ["logInfo"]
