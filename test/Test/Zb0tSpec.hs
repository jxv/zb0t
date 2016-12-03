{-# LANGUAGE QuasiQuotes #-}
module Test.Zb0tSpec (spec) where

import Pregame
import Test.Hspec
import Zb0t
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

mkFixture "Fixture" [ts|Log|]

spec :: Spec
spec = do
  describe "main" $ do
    it "should log the starting message" $ do
      calls <- logTestFixtureT main Fixture
        { _logInfo = \msg -> do
            log "logInfo"
            lift $ msg `shouldBe` startMessage
        }
      calls `shouldBe` ["logInfo"]
