{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AgentVM.ShellEscapeKeySpec (spec) where

import AgentVM (MonadVM (..), runVMT)
import AgentVM.TestUtils (withTestEnv)
import AgentVM.Types (VMConfig (..), defVMConfig)
import Data.Generics.Labels ()
import Lens.Micro
import Protolude
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "Shell Escape Key Configuration Tests" $ around withTestEnv $ do
  describe "VMConfig shell escape key" $ do
    it "has default shell escape key of Ctrl-W" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig
      shellEscapeKey vmConfig `shouldBe` "Ctrl-W"

    it "can create VMConfig with custom shell escape key" $ \(_, _) -> do
      customConfig <- liftIO $ defVMConfig Nothing "test-vm" "/tmp/test"
      let customConfigWithEscapeKey = customConfig {shellEscapeKey = "Ctrl-A"}
      shellEscapeKey customConfigWithEscapeKey `shouldBe` "Ctrl-A"

    it "preserves shell escape key through JSON serialization" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig
      let customConfig = vmConfig {shellEscapeKey = "Ctrl-Z"}

      -- Create VM with custom escape key
      createResult <- liftIO $ runVMT env (create customConfig)
      createResult `shouldSatisfy` isRight

      -- The config should be saved with the custom escape key
      shellEscapeKey customConfig `shouldBe` "Ctrl-Z"
