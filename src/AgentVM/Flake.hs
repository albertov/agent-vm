{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Flake reference utilities for both compile-time and runtime use
module AgentVM.Flake
  ( getFlakeReference,
  )
where

import qualified Data.Text as T
import Protolude
import qualified Shelly as Sh

getFlakeReference :: (MonadIO m) => Maybe FilePath -> m Text
getFlakeReference mDir = Sh.shelly $ Sh.silently $ do
  dir <- case mDir of
    Just dir ->
      Sh.test_d dir >>= \case
        True -> pure dir
        False -> Sh.pwd
    Nothing -> Sh.pwd
  repoRoot <- T.strip <$> Sh.run "git" ["-C", toS dir, "rev-parse", "--show-toplevel"]
  branch <- T.strip <$> Sh.run "git" ["-C", toS dir, "branch", "--show-current"]
  pure $ "git+file://" <> repoRoot <> "?ref=" <> branch
