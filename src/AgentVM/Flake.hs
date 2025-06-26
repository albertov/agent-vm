{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Flake reference utilities for both compile-time and runtime use
module AgentVM.Flake
  ( getFlakeReference,
  )
where

import qualified Data.Text as T
import Protolude
import System.Process (readProcess)

getFlakeReference :: (MonadIO m) => m Text
getFlakeReference = liftIO $ do
  repoRoot <- T.strip . toS <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""
  branch <- T.strip . toS <$> readProcess "git" ["branch", "--show-current"] ""
  pure $ "git+file://" <> repoRoot <> "?ref=" <> branch
