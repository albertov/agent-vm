{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Git repository operations and information
module AgentVM.Git
  ( -- * Types
    GitInfo (..),

    -- * Git operations
    gitInit,
    getGitBranch,
    getGitRepoName,
    getGitInfo,
    getLastCommitDiff,
    getRepoNameFallback,
    sanitizeName,
    generateDefaultName,
  )
where

import qualified Data.Text as T
import Protolude hiding (throwIO)
import qualified Shelly as Sh
import System.FilePath (takeBaseName)
import UnliftIO (catchAny)

-- | Git repository information
newtype GitInfo = GitInfo
  { gitLastCommitDiff :: Text
  }
  deriving (Show, Eq)

-- | Initialize a git repository in the given directory
gitInit :: FilePath -> IO ()
gitInit workingDir = do
  Sh.shelly $ do
    Sh.run_ "git" ["-C", toS workingDir, "init"]
    Sh.run_ "git" ["-C", toS workingDir, "config", "user.email", "test@example.com"]
    Sh.run_ "git" ["-C", toS workingDir, "config", "user.name", "Test User"]

-- | Get current git branch name from workspace directory
getGitBranch :: FilePath -> IO (Maybe Text)
getGitBranch workingDir = do
  result <-
    catchAny
      (Just <$> Sh.shelly (Sh.run "git" ["-C", toS workingDir, "branch", "--show-current"]))
      (\_ -> pure Nothing)
  pure $ fmap (T.strip . toS) result

-- | Get git repository name from remote origin in workspace directory
getGitRepoName :: FilePath -> IO (Maybe Text)
getGitRepoName workingDir = do
  result <-
    catchAny
      (Just <$> Sh.shelly (Sh.run "git" ["-C", toS workingDir, "remote", "get-url", "origin"]))
      (\_ -> pure Nothing)
  case result of
    Nothing -> pure Nothing
    Just output -> do
      let url = T.strip (toS output)
      -- Extract repo name from various URL formats
      let repoName = case T.splitOn "/" url of
            [] -> Nothing
            parts -> case reverse parts of
              [] -> Nothing
              lastPart : _ ->
                let -- Remove .git suffix if present
                    cleaned =
                      if T.isSuffixOf ".git" lastPart
                        then T.dropEnd 4 lastPart
                        else lastPart
                 in if T.null cleaned then Nothing else Just cleaned
      pure repoName

-- | Get repository name fallback using workspace directory basename
getRepoNameFallback :: FilePath -> Text
getRepoNameFallback workingDir = toS $ takeBaseName workingDir

-- | Sanitize name by replacing dashes with underscores
sanitizeName :: Text -> Text
sanitizeName = T.replace "-" "_"

-- | Generate default VM name from git repository and branch in workspace
generateDefaultName :: FilePath -> IO Text
generateDefaultName workingDir = do
  maybeBranch <- getGitBranch workingDir
  maybeRepo <- getGitRepoName workingDir

  let branch = fromMaybe "main" maybeBranch
      repo = fromMaybe (getRepoNameFallback workingDir) maybeRepo

  pure $ sanitizeName repo <> "-" <> sanitizeName branch

-- | Get git information from workspace
getGitInfo :: FilePath -> IO (Maybe GitInfo)
getGitInfo workingDir = do
  lastCommitDiff <- getLastCommitDiff workingDir
  case lastCommitDiff of
    Nothing -> pure Nothing
    Just diffText -> pure $ Just $ GitInfo diffText

-- | Get last commit diff using git log -p
getLastCommitDiff :: FilePath -> IO (Maybe Text)
getLastCommitDiff workingDir = do
  catchAny
    ( Sh.shelly $ Sh.silently $ do
        commitHash <- Sh.run "git" ["-C", toS workingDir, "rev-parse", "HEAD"]
        diffOutput <- Sh.run "git" ["-C", toS workingDir, "log", "-p", T.strip (toS commitHash), "-1"]
        pure $ Just (T.strip (toS diffOutput))
    )
    (\_ -> pure Nothing)
