# SOCAT Terminal Emulation Testing Plan

## Overview

This document outlines the implementation plan for using `socat` to create virtual terminal pairs that enable testing of the `interactWith` function's escape sequence detection capabilities.

## Background

The `interactWith` function in `AgentVM.Interactive` implements:
- Terminal raw mode setup
- Bidirectional I/O relay between stdin/stdout and subprocess
- Escape sequence detection (Ctrl-W + 'x' to exit)
- State management for escape mode

Currently, the test is pending because escape sequence detection requires actual terminal emulation to function properly.

## Socat Fundamentals

### What socat provides
- Creates **pseudo-terminal (PTY) pairs**
- One end acts as master (for our test code)
- Other end acts as slave (for the subprocess)
- Both ends behave like real terminals with proper `isatty()` responses

### Basic socat command structure
```bash
socat -d -d pty,raw,echo=0,link=/tmp/master pty,raw,echo=0,link=/tmp/slave
```

**Parameters explained:**
- `-d -d`: Debug mode (shows PTY paths)
- `pty`: Create pseudo-terminal
- `raw`: Minimal processing (pass-through mode)
- `echo=0`: Disable echo to prevent duplication
- `link=<path>`: Create predictable symlink to PTY device

## Implementation Strategy

### Phase 1: Socat Integration Infrastructure

#### 1.1 Add Socat Test Utilities

Create `test-utils/AgentVM/SocatTestUtils.hs`:

```haskell
module AgentVM.SocatTestUtils
  ( withSocatTerminal
  , SocatTerminal(..)
  , SocatConfig(..)
  , defaultSocatConfig
  , socatAvailable
  ) where

import Control.Exception (bracket)
import System.Process
import System.IO
import System.Directory (doesFileExist, removeFile)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import Protolude

data SocatConfig = SocatConfig
  { socatDebug :: Bool
  , socatTimeout :: Int  -- seconds
  , masterLink :: FilePath
  , slaveLink :: FilePath
  , socatExtraArgs :: [Text]
  }

data SocatTerminal = SocatTerminal
  { masterPath :: FilePath
  , slavePath :: FilePath
  , masterHandle :: Handle
  , slaveHandle :: Handle
  , socatProcess :: ProcessHandle
  }

defaultSocatConfig :: SocatConfig
defaultSocatConfig = SocatConfig
  { socatDebug = False
  , socatTimeout = 10
  , masterLink = "/tmp/test-master"
  , slaveLink = "/tmp/test-slave"
  , socatExtraArgs = []
  }

-- Check if socat is available on system
socatAvailable :: IO Bool
socatAvailable = do
  result <- try $ readProcess "which" ["socat"] ""
  return $ case result of
    Left (_ :: SomeException) -> False
    Right output -> not (null output)

-- Main function for creating socat terminal pairs
withSocatTerminal :: SocatConfig -> (SocatTerminal -> IO a) -> IO a
withSocatTerminal config action = bracket
  (startSocat config)
  cleanupSocat
  action
```

#### 1.2 Socat Process Management

```haskell
-- Start socat process and wait for PTY creation
startSocat :: SocatConfig -> IO SocatTerminal
startSocat config = do
  -- Clean up any existing symlinks
  cleanupLinks config

  let args = buildSocatArgs config
  (_, Just hout, Just herr, ph) <- createProcess (proc "socat" args)
    { std_out = CreatePipe
    , std_err = CreatePipe
    }

  -- Parse socat output to extract PTY paths
  (masterPath, slavePath) <- parseSocatOutput herr

  -- Wait for symlinks to be created
  waitForPTYs (masterLink config) (slaveLink config)

  -- Open handles to the PTY devices
  masterHandle <- openFile (masterLink config) ReadWriteMode
  slaveHandle <- openFile (slaveLink config) ReadWriteMode

  -- Configure handles for immediate I/O
  hSetBuffering masterHandle NoBuffering
  hSetBuffering slaveHandle NoBuffering

  return $ SocatTerminal
    { masterPath = masterPath
    , slavePath = slavePath
    , masterHandle = masterHandle
    , slaveHandle = slaveHandle
    , socatProcess = ph
    }

buildSocatArgs :: SocatConfig -> [String]
buildSocatArgs config = debugArgs ++ ptyArgs ++ extraArgs
  where
    debugArgs = if socatDebug config then ["-d", "-d"] else []
    ptyArgs =
      [ "pty,raw,echo=0,link=" ++ masterLink config
      , "pty,raw,echo=0,link=" ++ slaveLink config
      ]
    extraArgs = map toS (socatExtraArgs config)

cleanupLinks :: SocatConfig -> IO ()
cleanupLinks config = do
  let links = [masterLink config, slaveLink config]
  forM_ links $ \link -> do
    exists <- doesFileExist link
    when exists $ removeFile link

cleanupSocat :: SocatTerminal -> IO ()
cleanupSocat terminal = do
  -- Close handles
  hClose (masterHandle terminal)
  hClose (slaveHandle terminal)

  -- Terminate socat process
  terminateProcess (socatProcess terminal)

  -- Clean up symlinks (optional, they'll be cleaned by socat)
  -- removeFileIfExists (masterLink config)
  -- removeFileIfExists (slaveLink config)
```

#### 1.3 PTY Path Detection

```haskell
-- Parse socat debug output to extract actual PTY device paths
parseSocatOutput :: Handle -> IO (FilePath, FilePath)
parseSocatOutput herr = do
  content <- hGetContents herr
  let lines = T.lines (toS content)

  case extractPTYPaths lines of
    Just (master, slave) -> return (master, slave)
    Nothing -> throwIO $ userError "Failed to parse socat PTY paths"

extractPTYPaths :: [Text] -> Maybe (FilePath, FilePath)
extractPTYPaths lines = do
  let ptyLines = filter ("PTY is" `T.isInfixOf`) lines
  case ptyLines of
    [line1, line2] -> do
      path1 <- extractPath line1
      path2 <- extractPath line2
      -- First path is typically master, second is slave
      return (path1, path2)
    _ -> Nothing
  where
    extractPath line = case T.words line of
      ws | length ws >= 4 -> Just (toS (last ws))
      _ -> Nothing

-- Wait for PTY symlinks to be created
waitForPTYs :: FilePath -> FilePath -> IO ()
waitForPTYs masterLink slaveLink = do
  let maxWait = 50 -- 5 seconds total
  waitForPTYs' masterLink slaveLink 0 maxWait

waitForPTYs' :: FilePath -> FilePath -> Int -> Int -> IO ()
waitForPTYs' masterLink slaveLink current maxWait
  | current >= maxWait = throwIO $ userError "Timeout waiting for PTY creation"
  | otherwise = do
      masterExists <- doesFileExist masterLink
      slaveExists <- doesFileExist slaveLink
      if masterExists && slaveExists
        then return ()
        else do
          threadDelay 100000 -- 100ms
          waitForPTYs' masterLink slaveLink (current + 1) maxWait
```

### Phase 2: Test Implementation

#### 2.1 Basic Socat Integration Test

```haskell
-- Add to test/AgentVM/StreamingProcessSpec.hs

describe "escape sequence detection with terminal emulation" $ do
  it "should handle escape sequence via socat terminal" $ do
    available <- socatAvailable
    if not available
      then pendingWith "socat not available on system"
      else do
        let config = defaultSocatConfig
              { masterLink = "/tmp/test-escape-master"
              , slaveLink = "/tmp/test-escape-slave"
              , socatDebug = True
              }

        withSocatTerminal config $ \terminal -> do
          -- Test escape sequence detection
          testEscapeSequence terminal

testEscapeSequence :: SocatTerminal -> IO ()
testEscapeSequence terminal = do
  -- Create a simple subprocess that cat's input
  let subprocess = Process "cat" []

  withInteractive 10 subprocess $ \interactive -> do
    -- Run interactWith in background thread
    escapeResult <- newEmptyMVar

    async $ do
      result <- try $ interactWith
        (slaveHandle terminal)    -- stdin
        (slaveHandle terminal)    -- stdout
        (slaveHandle terminal)    -- stderr
        "ctrl-w"                  -- escape key
        interactive
      putMVar escapeResult result

    -- Send escape sequence through master handle
    hPutStr (masterHandle terminal) [chr 23] -- Ctrl-W
    hFlush (masterHandle terminal)

    threadDelay 100000 -- 100ms

    hPutStr (masterHandle terminal) "x"      -- Exit command
    hFlush (masterHandle terminal)

    -- Wait for interactWith to exit
    result <- timeout 2000000 (takeMVar escapeResult) -- 2 second timeout

    case result of
      Nothing -> expectationFailure "interactWith did not exit within timeout"
      Just (Left e) -> expectationFailure $ "interactWith failed: " ++ show e
      Just (Right _) -> return () -- Success!
```

#### 2.2 Advanced Escape Sequence Tests

```haskell
describe "advanced escape sequence scenarios" $ do
  it "should handle partial escape sequences" $ do
    -- Test Ctrl-W followed by non-'x' character
    withSocatTest $ \terminal -> do
      sendToTerminal terminal [chr 23, chr 97] -- Ctrl-W + 'a'
      -- Should continue running, not exit

  it "should handle multiple rapid escape attempts" $ do
    -- Test rapid Ctrl-W, Ctrl-W, x sequence
    withSocatTest $ \terminal -> do
      sendToTerminal terminal [chr 23, chr 23, chr 120] -- Ctrl-W, Ctrl-W, x
      -- Should exit properly

  it "should handle escape sequence with input buffer" $ do
    -- Test escape sequence mixed with normal input
    withSocatTest $ \terminal -> do
      sendToTerminal terminal "normal input\n"
      sendToTerminal terminal [chr 23, chr 120] -- Ctrl-W + x
      -- Should process input then exit

-- Helper function for common test patterns
withSocatTest :: (SocatTerminal -> IO ()) -> IO ()
withSocatTest testAction = do
  available <- socatAvailable
  if not available
    then pendingWith "socat not available on system"
    else do
      let config = defaultSocatConfig { socatDebug = False }
      withSocatTerminal config testAction

sendToTerminal :: SocatTerminal -> [Char] -> IO ()
sendToTerminal terminal chars = do
  hPutStr (masterHandle terminal) chars
  hFlush (masterHandle terminal)
```

### Phase 3: CI/CD Integration

#### 3.1 Docker Support

Add to project Dockerfile or CI config:
```dockerfile
# Ensure socat is available in test environment
RUN apt-get update && apt-get install -y socat
```

#### 3.2 Cabal Test Configuration

Update `agent-vm.cabal`:
```cabal
test-suite spec
    -- ... existing config ...
    build-depends:    base
                    , protolude
                    , agent-vm
                    , agent-vm:test-utils  -- Add test-utils dependency
                    , hspec
                    , unliftio
                    , bytestring
                    , network
                    , unbounded-delays
                    , text
                    , QuickCheck
                    , directory
                    , containers
                    , process              -- Add for socat integration
                    , async                -- Add for concurrent testing

-- Update test-utils library
library test-utils
    import:           warnings
    exposed-modules:  AgentVM.TestUtils
                    , AgentVM.SocatTestUtils  -- Add socat utilities
    build-depends:    base
                    , protolude
                    , agent-vm
                    , unliftio
                    , retry
                    , plow-log
                    , plow-log-async
                    , filepath
                    , microlens
                    , generic-lens
                    , HUnit
                    , process              -- Add for socat
                    , directory            -- Add for file operations
```

#### 3.3 Graceful Degradation

```haskell
-- Modify the pending test to conditionally run
it "escape sequence detection requires interactWith for full functionality" $ do
  available <- socatAvailable
  if available
    then do
      -- Run full socat-based test
      testEscapeSequenceWithSocat
    else do
      -- Fall back to basic logic test
      pendingWith "socat not available - install socat for full terminal emulation testing"

      -- Still test what we can without terminal emulation
      parseEscapeKey "ctrl-w" `shouldBe` 23
      let escapeSequence = BS.pack [23, 120]
      BS.unpack escapeSequence `shouldBe` [23, 120]
```

## Testing Strategy

### Test Scenarios to Cover

1. **Basic Escape Sequence**: Ctrl-W + x should exit
2. **Partial Sequences**: Ctrl-W + non-x should continue
3. **Rapid Sequences**: Multiple Ctrl-W presses
4. **Mixed Input**: Normal text + escape sequences
5. **Edge Cases**: Empty input, EOF, process termination
6. **Concurrent I/O**: Input/output happening during escape detection

### Error Handling

1. **Socat unavailable**: Graceful degradation with pending test
2. **PTY creation failure**: Clear error messages
3. **Timeout scenarios**: Proper cleanup and reporting
4. **Process cleanup**: Ensure no orphaned socat processes

## Benefits of This Approach

1. **No additional Haskell dependencies** - uses system socat
2. **Tests actual production code paths** - same PTY handling as runtime
3. **Portable** - socat available on most Unix systems
4. **Isolated** - each test gets fresh PTY pair
5. **Debuggable** - socat debug output shows PTY assignments

## Limitations and Mitigations

1. **External dependency**: Mitigated by graceful degradation
2. **Platform-specific**: Focus on Unix/Linux (primary target)
3. **Timing-sensitive**: Use appropriate timeouts and delays
4. **Resource cleanup**: Proper bracket usage and cleanup

## Future Considerations

This socat-based approach provides immediate testing capability while maintaining the option to migrate to pure Haskell solutions (like `posix-pty`) in the future for better integration and reduced external dependencies.
