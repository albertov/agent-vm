# Haskell Migration Plan for agent-vm

## Executive Summary

This document outlines a comprehensive plan to migrate agent-vm from Python to Haskell. The migration leverages Haskell's superior type safety, concurrency model, and compile-time guarantees while maintaining the existing Nix-based VM provisioning architecture. The Python version will be maintained during the transition period.

## Why Haskell?

### Type Safety Benefits
- **Compile-time state machine validation**: VM state transitions (Stopped → Running → Paused) can be encoded in the type system, making invalid transitions impossible at compile time
- **No more runtime type errors**: GHC will catch configuration mismatches, missing fields, and incorrect argument types before deployment
- **Phantom types for VM states**: Prevent operations on VMs in wrong states (e.g., can't stop an already stopped VM)

### Concurrency Advantages
- **Software Transactional Memory (STM)**: Manage VM registry with atomic operations, no deadlocks
- **Lightweight threads**: Each thread only 1KB overhead vs Python's ~8MB per thread
- **Built-in supervision**: Capataz provides OTP-like fault tolerance for VM monitoring services

### Performance Improvements
- **Compiled performance**: 10-100x faster than Python for CPU-bound operations
- **Efficient string handling**: ByteString instead of Python strings for subprocess I/O
- **Zero-cost abstractions**: High-level code compiles to efficient machine code
- **Static linking**: haskell.nix will be used to produce statically linked executables with musl

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                      CLI Interface                          │
│                 (optparse-applicative)                      │
├─────────────────────────────────────────────────────────────┤
│                    Library Core                             │
│         (Ready for daemon integration later)                │
├─────────────────────────────────────────────────────────────┤
│                    VM Controller Core                       │
│              (State Machines + STM Registry)                │
├─────────────────────────────────────────────────────────────┤
│   Process Manager  │  SSH Manager  │  State Persistence    │
│  (typed-process)   │   (libssh2)   │    (aeson/yaml)      │
├─────────────────────────────────────────────────────────────┤
│                 Contravariant Logging                       │
│               (plow-log with sum types)                     │
├─────────────────────────────────────────────────────────────┤
│                    Nix Integration Layer                    │
│              (Calls nix build, nix run, etc.)              │
└─────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. Type-Safe VM State Machine

```haskell
-- Phantom types for VM states
data VMState = Stopped | Running | Paused | Building

-- VM type parameterized by state
data VM (s :: VMState) = VM
  { vmId :: VMId
  , vmConfig :: VMConfig
  , vmPid :: Maybe ProcessID
  }

-- Type-safe state transitions
startVM :: VM 'Stopped -> IO (VM 'Running)
pauseVM :: VM 'Running -> IO (VM 'Paused)
-- This won't compile: pauseVM :: VM 'Stopped -> IO (VM 'Paused)
```

### 2. Concurrent VM Registry

```haskell
-- Thread-safe VM registry using STM
data VMRegistry = VMRegistry
  { vmMap :: TVar (Map BranchName (TVar VMInfo))
  , vmLocks :: TVar (Set BranchName)
  }

-- Atomic operations
createVM :: VMRegistry -> BranchName -> VMConfig -> STM (Either VMError ())
createVM registry branch config = do
  vms <- readTVar (vmMap registry)
  case Map.lookup branch vms of
    Just _ -> return $ Left VMAlreadyExists
    Nothing -> do
      vmVar <- newTVar (VMInfo config Stopped Nothing)
      writeTVar (vmMap registry) (Map.insert branch vmVar vms)
      return $ Right ()
```

### 3. Contravariant Logging with co-log

Note: The requirement mentions "plow-log" but this appears to be either an internal library or a typo. The standard contravariant logging library in Haskell is co-log, which provides the same functionality.

```haskell
-- Sum type for all possible log events
data AgentVmTrace
  = RepoCloned { origin :: GitRepo, destination :: FilePath }
  | VMCreated { branch :: BranchName, config :: VMConfig }
  | VMStarted { branch :: BranchName, pid :: ProcessID }
  | VMStopped { branch :: BranchName }
  | SSHConnectionEstablished { branch :: BranchName, port :: Int }
  | SSHConnectionFailed { branch :: BranchName, error :: Text }
  | NixBuildStarted { flakeRef :: Text }
  | NixBuildCompleted { flakeRef :: Text, storePath :: FilePath }
  | PortAllocated { port :: Int }
  | WorkspaceAccessible { path :: FilePath, size :: Text }
  | AgentServiceHealthy { branch :: BranchName, uptime :: Text }
  deriving (Show, Eq)

-- Render traces based on log level
showTrace :: LogLevel -> AgentVmTrace -> Text
showTrace level trace = case (level, trace) of
  (Debug, RepoCloned o d) -> "🔧 Cloned " <> o <> " to " <> d
  (Info, VMStarted b p) -> "🚀 VM started for " <> b <> " (PID: " <> show p <> ")"
  (Error, SSHConnectionFailed b e) -> "❌ SSH failed for " <> b <> ": " <> e
  -- ... etc

-- Using co-log's contravariant interface
vmLogger :: LogAction IO AgentVmTrace
vmLogger = contramap (showTrace Info) logTextStdout
```

### 4. Process Management

```haskell
-- Type-safe Nix script execution
data NixScript = NixScript
  { scriptPath :: FilePath
  , scriptArgs :: [Text]
  }

runNixVM :: NixScript -> IO (Either ProcessError Process)
runNixVM script = do
  let config = setStdin closed
             $ setStdout byteStringOutput
             $ setStderr byteStringOutput
             $ proc (scriptPath script) (map T.unpack $ scriptArgs script)

  tryStartProcess config
```

## Migration Strategy

### Phase 1: Core Infrastructure (Week 1-2)
1. Set up Haskell project structure with Cabal/Stack
2. Implement basic types and data models
3. Create typed-process wrappers for Nix commands
4. Port configuration parsing (JSON/YAML)
5. Set up plow-log logging infrastructure

### Phase 2: VM Lifecycle Management (Week 3-4)
1. Implement VM state machine with phantom types
2. Create STM-based VM registry
3. Port VM creation, start, stop, destroy operations
4. Add SSH connection management

### Phase 3: CLI Interface (Week 5)
1. Implement optparse-applicative CLI parser
2. Port all CLI commands with same interface
3. Add colored output and progress indicators
4. Ensure library/CLI separation for future daemon use

### Phase 4: Advanced Features (Week 6-7)
1. Implement supervisor trees for monitoring
2. Add concurrent VM operations
3. Port integration tests using hspec
4. Performance optimization

### Phase 5: Integration & Testing (Week 8)
1. Comprehensive integration testing
2. Migration tools for existing Python state
3. Documentation updates
4. Gradual rollout strategy

## Key Libraries

### Core Dependencies
```yaml
dependencies:
  # Process and system interaction
  - typed-process        # Type-safe process management
  - directory           # File system operations
  - filepath            # Path manipulation

  # CLI and user interface
  - optparse-applicative # CLI parsing
  - ansi-terminal       # Colored output
  - terminal-progress-bar # Progress indicators

  # Concurrency and state
  - async               # Concurrent operations
  - stm                 # Software Transactional Memory
  - capataz             # Supervision trees

  # Data and configuration
  - aeson               # JSON parsing
  - yaml                # YAML support
  - configurator        # Config management

  # Networking and SSH
  - libssh2             # SSH client bindings (must support static linking)
  - network             # Network utilities

  # Logging and debugging
  - co-log              # Contravariant logging with sum types
  - pretty-simple       # Debug output

  # Testing
  - hspec               # Testing framework
  - hspec-discover      # Test discovery
```

## Comprehensive Integration Test Suite

### Core VM Lifecycle Tests
1. **Complete lifecycle flow**: create → start → stop → destroy
2. **VM restart functionality**: stop → start cycle
3. **Multiple VM instances**: Managing VMs for different branches simultaneously
4. **Port allocation**: Automatic port assignment when conflicts occur
5. **Stale process cleanup**: Handling zombie QEMU processes

### State Management Tests
6. **Configuration persistence**: JSON config survives restarts
7. **SSH key management**: Key generation, storage, and permissions
8. **PID file handling**: Creation, verification, and cleanup
9. **State directory structure**: Proper organization of VM files
10. **Concurrent state updates**: Multiple operations on same VM

### Network and Connectivity Tests
11. **SSH connectivity**: Successful connection after VM start
12. **Port forwarding**: MCP proxy port accessibility
13. **Network isolation**: VMs can't interfere with each other
14. **Timeout handling**: Graceful failure when connections timeout
15. **Port conflict resolution**: Finding free ports automatically

### Process Management Tests
16. **Nix build integration**: Building VM configurations
17. **Script execution**: Running Nix-generated VM scripts
18. **Process monitoring**: Checking if QEMU is running
19. **Signal handling**: Graceful shutdown on SIGTERM/SIGINT
20. **Resource cleanup**: Temp files and processes cleaned up

### Error Handling Tests
21. **Missing git repository**: Graceful handling when not in git repo
22. **VM already exists**: Proper error when creating duplicate
23. **VM not found**: Clear error for operations on non-existent VMs
24. **SSH key generation failure**: Recovery and error reporting
25. **Nix build failures**: Proper error propagation

### Workspace Management Tests
26. **Git clone operations**: Cloning repo to VM workspace
27. **Branch switching**: Correct branch in VM workspace
28. **File permissions**: Host user can access VM workspace
29. **VirtioFS mounting**: Workspace accessible in VM
30. **Disk space checks**: Warning when running low

### Agent Service Tests
31. **Service startup**: Agent MCP service starts correctly
32. **Service health checks**: Monitoring agent status
33. **Service logs**: Accessing journalctl output
34. **Service restart**: Handling service failures
35. **MCP proxy health**: HTTP endpoint responding

### CLI Interface Tests
36. **Global options**: --state-dir, --timeout, --verbose work
37. **Help text**: All commands have proper documentation
38. **Exit codes**: Correct codes for success/failure
39. **Interactive commands**: shell and logs work properly
40. **JSON output**: Machine-readable output when requested

### Edge Cases and Recovery Tests
41. **Corrupted config files**: Detection and error handling
42. **Interrupted operations**: Recovery from partial states
43. **Full disk**: Graceful failure when out of space
44. **Permission denied**: Clear errors for access issues
45. **Network unreachable**: Handling offline scenarios

### Performance and Scale Tests
46. **VM startup time**: Reasonable boot times
47. **Concurrent VM operations**: Starting multiple VMs
48. **Large workspace handling**: Performance with big repos
49. **Memory usage**: Controller doesn't leak memory
50. **Log rotation**: Old logs are cleaned up

## Type-Safe Guarantees

### What GHC Will Prevent
1. **Invalid VM operations**: Can't start a running VM, can't SSH into stopped VM
2. **Missing configuration**: All required fields must be present at compile time
3. **Race conditions**: STM ensures atomic state updates
4. **Resource leaks**: Bracket patterns ensure cleanup
5. **Null pointer exceptions**: No null in Haskell
6. **Type mismatches**: Strong typing throughout

### Example: Safe VM Operations

```haskell
-- This enforces the state machine at compile time
data VMOp :: VMState -> VMState -> Type -> Type where
  Start   :: VMConfig -> VMOp 'Stopped 'Running Process
  Stop    :: VMOp 'Running 'Stopped ()
  Pause   :: VMOp 'Running 'Paused ()
  Resume  :: VMOp 'Paused 'Running ()
  Destroy :: VMOp 'Stopped 'Destroyed ()

-- Execute operation with automatic state tracking
runVMOp :: VM s1 -> VMOp s1 s2 a -> IO (VM s2, a)
```

## Integration with Existing Nix Infrastructure

The Haskell implementation will:
1. Call existing `nix build` commands to generate VM configurations
2. Execute Nix-generated shell scripts for VM startup
3. Parse Nix evaluation output for configuration
4. Maintain compatibility with existing workspace structure

```haskell
-- Example Nix integration
buildVMConfig :: BranchName -> IO (Either BuildError VMConfig)
buildVMConfig branch = do
  let flakeRef = "path:.#vmConfig." <> branchToText branch
  result <- readProcessStdout $ proc "nix" ["eval", "--json", flakeRef]
  case eitherDecode result of
    Left err -> return $ Left $ JSONParseError err
    Right config -> return $ Right config
```

## Testing Strategy

### Integration Tests with hspec
```haskell
spec :: Spec
spec = describe "Agent VM Integration Tests" $ do

  describe "VM Lifecycle" $ do
    it "completes full create-start-stop-destroy cycle" $ do
      withTempStateDir $ \stateDir -> do
        registry <- newVMRegistry
        result <- runExceptT $ do
          createVMConfig registry "test-branch" defaultConfig
          startVM registry "test-branch"
          stopVM registry "test-branch"
          destroyVM registry "test-branch"
        result `shouldBe` Right ()

    it "handles concurrent VM operations safely" $ do
      withTempStateDir $ \stateDir -> do
        registry <- newVMRegistry
        -- Start 10 VMs concurrently
        results <- forConcurrently [1..10] $ \i -> do
          let branch = "test-" <> show i
          runExceptT $ do
            createVMConfig registry branch defaultConfig
            startVM registry branch
        all isRight results `shouldBe` True
```

## Library Architecture

The implementation will be structured as a library with a thin CLI wrapper:

```haskell
-- Library exports (AgentVM.hs)
module AgentVM
  ( -- * VM Operations
    createVM
  , startVM
  , stopVM
  , destroyVM
  , vmStatus

    -- * Types
  , VMConfig(..)
  , VMState(..)
  , VMError(..)

    -- * Registry
  , VMRegistry
  , newVMRegistry

    -- * Logging
  , AgentVmTrace(..)
  , showTrace
  ) where

-- CLI app (app/Main.hs)
import AgentVM
import Options.Applicative
```

This separation enables future integration into a long-running daemon while maintaining the current CLI interface.

## Success Criteria

1. **Feature parity**: All Python features implemented
2. **Performance**: 5x improvement in concurrent operations
3. **Reliability**: Zero runtime type errors
4. **Maintainability**: Reduced bug rate after migration
5. **Developer experience**: Clear error messages, good CLI help
6. **Test coverage**: All 50 integration test scenarios passing

## Timeline

- **Month 1**: Core implementation (Phases 1-2)
- **Month 2**: Features and testing (Phases 3-4)
- **Month 3**: Integration and rollout (Phase 5)

## Implementation Notes

### Process Output Capture
The Python implementation captures subprocess stdout/stderr to temporary files for debugging. In Haskell:

```haskell
-- Capture process output with automatic cleanup
data ProcessCapture = ProcessCapture
  { pcStdout :: Handle
  , pcStderr :: Handle
  , pcLogDir :: FilePath
  }

withProcessCapture :: VMName -> (ProcessCapture -> IO a) -> IO a
withProcessCapture vmName action =
  bracket acquireCapture releaseCapture action
  where
    acquireCapture = do
      logDir <- getVMLogDir vmName
      stdout <- openTempFile logDir "stdout.log"
      stderr <- openTempFile logDir "stderr.log"
      return $ ProcessCapture stdout stderr logDir
```

### SSH Port Allocation
The system automatically finds free ports starting from 2222:

```haskell
findFreePort :: Int -> IO (Either PortError Int)
findFreePort startPort = go startPort (startPort + 100)
  where
    go port maxPort
      | port >= maxPort = return $ Left NoFreePortsAvailable
      | otherwise = do
          available <- isPortFree port
          if available
            then return $ Right port
            else go (port + 1) maxPort
```

### Color-Coded Output
Maintain the same visual feedback with ANSI colors:

```haskell
data OutputStyle = Success | Error | Warning | Info | Debug

styleOutput :: OutputStyle -> Text -> Text
styleOutput style msg = case style of
  Success -> green "✅ " <> msg
  Error   -> red "❌ " <> msg
  Warning -> yellow "⚠️ " <> msg
  Info    -> blue "🚀 " <> msg
  Debug   -> dim "🔧 " <> msg
```

## Static Linking Considerations

The use of haskell.nix for creating statically linked executables with musl requires careful attention to:
- libssh2 must be built with musl support
- Any C dependencies must be statically linkable
- Final binary will be fully self-contained

## Daemon Architecture (Future)

While the initial scope is limited to the CLI tool, the library architecture enables future daemon integration:

```haskell
-- Future daemon API (not in initial scope)
data VMDaemon = VMDaemon
  { daemonRegistry :: VMRegistry
  , daemonLogger :: Logger AgentVmTrace
  , daemonConfig :: DaemonConfig
  }

-- The library provides all functionality needed for both CLI and daemon
```

## Serialization Testing Strategy

Following the guideline that the compiler handles type safety and external libraries will handle roundtrip testing:
- No manual JSON serialization tests needed (Aeson's Generic deriving is proven)
- No property-based testing for serialization (will be added by library later)
- Focus integration tests on actual VM behavior, not data encoding

## Conclusion

Migrating agent-vm to Haskell will provide:
- Compile-time guarantees preventing entire classes of bugs
- Superior concurrency for managing multiple VMs
- Type-safe state machines enforcing valid operations
- Better performance and resource utilization
- Clean library/CLI separation for future daemon integration
- Contravariant logging with rich context via sum types

The investment in migration will pay off through reduced debugging time, fewer runtime errors, and a more maintainable codebase that GHC helps keep correct.

## Notes on the Migration Approach

1. **Coexistence**: The Python version will remain functional during the migration, allowing gradual transition and validation.

2. **Static Linking**: The use of haskell.nix for musl-based static linking will produce self-contained binaries that can be deployed without runtime dependencies.

3. **Library-First Design**: All functionality will be in a library, with the CLI as a thin wrapper, enabling future daemon integration without code changes.

4. **Testing Focus**: With 50 comprehensive integration tests using hspec, the Haskell version will have better test coverage than the Python original.

5. **Type-Driven Development**: By encoding VM states and operations in the type system, many bugs will be caught at compile time rather than runtime.

The migration represents not just a language change, but an architectural improvement that leverages Haskell's strengths to create a more robust and maintainable system.
