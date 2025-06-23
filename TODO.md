# Haskell agent-vm Implementation TODO


## Testing Infrastructure

### Integration Test Setup (`test/AgentVM/IntegrationSpec.hs`)
```haskell
```

## Implementation Phases

### URGENT
- [x] Refactor the functions from Process.hs, SSH.hs, Nix.hs, etc
  that take a 'IOTracer AgentVmTrace' to use the same pattern
  as the previous step


- [x] Refactor createVM and destroyVM so they look like this

```haskell
createVM ::
    (
    MonadReader env m,
    HasType (IOTracer AgentVmTrace) env
    ) => VMConfig -> m VMHandle

destroyVM ::
    (
    MonadReader env m,
    HasType (IOTracer AgentVmTrace) env
    ) => VMHandle -> m ()
```

and crate a type like

```haskell
{-# LANGUAGE BangPatterns #-}
-- | This lives in AgentVM.Env module
data AgentVmEnv = AgentVmEnv {
    tracer :: !(IOTracer AgentVmTrace,)
    ...
} deriving Generic
```

to hold the app environment

Then you'd do `runReaderT (createVM config) agentVmEnv` to use them
But ALWAYS combine them like this:

```haskell
-- | This lives in AgentVM.Env module
runVM = flip runReaderT

-- | This lives where it is used
runVM agentVmEnv $ do
   machine <- createVM cfg
   destroyVM machine
```

You'll see why we do this soon

- [x] - Create a VM type-class like
```haskell
class VM m where
    create :: ...
    destroy :: ...
    ...
```

and implement an instance for it using createVM and destroyVM

- [ ] Don't use IOTracer but Tracer so we can log in non-IO monads (not a
  priority)

- [x] - The log systems should be using a type like
        ```haskell
        data LogLevel a = Critical a | Error a | Info a | Debug a | Trace a
           deriving via stock (Show, Ord, Eq, Generic, ToJSON, FromJSON, ...)

        So we can tag the importance of the trace and filter them via a level
        specified on config
      - The handlers should not change signature but we should use contravariant
      on a `traceLevel :: AgentVmTrace -> LogLevel AgentVmTrace` at Main.hs to tag them
      appropiately.
      - Implement a helper to do this: renderLogLevel :: (a -> Text) -> LogLevel a -> Text

- [x] Run the fix build issues
- [x] Add NoImplicitPrelude extension to cabal file
      and use protolude instead

### Phase 1: Core Infrastructure (Weeks 1-2) ✅ COMPLETED
- [x] Set up project with Cabal and haskell.nix
- [x] Implement core types (Types.hs)
- [x] Implement logging infrastructure (Logging.hs)
- [x] Create STM-based registry (State.hs)
- [x] Add JSON config parsing (Config.hs)

### Phase 2: VM Lifecycle Management (Weeks 3-4)
- [x] Write comprehensive integration tests with `pending` bodies
- [ ] Implement Nix integration (Nix.hs) - IN PROGRESS
  - [x] Basic module structure
  - [ ] Mock subprocess calls for testing
  - [ ] Full implementation
- [ ] Add process management for VM startup - IN PROGRESS
  - [x] checkVMProcess implementation
  - [x] startVMProcess implementation
  - [ ] stopVMProcess implementation
  - [ ] waitForProcess implementation
- [ ] Implement SSH connectivity (SSH.hs)
- [ ] Port all VM operations (create, start, stop, destroy)

### Progress Summary
Successfully implemented using TDD approach:
- VM Process Management: checkVMProcess, startVMProcess
- VM State Management: All port allocation functions tested and working
- Tests passing: 6/21 unit tests

### Phase 3: CLI Interface (Week 5)
- [ ] Write comprehensive CLI tests with `pending` bodies
- [ ] Implement optparse-applicative CLI (Main.hs)
- [ ] Add colored output using ansi-terminal
- [ ] Ensure proper error handling and reporting
- [ ] Maintain compatibility with Python CLI interface

### Phase 4: Advanced Features - agent-vmd systemd service (Weeks 6-7)
- [ ] Write comprehensive daemon tests with `pending` bodies
- [ ] Design systemd service architecture for host machine
- [ ] Implement supervisor trees using async/STM
- [ ] Add concurrent VM operation support
- [ ] Performance optimization and profiling

### Phase 5: Integration & Testing (Week 8)
- [ ] Complete all pending test implementations
- [ ] Documentation updates
- [ ] Performance benchmarking
- [ ] Create deployment strategy

## Development Guidelines

### Error Handling
- Use `Either VMError a` for operations that can fail
- Provide descriptive error messages
- Log all errors with appropriate severity

### Concurrency
- All shared state must go through STM
- Use async for long-running operations
- Implement proper cleanup with bracket patterns

### Testing
- Write tests first with `pending` implementations
- Aim for >90% code coverage
- Test both success and failure paths

### Performance
- Profile regularly during development
- Use ByteString for all I/O operations
- Minimize STM transaction scope
