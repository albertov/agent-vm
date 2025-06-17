# Haskell agent-vm Implementation TODO


## Testing Infrastructure

### Integration Test Setup (`test/AgentVM/IntegrationSpec.hs`)
```haskell
```

## Implementation Phases

### URGENT

- [x] Run the fix build issues
- [ ] Add NoImplicitPrelude extension to cabal file
      and use protolude instead

### Phase 1: Core Infrastructure (Weeks 1-2)
- [ ] Set up project with Cabal and haskell.nix
- [ ] Implement core types (Types.hs)
- [ ] Implement logging infrastructure (Logging.hs)
- [ ] Create STM-based registry (State.hs)
- [ ] Add JSON config parsing (Config.hs)

### Phase 2: VM Lifecycle Management (Weeks 3-4)
- [ ] Write comprehensive integration tests with `pending` bodies
- [ ] Implement Nix integration (Nix.hs)
- [ ] Add process management for VM startup
- [ ] Implement SSH connectivity (SSH.hs)
- [ ] Port all VM operations (create, start, stop, destroy)

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
