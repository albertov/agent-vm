# agent-vm (Haskell)

Type-safe VM lifecycle management with Nix integration.

## Overview

This is the Haskell implementation of agent-vm, providing compile-time guarantees for VM state transitions and superior concurrency through STM.

## Features

- **Type-safe state machines**: VM operations are validated at compile time
- **STM-based concurrency**: Atomic operations without deadlocks
- **Nix integration**: Seamless integration with existing Nix infrastructure
- **Comprehensive logging**: Structured logging with co-log

## Building

```bash
nix build .#agent-vm
```

## Testing

```bash
nix run .#integration-test
```

## Status

This implementation is currently a work in progress. The Python version (agent-vm-py) remains the primary implementation.
