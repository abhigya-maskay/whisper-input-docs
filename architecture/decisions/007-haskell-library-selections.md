# ADR 007: Haskell Library Selections

## Status
Accepted

## Context
Selecting Haskell libraries for:
- Process management (external tools via subprocess)
- Async/concurrency (daemon architecture, IPC, state management)
- JSON parsing (whisper output)
- Unix socket IPC
- XDG-compliant file paths
- CLI parsing

Constraints: actively maintained, type-safe, composable, support streaming migration

## Decision

### Core Libraries

1. **Process Management**: `typed-process`
2. **Async/Concurrency**: `async` + `stm`
3. **JSON Parsing**: `aeson`
4. **IPC (Unix Sockets)**: `network` (+ `unix`)
5. **File Path Handling**: `directory` + `filepath` + `time`
6. **CLI Argument Parsing**: `optparse-applicative`

### Supporting Libraries (from previous ADRs)

7. **Logging**: `katip` (per ADR-004)
8. **Configuration**: `dhall` (per ADR-003)

### Standard Libraries

9. **Text**: `text` - Unicode text handling
10. **Binary Data**: `bytestring` - Binary I/O, network data

## Options Considered

### 1. Process Management

#### `typed-process` (Selected)
- Type-safe I/O configuration, async integration, signal control (SIGINT/SIGTERM), exception-safe cleanup
- Supersedes older `process` library

**Rejected:**
- `process`: Less type-safe, more boilerplate, manual async coordination
- `shelly`/`turtle`: Too high-level, abstracts away needed signal control

### 2. Async/Concurrency

#### `async` + `stm` (Selected)

**`async`:** Combinators (`race`, `concurrently`, `cancel`), exception-safe, integrates with `typed-process`, supports streaming migration

**`stm`:** Composable transactions (no race conditions), natural for state machines (idle/recording/transcribing), built-in retry semantics

**Rejected:**
- `forkIO` + `MVar`: Race-prone, less composable, manual cleanup
- High-level frameworks (`servant`): Wrong abstraction, heavy dependencies

### 3. JSON Parsing

#### `aeson` (Selected)
- Industry standard, type-safe automatic encoding/decoding, generic deriving (minimal boilerplate), high performance

**Rejected:**
- `json`: No generic deriving, less maintained
- Manual `attoparsec`: Unnecessary for standard JSON

### 4. IPC (Unix Sockets)

#### `network` (Selected)
- Low-level API with Unix domain socket support (`SockAddrUnix`), required for `$XDG_RUNTIME_DIR` socket per ADR-006; integrates with `async`

**Rejected:**
- `network-simple`: TCP/UDP helpers; insufficient coverage for Unix domain sockets
- `zeromq4-haskell`: Heavy C dependency, overkill for simple protocol
- D-Bus: Complex API, less standard on macOS

### 5. File Path Handling

#### `directory` + `filepath` + `time` (Selected)
- **`directory`:** XDG support (`getXdgDirectory`), cross-platform, environment-aware, directory operations
- **`filepath`:** Path manipulation (`</>`), platform-aware separators
- **`time`:** Timestamp formatting for filenames

All part of GHC platform (stable, widely available)

Note: Require `directory` version with XDG state/runtime support; code includes fallbacks when env vars are missing (macOS runtime → `/tmp/whisper-input-$UID` with 0700 perms; state default → `~/.local/state`).

**Rejected:**
- `path`: Type-safe but cumbersome, needs XDG integration
- Manual string manipulation: Error-prone, no XDG support

### 6. CLI Argument Parsing

#### `optparse-applicative` (Selected)
- Industry standard, declarative (automatic help/errors), subcommand support, type-safe (parse to ADTs), bash completion

**Rejected:**
- `cmdargs`: Less flexible, Template Haskell, weaker subcommands
- Manual parsing: Massive boilerplate, poor UX
- `optparse-simple`: Unnecessary wrapper

## Consequences

### Positive
- **Integration:** Libraries compose well (`typed-process` + `async`, `stm` + `async`, XDG paths across config/state/runtime)
- **Type safety:** Compile-time guarantees (I/O config, JSON parsing, CLI parsing, STM transactions)
- **Streaming-ready:** `async` supports concurrent chunking/HTTP, easy subprocess → HTTP migration
- **Maintainability:** Industry-standard, actively maintained, well-documented

### Negative
- ~10 direct dependencies (standard for Haskell)
- Learning curve: STM, applicative style, type-level configuration
- Longer initial build (incremental rebuilds fast)

## Implementation Notes

### Dependencies
- typed-process
- async, stm
- aeson, text, bytestring
- network, unix
- directory, filepath, time
- optparse-applicative
- dhall
- katip

Logging rotation: use `katip` file scribe with size-based rotation (10MB, keep 3) per [[004-error-handling-and-logging|ADR-004]].

## Related Decisions

- [[001-system-architecture-and-component-structure|ADR-001]]: Daemon model → `async` + `stm`
- [[002-technology-stack-selection|ADR-002]]: Subprocess approach → `typed-process`
- [[003-configuration-and-settings-storage|ADR-003]]: Dhall config, XDG paths → `directory`, `dhall`
- [[004-error-handling-and-logging|ADR-004]]: Katip logging, fail-fast → `katip`
- [[005-local-whisper-model-integration|ADR-005]]: JSON parsing, timeouts → `aeson`, `typed-process`, `async`
- [[006-hotkey-and-audio-capture-implementation|ADR-006]]: Unix sockets, signal handling → `network`, `async`, `stm`, `typed-process`, `unix`

## Future Considerations

**Streaming migration:**
- HTTP client: `http-client`/`http-conduit` (works with `async`, reuses `aeson`)
- WebSocket: `websockets` (integrates with `async`)

**Additional libraries (deferred):**
- Testing: `hspec`, `QuickCheck`, `tasty`
- Benchmarking: `criterion`
- Pretty-printing: `prettyprinter`
