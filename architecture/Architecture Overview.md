---
tags: [overview, architecture]
---

# Architecture Overview

Concise, high-level view of how Whisper Input is structured and operates. This summarizes the accepted ADRs; see linked notes for details.

## System Overview
- Single Haskell binary with daemon and command modes (hold‑to‑talk workflow).
- Flow: hotkey press → record audio → transcribe locally → inject text.
- See [[decisions/001-system-architecture-and-component-structure|ADR‑001]] and [[decisions/006-hotkey-and-audio-capture-implementation|ADR‑006]].

## Core Components
- Daemon: state machine (idle/recording/transcribing), IPC, subprocess orchestration, logging. ([[decisions/001-system-architecture-and-component-structure|ADR‑001]], [[decisions/004-error-handling-and-logging|ADR‑004]], [[decisions/006-hotkey-and-audio-capture-implementation|ADR‑006]])
- Audio capture: Linux `pw-record`/`parecord`; macOS `sox`; fixed 16 kHz, mono, 16‑bit. ([[decisions/002-technology-stack-selection|ADR‑002]], [[decisions/003-configuration-and-settings-storage|ADR‑003]], [[decisions/006-hotkey-and-audio-capture-implementation|ADR‑006]])
- Speech recognition: Linux faster‑whisper (CUDA) via custom Python wrapper; macOS whisper.cpp (CoreML/Metal). ([[decisions/002-technology-stack-selection|ADR‑002]], [[decisions/005-local-whisper-model-integration|ADR‑005]])
- Text injection: Linux `ydotool`; macOS `cliclick`. ([[decisions/002-technology-stack-selection|ADR‑002]], [[decisions/006-hotkey-and-audio-capture-implementation|ADR‑006]])
- Config/state: Dhall config; logs and recordings under XDG state dir with rotation. ([[decisions/003-configuration-and-settings-storage|ADR‑003]], [[decisions/004-error-handling-and-logging|ADR‑004]], [[decisions/005-local-whisper-model-integration|ADR‑005]])

## IPC & Lifecycle
- Unix domain socket at `$XDG_RUNTIME_DIR/whisper-input.sock` with `START`/`STOP` → `ACK`/`ERROR: …`.
- PID file for stale‑socket recovery; daemon managed by systemd/launchd (manual setup).
- Clients never auto‑start the daemon. ([[decisions/006-hotkey-and-audio-capture-implementation|ADR‑006]])

## Hotkeys
- Hold‑to‑talk: press to start recording, release to stop and transcribe.
- Linux: Hyprland bind/bindrelease; macOS: skhd. Configured externally. ([[decisions/006-hotkey-and-audio-capture-implementation|ADR‑006]])

## Error Handling & Observability
- Fail‑fast with desktop notifications; structured logs with levels via `katip`.
- Startup validation of critical dependencies.
- Log rotation (10 MB × 3); recordings retained with rotation. ([[decisions/004-error-handling-and-logging|ADR‑004]], [[decisions/005-local-whisper-model-integration|ADR‑005]])

## Operational Policies
- Single active transcription; concurrent requests are blocked with a notification.
- Duration limits: min 0.5 s (tap = cancel), max 60 s (auto‑stop + notify).
- Timeouts per model size; no GPU fallback (surface config issues). ([[decisions/005-local-whisper-model-integration|ADR‑005]], [[decisions/006-hotkey-and-audio-capture-implementation|ADR‑006]])

## Technology Stack
- Haskell with `typed-process`, `async`/`stm`, `aeson`, `network`/`unix`, `katip`, `dhall`, `optparse-applicative`.
- Subprocess‑first design avoids FFI complexity; streaming/server mode remains a future option. ([[decisions/002-technology-stack-selection|ADR‑002]], [[decisions/007-haskell-library-selections|ADR‑007]])

## Configuration
- Dhall at `~/.config/whisper-input/config.dhall` with platform sections:
  - Audio device
  - Whisper model + optional path; GPU settings (`gpu_device` on Linux, `use_gpu` on macOS)
  - `log_level` with `WHISPER_INPUT_LOG_LEVEL` override
- Defaults generated on first run; restart required to apply changes. ([[decisions/003-configuration-and-settings-storage|ADR‑003]])

## Future Direction
- Optional migration to streaming via whisper server processes (HTTP/WebSocket) with minimal core changes. ([[decisions/002-technology-stack-selection|ADR‑002]])

## Related ADRs
- [[decisions/001-system-architecture-and-component-structure|ADR‑001: Architecture & Components]]
- [[decisions/002-technology-stack-selection|ADR‑002: Technology Stack]]
- [[decisions/003-configuration-and-settings-storage|ADR‑003: Config & Settings]]
- [[decisions/004-error-handling-and-logging|ADR‑004: Errors & Logging]]
- [[decisions/005-local-whisper-model-integration|ADR‑005: Whisper Integration]]
- [[decisions/006-hotkey-and-audio-capture-implementation|ADR‑006: Hotkey & Audio Capture]]
- [[decisions/007-haskell-library-selections|ADR‑007: Haskell Libraries]]

