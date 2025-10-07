# ADR 002: Technology Stack Selection

## Status
Accepted

## Context
Whisper Input is a personal voice-to-text tool built in Haskell, targeting Linux (Hyprland/Wayland) and macOS (Apple Silicon). The tool needs to:
- Capture audio from the microphone
- Transcribe speech using local Whisper models with GPU acceleration
- Inject transcribed text into any application
- Provide minimal UI (CLI-based with hotkeys)
- Support future evolution toward chunked/streaming transcription

Key constraints:
- **Performance-critical**: Fast transcription using GPU acceleration (NVIDIA CUDA on Linux, Metal on macOS)
- **Privacy-first**: All processing happens locally
- **Platform-specific optimization**: Different hardware capabilities per platform
- **Maintainability**: Simple, clean architecture for single developer
- **Reliability over micro-optimization**: Prefer battle-tested tools over complex FFI bindings
- **Development velocity**: Minimize time spent on infrastructure, maximize feature development
- **Future-proofing**: Enable migration to streaming/chunked transcription later

## Decision

We will use the following technology stack:

### Core Language
- **Haskell** - As specified in Project Overview

### Speech Recognition
Platform-specific implementations optimized for each GPU:

**Linux (NVIDIA GPU):**
- **faster-whisper** with CUDA acceleration
- Integration via subprocess (CLI mode initially)
- Python-based wrapper around CTranslate2

**macOS (Apple Silicon):**
- **whisper.cpp** with Metal and Core ML/ANE acceleration
- Integration via subprocess (binary executable)
- C/C++ implementation optimized for Apple Silicon

**Future Migration Path:**
- Both tools support server mode for low-latency streaming
- Can migrate from CLI subprocess to HTTP/socket-based IPC when streaming is needed
- No architectural changes required for this migration

### Text Injection
Platform-specific keystroke simulation tools:

**Linux (Hyprland/Wayland):**
- **ydotool** - Kernel-level uinput framework for keystroke injection
- Requires: `ydotoold` daemon running (systemd service)
- Hyprland has native support for configuring ydotool virtual devices

**macOS:**
- **cliclick** - Simple CLI tool for keyboard/mouse emulation
- Install: `brew install cliclick`
- Usage: `cliclick t:"text"`
- Requires: Accessibility permissions

### Audio Capture
Platform-specific command-line tools via subprocess:

**Linux:**
- **PipeWire:** `pw-record` (modern systems)
- **PulseAudio:** `parecord` (fallback)
- Format: 16-bit PCM, 16kHz, mono

**macOS:**
- **sox** - Sound eXchange utility
- Cross-platform audio tool with CoreAudio support
- Install: `brew install sox`

### User Interface
- **Minimal CLI approach**
- Configuration via config file (YAML/TOML)
- Hotkey-based activation (no GUI)
- Background service/daemon
- Suitable for power users and tiling window managers

## Options Considered

### Speech Recognition Integration

#### Option 1: Platform-specific subprocess CLI (Selected)
**Linux:** faster-whisper, **macOS:** whisper.cpp

**Pros:**
- Best performance on each platform (CUDA on NVIDIA, Metal on Apple Silicon)
- Simple integration (no FFI complexity)
- Independent updates of Whisper libraries
- Clean separation of concerns
- Easy migration to server mode for streaming
- Standard approach for both tools

**Cons:**
- Two different implementations to maintain
- Subprocess overhead (~10-50ms, negligible for record-then-transcribe)

#### Option 2: whisper.cpp on both platforms
**Pros:**
- Single implementation
- Excellent on Apple Silicon (Metal + ANE)

**Cons:**
- Slower on NVIDIA GPU (~3-4x slower than faster-whisper)
- Higher hallucination rate (68% line repetition in tests)
- Suboptimal for primary Linux use case

#### Option 3: FFI bindings to C/C++ libraries
**Pros:**
- Lowest latency (~0-10ms overhead)
- Direct memory access

**Cons:**
- Weeks of development time for proper bindings
- No existing Haskell bindings for whisper.cpp
- CTranslate2 C++ FFI extremely complex in Haskell
- Ongoing maintenance burden
- Unnecessary for record-then-transcribe workflow
- Can't easily leverage tool updates

#### Option 4: faster-whisper on both platforms
**Pros:**
- Single implementation
- Best on NVIDIA GPU

**Cons:**
- No Metal/GPU support on Apple Silicon (CPU-only)
- CTranslate2 only supports CUDA GPUs
- Misses 4.5x speedup from Metal/ANE on macOS

### Text Injection (Linux/Wayland)

#### Option 1: ydotool (Selected)
**Pros:**
- Actually works on Wayland
- Kernel-level uinput (universal compatibility)
- Hyprland has built-in support
- Reliable keystroke simulation

**Cons:**
- Requires daemon setup (one-time)
- Needs permissions configuration

#### Option 2: wtype
**Pros:**
- Wayland-native approach

**Cons:**
- Requires virtual-keyboard protocol support
- Not supported by all compositors
- Recent issues with Hyprland keybind triggering
- Less mature than ydotool

#### Option 3: Clipboard + notification
**Pros:**
- No special permissions

**Cons:**
- User must manually paste (breaks workflow)
- Not truly "system-wide"
- Interferes with clipboard

#### Option 4: Hyprland sendshortcut dispatcher
**Pros:**
- Hyprland-specific native integration
- No daemon needed

**Cons:**
- Only works in Hyprland (not portable)
- More fragile (relies on Ctrl+V support)
- Clipboard interference

### Audio Capture

#### Option 1: Command-line tools via subprocess (Selected)
**Linux:** `pw-record` / `parecord`, **macOS:** `sox`

**Pros:**
- ✅ **Extremely reliable** - battle-tested tools used everywhere
- ✅ **No FFI complexity** - no segfaults, memory leaks, or binding issues
- ✅ **Consistent architecture** - already using subprocess for whisper
- ✅ **Simple integration** - easy to start/stop/control
- ✅ **Negligible overhead** - ~10-30ms per session (<1% of total latency)
- ✅ **Zero maintenance burden** - no bindings to maintain

**Cons:**
- Platform-specific commands (but simple abstraction)
- Less fine-grained control (acceptable for record-then-transcribe)

#### Option 2: PortAudio FFI bindings
**Pros:**
- Cross-platform abstraction
- Lower overhead (~1-5ms vs 10-30ms)

**Cons:**
- ❌ **Unmaintained** - Haskell bindings last updated 2014
- ❌ **Known bugs** - segfaults, Windows FFI crashes reported
- ❌ **High complexity** - FFI marshalling, memory management
- ❌ **Weeks of debugging risk** - old, unmaintained code
- ❌ **Marginal benefit** - saves 20ms on 3-10 second workflows (0.2%)
- Requires libportaudio system dependency

#### Option 3: Custom PortAudio FFI
Write minimal bindings yourself

**Pros:**
- Full control
- Lowest latency (~1-5ms)

**Cons:**
- ❌ **2-5 days development time** to save 20ms per recording
- ❌ **Ongoing maintenance** - platform-specific bugs
- ❌ **Risk of memory leaks** and crashes
- ❌ **Poor ROI** - time better spent on features

#### Option 4: Platform-specific FFI bindings
**Linux:** PulseAudio/PipeWire bindings, **macOS:** CoreAudio bindings

**Pros:**
- Maximum control and lowest latency

**Cons:**
- Even more complex than PortAudio
- Different implementations per platform
- Same poor ROI as Option 3

### User Interface

#### Option 1: Minimal CLI (Selected)
**Pros:**
- Simplest possible implementation
- Perfect for Hyprland power users
- No GUI framework dependencies
- Fast to build

**Cons:**
- Less user-friendly for non-technical users
- No graphical settings panel

#### Option 2: GTK (gi-gtk bindings)
**Pros:**
- Native Linux feel
- System tray support

**Cons:**
- Complex FFI bindings
- Overkill for personal tool
- macOS support exists but not native-looking

#### Option 3: Web-based (Threepenny-gui)
**Pros:**
- Easy to build nice UI in Haskell
- Cross-platform

**Cons:**
- Less "native" feel
- System tray support tricky
- Overhead of running web server

## Consequences

### Positive
- **Optimal performance** on both target platforms using native GPU acceleration
- **Unified architecture** - subprocess approach for all external tools (whisper, audio, text injection)
- **Rock-solid reliability** - using battle-tested command-line tools
- **Zero FFI complexity** - no bindings, no segfaults, no memory management issues
- **Easy to build and maintain** - pure Haskell process management
- **Future-proof** - clear migration path to streaming via server mode
- **Independent updates** - can update all external tools separately
- **Minimal dependencies** - only standard system tools (mostly pre-installed)
- **Hyprland-optimized** - ydotool integration is well-supported
- **Negligible overhead** - subprocess latency irrelevant for 3-10 second recordings

### Negative
- **Platform-specific code paths** for Whisper, audio capture, and text injection
- **External dependencies** required:
  - Linux: faster-whisper (Python), ydotoold daemon, pw-record/parecord
  - macOS: whisper.cpp binary, cliclick, sox
- **Setup requirements** documented and non-trivial (CUDA, daemon configuration, permissions)
- **No GUI** limits accessibility for non-technical users (acceptable for personal tool)
- **Less fine-grained control** over audio stream (acceptable trade-off for simplicity)

### Neutral
- **Subprocess approach** adds minimal latency for record-then-transcribe (~10-50ms)
- **Two Whisper implementations** to keep updated, but they're stable upstream projects
- **Can revisit later** - decisions are reversible if requirements change

## Implementation Notes

### Dependencies Setup

**Linux (NVIDIA):**
```bash
# Install faster-whisper
pip install faster-whisper

# Install and setup ydotool
# Distribution-specific package manager
systemctl --user enable --now ydotoold

# Audio recording (usually pre-installed)
# PipeWire: pw-record (modern)
# PulseAudio: parecord (fallback)
```

**macOS (Apple Silicon):**
```bash
# Install whisper.cpp
brew install whisper-cpp

# Install cliclick
brew install cliclick

# Install sox for audio recording
brew install sox

# Grant Accessibility permissions for cliclick
# System Settings > Privacy & Security > Accessibility
```

### Audio Recording Commands

**Linux (PipeWire):**
```bash
pw-record --format=s16 --rate=16000 --channels=1 output.wav
```

**Linux (PulseAudio fallback):**
```bash
parecord --format=s16le --rate=16000 --channels=1 output.wav
```

**macOS:**
```bash
sox -d -t wav -r 16000 -c 1 -b 16 output.wav
```

### Future Migration to Streaming

When migrating to chunked/streaming transcription:

1. **Start server processes on app launch:**
   - Linux: `faster-whisper-server --model large-v3 --device cuda`
   - macOS: `whisper.cpp --server --model large-v3`

2. **Switch from CLI subprocess to HTTP client:**
   - Send audio chunks via HTTP POST
   - Receive transcription responses
   - Minimal Haskell code changes (http-client instead of process)

3. **Benefits:**
   - Low latency (~50-200ms per chunk)
   - No FFI complexity
   - Same performance characteristics

### Hyprland Configuration

Example per-device config for ydotool virtual device (if needed):
```
device {
    name = ydotoold-virtual-device
    kb_layout = us
}
```

## Related Decisions
- ADR-001: System Architecture & Component Structure - Established monolithic architecture suitable for these technology choices
