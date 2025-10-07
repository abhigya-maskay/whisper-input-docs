# ADR 002: Technology Stack Selection

## Status
Accepted

## Context
Haskell voice-to-text tool for Linux (Hyprland/Wayland) and macOS (Apple Silicon). Requirements:
- GPU-accelerated transcription (CUDA/Metal)
- Audio capture and text injection
- Minimal CLI interface
- Future streaming support

Constraints:
- Reliability over micro-optimization (battle-tested tools, no complex FFI)
- Platform-specific optimization per GPU

## Decision

### Core: Haskell

### Speech Recognition
**Linux:** faster-whisper (CUDA) via custom Python wrapper subprocess
**macOS:** whisper.cpp (Metal/ANE) via subprocess

Both support server mode for future streaming migration via HTTP/sockets.

### Text Injection
**Linux:** ydotool (kernel uinput, requires ydotoold daemon)
**macOS:** cliclick (requires Accessibility permissions)

### Audio Capture
**Linux:** pw-record (PipeWire) or parecord (PulseAudio fallback)
**macOS:** sox

Format: 16-bit PCM, 16kHz, mono

### UI
Minimal CLI with config file, hotkey activation, background daemon

## Options Considered

### Speech Recognition Integration

#### Option 1: Platform-specific subprocess (Selected)
**Linux:** faster-whisper via custom Python wrapper, **macOS:** whisper.cpp

**Pros:**
- Best performance on each platform (CUDA on NVIDIA, Metal on Apple Silicon)
- Simple integration (no FFI complexity)
- Independent updates of Whisper libraries
- Clean separation of concerns
- Easy migration to server mode for streaming
- Custom wrapper provides full control over interface
- No dependency on unmaintained third-party CLI wrappers

**Cons:**
- Two different implementations to maintain
- Subprocess overhead (~10-50ms, negligible for record-then-transcribe)
- Custom Python wrapper script to maintain

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
- Optimal GPU performance per platform
- Unified subprocess architecture for all external tools
- Zero FFI complexity
- Clear streaming migration path
- Custom Python wrapper provides full control

### Negative
- Platform-specific code paths
- External dependencies (faster-whisper, ydotoold, whisper.cpp, cliclick, sox)
- Non-trivial setup (CUDA, daemons, permissions)

## Implementation Notes

### Dependencies Setup

**Linux (NVIDIA):**
```bash
# Install faster-whisper Python library
pip install faster-whisper

# Custom Python wrapper script included in repository
# Located at: ./scripts/whisper-transcribe.py

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
