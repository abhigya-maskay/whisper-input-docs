# ADR 004: Error Handling & Logging

## Status
Accepted

## Context
Background daemon orchestrating external processes. Failure points:
- Startup: missing dependencies, invalid config
- Runtime: mic unavailable, transcription/injection failures
- Subprocess: crashes, non-zero exits
- Resources: disk full, permissions

Requirements: immediate feedback, debugging capability

## Decision

### Philosophy
**Fail fast, notify immediately**
- Critical failures → desktop notification + stop operation
- Startup failures → validate dependencies, fail with explanation
- Non-critical → log only

### Notifications
`notify-send` (Linux) / `osascript` (macOS)
- Critical: startup/dependency failures
- Normal: runtime errors
- Low: warnings
Messages are user-friendly (no stack traces)

### Logging
**Levels:** DEBUG (verbose), INFO (key ops), WARN (non-critical), ERROR (critical)
**Config:** `log_level` in config.dhall, `WHISPER_INPUT_LOG_LEVEL` env var override
**Format:** `timestamp [Component] LEVEL message`
**Location:** `~/.local/state/whisper-input/whisper-input.log` (XDG compliant)
**Rotation:** 10MB max, keep 3 files (~30MB total)
Implementation: katip file scribe with size-based rotation

### Subprocess Errors
Capture stdout/stderr
- Notification: sanitized message
- Logs: ERROR (sanitized), DEBUG (full output + exit code)

### Startup Validation
**Linux:** faster-whisper, ydotoold running, pw-record/parecord, model files
**macOS:** whisper.cpp, cliclick, sox, Accessibility permissions, model files
On failure: ERROR log + critical notification + exit non-zero

### Stack Traces
INFO/WARN/ERROR: clean messages only
DEBUG: full Haskell traces, subprocess stderr, env details

## Options Considered

### Error Response Philosophy

#### Option A: Fail fast and notify (Selected)
**Pros:**
- ✅ **Immediate feedback** - User knows instantly when something fails
- ✅ **Clear UX** - No silent failures or confusion about whether it worked
- ✅ **Simple implementation** - No complex retry/fallback logic
- ✅ **Appropriate for interactive tool** - User is waiting for text to appear
- ✅ **Most failures unrecoverable** - Missing dependencies need user action anyway

**Cons:**
- Less graceful than automatic recovery (but few cases where that's possible)

#### Option B: Fail gracefully with fallback
**Pros:**
- Better automation, less user intervention

**Cons:**
- ❌ **Inappropriate for voice input** - User needs to know if it failed
- ❌ **Few fallback options** - No alternative to ydotool, faster-whisper, etc.
- ❌ **Complex** - Retry logic, fallback chains, state management
- ❌ **Hides problems** - User might not realize setup is broken

#### Option C: Silent failures with logging
**Pros:**
- Non-intrusive

**Cons:**
- ❌ **Terrible UX** - User speaks, nothing happens, no idea why
- ❌ **Confusing** - Have to check logs every time to debug
- ❌ **Inappropriate for interactive tool** - Voice input requires feedback

### Notification Mechanism

#### Option A: Desktop notifications + logging (Selected)
**Pros:**
- ✅ **Best of both worlds** - Immediate user feedback + debugging capability
- ✅ **Works for background service** - No terminal needed
- ✅ **Persistent record** - Logs survive across restarts
- ✅ **Debugging friendly** - Full context in logs, clean message in notification

**Cons:**
- Slightly more complex (but standard practice)

#### Option B: Terminal/stdout only
**Pros:**
- Simplest implementation

**Cons:**
- ❌ **Invisible when running as service** - systemd/launchd captures stdout
- ❌ **No user feedback** - Unless watching journalctl/Console.app
- ❌ **Poor UX** - Have to actively monitor logs

#### Option C: System logs (journald/unified logging)
**Pros:**
- Integration with system logging

**Cons:**
- ❌ **No user notification** - Still need to check logs
- ❌ **Platform-specific APIs** - Different implementation per OS
- ❌ **Less accessible** - journalctl less friendly than plain text files

### Logging Detail Level

#### Option A: Verbose with levels (DEBUG, INFO, WARN, ERROR) - Selected
**Pros:**
- ✅ **Flexible** - Can adjust verbosity as needed
- ✅ **Production-friendly** - INFO by default, DEBUG for troubleshooting
- ✅ **Complete debugging** - DEBUG captures everything
- ✅ **Clean at INFO** - Readable audit trail without noise
- ✅ **Standard practice** - Familiar to developers

**Cons:**
- Need log level configuration

#### Option B: Errors + key operations
**Pros:**
- Simpler (no levels to configure)

**Cons:**
- ❌ **Less flexible** - Can't get more detail when debugging
- ❌ **No deep troubleshooting** - Missing subprocess details, timing info

#### Option C: Errors only
**Pros:**
- Minimal file size

**Cons:**
- ❌ **No audit trail** - Can't see what app was doing
- ❌ **Hard to debug** - No context around errors
- ❌ **Unhelpful for personal tool** - Want to see what's happening

### Log File Location

#### Option A: XDG State Directory - `~/.local/state/whisper-input/` (Selected)
**Pros:**
- ✅ **Follows XDG standard** - Per ADR-003, state/logs belong in `$XDG_STATE_HOME`
- ✅ **Cross-platform** - Works on Linux and macOS
- ✅ **Organized** - Separate from config (`~/.config`)
- ✅ **Respects `$XDG_STATE_HOME`** - User can override
- ✅ **Future-proof** - Room for other state files

**Cons:**
- Less common than `~/.config` (but more semantically correct)

#### Option B: With config - `~/.config/whisper-input/`
**Pros:**
- Everything in one place

**Cons:**
- ❌ **Semantically incorrect** - Logs are state, not configuration
- ❌ **Mixes concerns** - Config should be read-only, logs are write-heavy
- ❌ **Not XDG-compliant** - XDG spec separates state and config

#### Option C: System logs (journald/unified logging)
**Pros:**
- OS integration

**Cons:**
- ❌ **Platform-specific** - Different on Linux vs macOS
- ❌ **Less control over format** - Harder to structure logs
- ❌ **Harder to access** - journalctl vs text file

### Log Rotation

#### Option A: Size-based rotation (Selected)
10 MB max, keep last 3 files

**Pros:**
- ✅ **Predictable disk usage** - ~30MB maximum
- ✅ **Prevents runaway growth** - Caps file size automatically
- ✅ **Simple to implement** - Check size on write, rotate when needed
- ✅ **Sufficient for personal use** - 10MB = weeks/months at INFO level
- ✅ **Debugging-friendly** - Recent history preserved

**Cons:**
- Slight complexity over append-only

#### Option B: Simple append, no rotation
**Pros:**
- Simplest possible

**Cons:**
- ❌ **Unbounded growth** - Could fill disk over time
- ❌ **Performance degradation** - Large files slow to read/search
- ❌ **Manual cleanup** - User has to remember to truncate

#### Option C: Time-based rotation
**Pros:**
- Nice organization (one file per day)

**Cons:**
- ❌ **More complex** - Time tracking, date logic
- ❌ **Unpredictable size** - Could have huge daily files if DEBUG enabled
- ❌ **Overkill** - Personal tool doesn't need daily rotation

### Startup Validation

#### Option A: Validate critical dependencies on startup (Selected)
**Pros:**
- ✅ **Fail fast with clear errors** - User knows immediately what's wrong
- ✅ **Helpful error messages** - Explain what's missing and how to fix
- ✅ **Prevents confusing runtime errors** - No "first hotkey press mysteriously fails"
- ✅ **Better onboarding** - Setup issues caught upfront
- ✅ **Simple to implement** - Check PATH, check running services

**Cons:**
- Slightly slower startup (negligible - milliseconds)

#### Option B: Lazy validation (check on first use)
**Pros:**
- Faster startup

**Cons:**
- ❌ **Delayed feedback** - User thinks it started successfully, then first use fails
- ❌ **Confusing UX** - "Why did my hotkey not work?"
- ❌ **Harder to debug** - Error appears in runtime logs, not startup

#### Option C: Validate but continue anyway
**Pros:**
- Doesn't block startup

**Cons:**
- ❌ **Pointless** - App can't function without dependencies anyway
- ❌ **Confusing** - Starts successfully but doesn't work
- ❌ **Wastes resources** - Running a broken service

## Consequences

### Positive
- Instant error notification
- Full debugging at DEBUG level
- Flexible verbosity (config + env var)
- XDG-compliant
- Predictable disk usage (~30MB cap)
- Startup validation catches setup issues early

### Negative
- Notification dependencies required
- Platform-specific commands
- Log files accumulate (30MB cap)

## Implementation Notes

### Haskell Libraries

**Logging:**
- **`katip`** - Structured logging library with multiple log destinations (scribes), typed contexts, and good performance

**Notifications:**
- Linux: `notify-send` command via subprocess
- macOS: `osascript -e 'display notification'` command via subprocess

### Startup Validation

**Linux dependency checks:**
- `faster-whisper` Python module available (`python -c "import faster_whisper"`)
- Custom Python wrapper script exists (`/usr/local/lib/whisper-input/whisper-transcribe.py`)
- `pw-record` or `parecord` in PATH
- `ydotoold` service running (check via `systemctl --user is-active`)

**macOS dependency checks:**
- `whisper.cpp` binary in PATH
- `cliclick` in PATH
- `sox` in PATH

### Configuration Schema Addition

Update `config.dhall` to include log level:
- Add `log_level` field to both `linux` and `macos` sections
- Values: "DEBUG", "INFO", "WARN", "ERROR"
- Default: "INFO"

### Environment Variable Override

- Variable name: `WHISPER_INPUT_LOG_LEVEL`
- Takes precedence over config file setting
- Same values as config: DEBUG, INFO, WARN, ERROR

### Component Names

Standardized component identifiers for logging:

- `[Main]` - Application startup/shutdown
- `[Config]` - Configuration loading/validation
- `[AudioCapture]` - Microphone recording
- `[Whisper]` - Speech recognition
- `[TextInjection]` - Keystroke simulation
- `[Hotkey]` - Hotkey registration/handling
- `[Notification]` - Desktop notifications
- `[Subprocess]` - Generic subprocess management

### Example Log Output

**INFO level (default):**
```
2025-10-06 14:32:10 [Main] INFO Whisper Input starting...
2025-10-06 14:32:10 [Config] INFO Loaded config from ~/.config/whisper-input/config.dhall
2025-10-06 14:32:10 [Hotkey] INFO Registered hotkey: Super+Shift+Space
2025-10-06 14:32:15 [AudioCapture] INFO Recording started (device: default)
2025-10-06 14:32:18 [AudioCapture] INFO Recording stopped (duration: 3.2s)
2025-10-06 14:32:18 [Whisper] INFO Transcription started (model: medium)
2025-10-06 14:32:20 [Whisper] INFO Transcription completed: "Hello world"
2025-10-06 14:32:20 [TextInjection] INFO Text injected successfully
```

**DEBUG level:**
```
2025-10-06 14:32:15 [AudioCapture] DEBUG Running: pw-record --format=s16 --rate=16000 --channels=1 /tmp/whisper-abc123.wav
2025-10-06 14:32:18 [AudioCapture] DEBUG Process exited: ExitSuccess
2025-10-06 14:32:18 [AudioCapture] DEBUG stdout:
2025-10-06 14:32:18 [Whisper] DEBUG Running: faster-whisper --model medium --device cuda --device-index 0 /tmp/whisper-abc123.wav
2025-10-06 14:32:20 [Whisper] DEBUG stdout: {"text": "Hello world", "language": "en"}
2025-10-06 14:32:20 [TextInjection] DEBUG Running: ydotool type "Hello world"
2025-10-06 14:32:20 [TextInjection] DEBUG Process exited: ExitSuccess
```

**ERROR example:**
```
2025-10-06 14:35:12 [AudioCapture] ERROR Failed to start audio recording: Microphone 'default' not found
2025-10-06 14:35:12 [AudioCapture] DEBUG Running: pw-record --format=s16 --rate=16000 --channels=1 /tmp/whisper-xyz789.wav
2025-10-06 14:35:12 [AudioCapture] DEBUG stderr: pw-record: error opening device 'default': No such device
2025-10-06 14:35:12 [AudioCapture] ERROR pw-record failed with exit code 1
2025-10-06 14:35:12 [Notification] INFO Sent notification: "Recording failed" - "Microphone 'default' not found"
```

## Related Decisions
- [[001-system-architecture-and-component-structure|ADR-001: System Architecture]] - Monolithic architecture simplifies error propagation
- [[002-technology-stack-selection|ADR-002: Technology Stack]] - Subprocess approach requires robust error handling for external tools
- [[003-configuration-and-settings-storage|ADR-003: Configuration & Settings Storage]] - Log level configuration follows same pattern (Dhall + env var)
