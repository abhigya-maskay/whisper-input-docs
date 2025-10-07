# ADR 004: Error Handling & Logging

## Status
Accepted

## Context
Whisper Input is a background daemon that orchestrates multiple external processes (audio capture, speech recognition, text injection) with potential failure points at every step. As a personal tool running as a system service, it needs robust error handling and observability.

Key considerations:
- **Interactive workflow** - User presses hotkey and expects immediate feedback
- **Multiple failure points** - Microphone unavailable, Whisper process crashes, text injection fails, dependencies missing
- **Background operation** - Running as systemd/launchd service (no visible terminal)
- **Debugging complexity** - Subprocess integration means errors can occur in external tools
- **Personal use** - Single user needs clear feedback and debugging capability
- **Simplicity** - Avoid over-engineering for edge cases

Critical failure scenarios:
1. **Startup failures** - Missing dependencies (faster-whisper, ydotool), daemon not running (ydotoold), invalid config
2. **Runtime failures** - Microphone busy/unavailable, Whisper transcription fails, text injection blocked
3. **Subprocess failures** - External tools crash, return non-zero exit codes, produce unexpected output
4. **Resource failures** - Disk full, permissions denied, temp file creation fails

## Decision

### Error Handling Philosophy
**Fail fast and notify immediately** - Stop operations on critical errors and inform the user via desktop notifications.

**Critical failures → Immediate notification:**
- Microphone unavailable
- Whisper process failure
- Text injection failure
- Dependency missing

**Startup failures → Fail hard with explanation:**
- Config malformed (show Dhall error)
- Dependencies missing (show what's missing and how to install)
- Hotkey registration fails (show conflict)
- **Validate all critical dependencies on startup** before proceeding

**Non-critical → Log only:**
- Temp file cleanup failures
- Unknown config fields (forward compatibility)

### Notification Mechanism
**Desktop notifications + logging** (both channels):

**Notifications:**
- Use `notify-send` (Linux) / `osascript -e 'display notification'` (macOS)
- User-friendly error messages (sanitized, no stack traces)
- Severity-based urgency levels:
  - **Critical** - Startup failures, missing dependencies (stays on screen)
  - **Normal** - Runtime errors like transcription failures (standard notification)
  - **Low** - Warnings (or no notification, log only)

**Logging:**
- Complete error details, stack traces, subprocess output
- Persistent file for debugging and audit trail

### Logging System
**Log levels:**
- **DEBUG** - Verbose subprocess details, audio buffers, every operation
- **INFO** - Key operations (recording started/stopped, transcription completed, text injected)
- **WARN** - Retryable errors, deprecations, non-critical issues
- **ERROR** - Critical failures that stop current operation

**Log level configuration:**
- Default level in `config.dhall`: `log_level = "INFO"`
- Environment variable override: `WHISPER_INPUT_LOG_LEVEL=DEBUG`
- Restart required for config change, env var works immediately

**Log format:**
```
2025-10-06 14:32:15 [AudioCapture] INFO Recording started
2025-10-06 14:32:18 [Whisper] INFO Transcription completed: "Hello world"
2025-10-06 14:32:19 [TextInjection] ERROR Text injection failed: ydotool not responding
```
- Timestamp (ISO 8601)
- Component name in brackets
- Log level
- Message

**Log file location:**
- **Path:** `~/.local/state/whisper-input/whisper-input.log`
- Follows XDG State Directory specification
- Platform-independent (works on Linux and macOS)

**Log rotation:**
- **Max file size:** 10 MB before rotation
- **Retention:** Keep last 3 files (current + 2 backups = ~30MB total)
- **Naming:** `whisper-input.log`, `whisper-input.log.1`, `whisper-input.log.2`
- Automatic rotation when size limit reached

### Subprocess Error Handling
**On subprocess failure (non-zero exit, crash):**
- Capture both stdout and stderr
- **Notification:** User-friendly message (sanitized, no raw subprocess output)
  - Example: "Whisper transcription failed" instead of Python traceback
- **Logs:**
  - ERROR level: Sanitized error message with context
  - DEBUG level: Full subprocess stdout, stderr, exit code, command line

**Subprocess output logging:**
- **stdout:** Always logged at DEBUG level
- **stderr:** Always logged at DEBUG level
- **On failure:** stderr also logged at ERROR level with context

### Startup Validation
**Validate critical dependencies before running:**

**Linux checks:**
1. `faster-whisper` in PATH (or Python module available)
2. `ydotoold` daemon running (`systemctl --user is-active ydotoold`)
3. Audio recording tool available (`pw-record` or `parecord` in PATH)
4. Whisper model files exist (if specified in config)

**macOS checks:**
1. `whisper.cpp` binary in PATH
2. `cliclick` in PATH
3. `sox` in PATH
4. Accessibility permissions granted (if detectable)
5. Whisper model files exist

**On validation failure:**
- Log detailed error at ERROR level
- Show critical-priority notification with fix instructions
- Exit with non-zero status code
- Example: "Startup failed: ydotoold daemon not running. Start it with: systemctl --user start ydotoold"

### Error Context & Stack Traces
**INFO/WARN/ERROR levels:**
- Error message with operation context
- Example: "Failed to start audio recording: Microphone 'default' not found"
- No stack traces (clean, readable)

**DEBUG level:**
- Full Haskell stack traces (when available)
- Subprocess stderr output
- Environment details (working directory, PATH, env vars)
- Maximum debugging capability

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
- ✅ **Follows XDG standard** - Correct semantic location for state/logs
- ✅ **Cross-platform** - Works on Linux and macOS
- ✅ **Organized** - Separate from config (`~/.config`) and cache
- ✅ **Respects `$XDG_STATE_HOME`** - User can override
- ✅ **Future-proof** - Room for other state files (PID, temp files)

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
- ✅ **Clear user feedback** - Instant notification when errors occur
- ✅ **Excellent debugging** - Full logs at DEBUG level with all subprocess details
- ✅ **Flexible verbosity** - INFO by default, DEBUG when needed, env var override
- ✅ **XDG-compliant** - Follows standards for state directory
- ✅ **Predictable disk usage** - Log rotation caps at ~30MB
- ✅ **Fast startup failure** - Dependency validation catches setup issues immediately
- ✅ **Production-ready logging** - Structured format with component names and timestamps
- ✅ **Component isolation** - Can trace errors to specific subsystems
- ✅ **Audit trail** - INFO logs show what app was doing when error occurred
- ✅ **Simple implementation** - No complex retry logic or fallback chains

### Negative
- Desktop notification dependencies (notify-send, osascript) required
- Must implement log rotation logic
- Platform-specific notification commands
- Startup validation adds slight delay (milliseconds)
- Log files accumulate over time (but capped at 30MB)

### Neutral
- Restart required to change log level via config (env var works immediately)
- Notifications may be intrusive during debugging (can disable if needed)
- DEBUG level can produce large logs quickly (expected behavior)

## Implementation Notes

### Haskell Libraries

**Logging:**
- **`katip`** - Structured logging library with multiple log destinations (scribes), typed contexts, and good performance

**Notifications:**
- Linux: `notify-send` command via subprocess
- macOS: `osascript -e 'display notification'` command via subprocess

### Startup Validation

**Linux dependency checks:**
- `faster-whisper` in PATH or Python module available
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
- **ADR-001**: System Architecture - Monolithic architecture simplifies error propagation
- **ADR-002**: Technology Stack - Subprocess approach requires robust error handling for external tools
- **ADR-003**: Configuration & Settings Storage - Log level configuration follows same pattern (Dhall + env var)
