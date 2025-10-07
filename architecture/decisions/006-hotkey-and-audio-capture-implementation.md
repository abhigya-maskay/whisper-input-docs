# ADR 006: Hotkey & Audio Capture Implementation

## Status
Accepted

## Context
Whisper Input requires a hotkey-triggered audio capture mechanism for voice dictation. The system must detect when the user wants to record, capture audio while they speak, and stop when they're done. This ADR covers hotkey registration, audio process management, application architecture, and user feedback.

Key considerations:
- **Hold-to-talk workflow** - User presses and holds key to record, releases to stop and transcribe
- **Platform differences** - Linux (Hyprland/Wayland) and macOS require different hotkey mechanisms
- **Process orchestration** - Audio capture runs as subprocess, needs lifecycle management
- **Background daemon** - Runs continuously, responds to hotkey commands
- **Safety limits** - Prevent runaway recordings and accidental activations
- **User feedback** - Balance between informative and non-intrusive notifications
- **Error handling** - Fail fast with clear feedback per ADR-004

## Decision

### Hotkey Behavior
**Hold-to-talk activation:**
- User presses and holds configured hotkey → recording starts
- User releases hotkey → recording stops, transcription begins
- Natural, intuitive gesture for voice input

### Platform-Specific Hotkey Handling

**Linux (Hyprland):**
- Use Hyprland's native `bind` and `bindrelease` directives
- User manually configures `~/.config/hypr/hyprland.conf`:
  - `bind = SUPER_SHIFT, Space, exec, whisper-input start-recording`
  - `bindrelease = SUPER_SHIFT, Space, exec, whisper-input stop-recording`
- Leverages existing window manager infrastructure

**macOS:**
- Use `skhd` (Simple Hotkey Daemon)
- User manually configures `~/.skhdrc` with press/release bindings
- Consistent architecture across platforms (external hotkey handler → app commands)

**Configuration approach:**
- Manual setup via documentation only
- No automated scripts or file editing
- User has full control over configuration files

### Application Architecture
**Single binary with dual modes:**

**Daemon mode:**
- Long-running background process
- Maintains state (recording status, transcription in progress)
- Listens for commands via Unix domain socket
- Started via systemd (Linux) or launchd (macOS) service

**Command mode:**
- Lightweight client invocations: `whisper-input start-recording` / `whisper-input stop-recording`
- Connects to daemon via socket
- Sends command, receives acknowledgment or error
- Exits immediately after response

### IPC Mechanism
**Unix domain socket:**
- Location: `$XDG_RUNTIME_DIR/whisper-input.sock`
- Fallback: `/tmp/whisper-input-$UID.sock` if `XDG_RUNTIME_DIR` unset
- Simple text protocol: `START` / `STOP` commands
- Daemon responds with `ACK` or `ERROR: message`
- Bidirectional communication for error reporting

**Daemon startup:**
- Write PID to `$XDG_RUNTIME_DIR/whisper-input.pid`
- On startup, check for existing socket:
  - If socket exists, read PID file
  - Check if PID process is running
  - If running, fail with error (daemon already running)
  - If not running, clean up stale socket and PID file, then start
- Automatic recovery from crashes without manual intervention

### Daemon Lifecycle Management
**systemd/launchd service:**
- Daemon started via system service manager
- User sets up service manually (one-time configuration)
- Auto-restart on crash
- Starts on login
- Service file templates provided in documentation

### Audio Capture Process Management
**Fresh subprocess per recording:**
- `start-recording` command → daemon spawns audio capture process (`pw-record` / `parecord` / `sox`)
- Process writes audio to file: `~/.local/state/whisper-input/recordings/recording-{timestamp}.wav`
- `stop-recording` command → daemon sends SIGTERM to process
- Process exits cleanly, finalizes WAV file
- File passed to Whisper for transcription

### Recording Duration Limits
**Maximum duration: 60 seconds**
- Hard limit to prevent accidental runaway recordings
- At 60 seconds, daemon kills recording process (SIGTERM)
- Shows notification: "Maximum recording duration reached (60s)"
- Waits for user to release key (completes hold-to-talk gesture)
- When key released, proceeds with transcription of 60-second recording

**Minimum duration: 0.5 seconds**
- Recordings shorter than 0.5 seconds are discarded
- Shows low-priority notification: "Recording too short"
- Acts as implicit cancelation mechanism (quick tap = abort)
- Filters accidental hotkey taps

### User Feedback Strategy

**During recording:**
- No notifications while recording
- Physical act of holding key is the feedback
- Minimal distraction, clean UX

**During transcription:**
- If transcription completes in < 3 seconds: silent, text just appears
- If transcription takes ≥ 3 seconds: show notification "Transcribing..."
- Adaptive feedback - only notify when useful

**On errors:**
- Immediate notification on recording failure (aligns with ADR-004 fail-fast)
- If recording process fails to start or crashes, show error immediately (even if key still held)
- User can release key and retry

**On concurrent requests:**
- If hotkey pressed while transcription in progress: ignore, show notification "Transcription in progress, please wait"
- No queuing, no cancelation of in-progress transcription

### Audio Device Validation
**Startup validation only:**
- On daemon startup, validate configured microphone device exists
- Fail daemon startup if device not found (aligns with ADR-004 startup validation)
- Show critical notification with device name and setup instructions
- Exit with non-zero status code

### Concurrent Hotkey Handling
**Single recording/transcription at a time:**
- Daemon tracks state: idle / recording / transcribing
- Hotkey press ignored if not in idle state
- Prevents race conditions and out-of-order text injection
- Simple state management, predictable behavior

**Multiple presses during recording:**
- Once recording started, ignore additional key presses
- Only the first key release stops recording
- No key-repeat or multi-finger edge cases

## Options Considered

### Hotkey Behavior

#### Option A: Toggle (press to start, press again to stop)
**Pros:**
- Single key action to start

**Cons:**
- Two separate presses required
- Less intuitive for voice input
- Easy to forget to stop recording

#### Option B: Hold-to-talk (Selected)
**Pros:**
- Natural gesture - physical feedback while recording
- Automatic stop when key released
- Prevents forgotten recordings
- Common pattern in voice communication tools

**Cons:**
- Requires holding key (minor for short dictation)

#### Option C: Press-to-start with auto-stop
**Pros:**
- No need to hold key

**Cons:**
- Silence detection unreliable for dictation pauses
- Max duration required anyway
- Less user control over stop timing

### Linux Hotkey Handling

#### Option A: Hyprland native binds (Selected)
**Pros:**
- Leverages existing window manager
- Reliable, well-tested key handling
- No additional dependencies
- Fits personal use case (already using Hyprland)

**Cons:**
- Requires manual config file editing
- Hyprland-specific (acceptable for personal tool)

#### Option B: Direct evdev input capture
**Pros:**
- No Hyprland config changes

**Cons:**
- Requires root permissions or udev rules
- Complex input event handling
- Keyboard layout mapping challenges
- Over-engineered for personal use

#### Option C: Hotkey daemon (sxhkd)
**Pros:**
- Separation of concerns

**Cons:**
- Additional process running
- Unnecessary when Hyprland already handles hotkeys

### macOS Hotkey Handling

#### Option A: skhd (Selected)
**Pros:**
- Consistent with Linux approach (external hotkey handler)
- Supports press/release events
- Simple configuration
- Established tool in macOS ecosystem

**Cons:**
- Additional dependency

#### Option B: Carbon/Cocoa Event Tap FFI
**Pros:**
- No external dependencies

**Cons:**
- Complex platform-specific FFI
- Weeks of development time
- Ongoing maintenance burden
- Violates ADR-002 principle of avoiding FFI complexity

#### Option C: AppleScript polling
**Pros:**
- No FFI, no additional dependencies

**Cons:**
- Polling-based (high CPU usage)
- Unreliable for hold-to-talk
- Poor latency

### Application Architecture

#### Option A: Single binary, daemon + command modes (Selected)
**Pros:**
- Single artifact to install
- Daemon maintains state across recordings
- Commands are lightweight, instant response
- Standard pattern for system tools
- Clean separation of concerns

**Cons:**
- Need IPC mechanism

#### Option B: Hotkey commands spawn full app
**Pros:**
- No IPC needed

**Cons:**
- Can't maintain state between recordings
- Slow startup on each hotkey press
- Concurrent invocation issues

#### Option C: Pure daemon with D-Bus
**Pros:**
- Standard desktop IPC

**Cons:**
- D-Bus complexity overkill for start/stop commands
- Additional dependency
- Platform differences (less standard on macOS)

### IPC Mechanism

#### Option A: Unix domain socket (Selected)
**Pros:**
- Standard, reliable, portable (Linux and macOS)
- Bidirectional - daemon can send errors to client
- Fast, low overhead
- File-based addressing (well-known location)

**Cons:**
- Socket file management (stale sockets after crashes)

#### Option B: Named pipe (FIFO)
**Pros:**
- Simple, file-based

**Cons:**
- One-way communication (can't send errors back)
- Blocking I/O complications
- Less standard for command/response pattern

#### Option C: Signals (USR1/USR2)
**Pros:**
- No socket files

**Cons:**
- Can't pass error information back to client
- Limited signal vocabulary
- Race conditions on rapid signals
- No confirmation of receipt

### Daemon Startup Socket Handling

#### Option A: Remove stale socket blindly
**Pros:**
- Automatic recovery

**Cons:**
- Could accidentally interfere with running daemon if check is flawed

#### Option B: Fail if socket exists
**Pros:**
- Safe, no accidental interference

**Cons:**
- Requires manual intervention after every crash
- Poor user experience

#### Option C: PID file + stale detection (Selected)
**Pros:**
- Reliable detection of running vs crashed daemon
- Automatic recovery from crashes
- Safe - won't interfere with running daemon

**Cons:**
- Additional PID file to manage (minimal complexity)

### Daemon Lifecycle

#### Option A: systemd/launchd service (Selected)
**Pros:**
- Auto-restart on crash
- Starts on login
- Standard system integration
- Consistent with existing approach (ydotoold daemon, etc.)
- Service manager handles process supervision

**Cons:**
- One-time manual setup required

#### Option B: Auto-start on first command
**Pros:**
- Zero setup, works immediately

**Cons:**
- No auto-restart after crashes
- Doesn't survive reboots
- Daemon could be left orphaned
- Unreliable for background service

#### Option C: Hybrid (auto-start + document service)
**Pros:**
- Works out of box

**Cons:**
- Inconsistent behavior before/after service setup
- Confusing for debugging
- Two code paths to maintain

### Audio Process Management

#### Option A: Start on press, kill on release (Selected)
**Pros:**
- Simple, direct control
- Audio tools handle SIGTERM gracefully
- Real-time responsiveness
- Aligns with subprocess approach from ADR-002

**Cons:**
- None significant

#### Option B: Stream to app, buffer in memory
**Pros:**
- More control over audio data

**Cons:**
- Complex streaming/buffering
- Memory management overhead
- Unnecessary - audio tools write files perfectly fine
- Violates simplicity principle

#### Option C: Pre-allocate max duration, truncate
**Pros:**
- Simple file handling

**Cons:**
- WAV truncation complex (header updates)
- Wasted I/O
- Inefficient

### Maximum Duration Behavior

#### Option A: Auto-stop, notify, wait for release, transcribe (Selected)
**Pros:**
- Maintains hold-to-talk gesture model
- User completes natural action
- Clear notification of limit

**Cons:**
- User might hold key unnecessarily after limit

#### Option B: Auto-stop and immediately transcribe
**Pros:**
- Faster to completion

**Cons:**
- Breaks hold-to-talk mental model
- Confusing (text appears while key held)

#### Option C: Continue recording, truncate later
**Pros:**
- User-controlled timing

**Cons:**
- Wastes resources recording audio we'll discard
- Limit becomes meaningless

### Minimum Duration Handling

#### Option A: No minimum
**Pros:**
- Maximum flexibility

**Cons:**
- Accidental taps trigger transcription
- Wasted processing on empty recordings

#### Option B: 0.5 seconds minimum (Selected)
**Pros:**
- Filters accidental taps
- Acts as natural cancelation (quick tap = abort)
- Short enough for intentional recordings

**Cons:**
- Very brief words might be hard to record

#### Option C: 1 second minimum
**Pros:**
- Definitely intentional only

**Cons:**
- Too restrictive
- Harder to dictate short words

### Transcription Feedback

#### Option A: Notification while transcribing
**Pros:**
- User always knows status

**Cons:**
- Distracting for fast transcriptions
- Notification lingers on screen

#### Option B: Brief notification, silent completion
**Pros:**
- Confirmation without persistence

**Cons:**
- User doesn't know when done
- Two notifications per transcription

#### Option C: Silent transcription
**Pros:**
- Non-intrusive

**Cons:**
- Long transcriptions leave user wondering if it's working

#### Option D: Conditional notification (≥3s shows progress) (Selected)
**Pros:**
- Best of both worlds - minimal distraction for fast, feedback for slow
- Adaptive to actual performance
- Most transcriptions are fast (silent), long ones get confirmation

**Cons:**
- Slightly more complex logic (negligible)

### Recording Feedback

#### Option A: Notification while recording
**Pros:**
- Visual confirmation

**Cons:**
- Distracting popup
- Might cover text field user is targeting

#### Option B: No feedback (Selected)
**Pros:**
- Minimal distraction
- Physical key holding is the feedback
- Clean UX

**Cons:**
- No visual confirmation

#### Option C: Notification on start only
**Pros:**
- Confirmation without persistence

**Cons:**
- Still adds distraction
- User doesn't know when stopped (but they control release)

### Audio Device Validation

#### Option A: Startup only (Selected)
**Pros:**
- Aligns with ADR-004 startup validation philosophy
- Fast recording start (no repeated checks)
- Device changes rare for personal use

**Cons:**
- If device unplugged later, runtime error

#### Option B: Before each recording
**Pros:**
- Handles dynamic device changes (USB mics)

**Cons:**
- Delay before every recording
- Repeated checks wasteful
- Over-engineering for personal use

#### Option C: Lazy validation on first failure
**Pros:**
- No startup delay, helpful errors

**Cons:**
- First recording after device removal fails
- Delayed feedback

### Concurrent Request Handling

#### Option A: Block with notification (Selected)
**Pros:**
- Clear feedback
- Simple state management
- Predictable behavior
- Matches ADR-005 specification

**Cons:**
- User sees notification if they accidentally press

#### Option B: Queue requests
**Pros:**
- No lost input

**Cons:**
- Complex state management
- Unexpected behavior (text appears later)
- Out-of-order injection possible

#### Option C: Cancel and restart
**Pros:**
- User control

**Cons:**
- Loses work in progress
- Confusing interaction

### Configuration Approach

#### Option A: Manual configuration, documentation only (Selected)
**Pros:**
- User has full control
- No magic file editing
- Clear, explicit setup
- Fits power-user audience

**Cons:**
- Requires manual setup step

#### Option B: Setup script
**Pros:**
- Automated, less error-prone

**Cons:**
- Users uncomfortable with scripts modifying configs
- Platform-specific scripts to maintain
- Reduces transparency

#### Option C: Separate include file
**Pros:**
- Modular, easy to enable/disable

**Cons:**
- Still requires manual edit to add include directive
- Extra file to manage

## Consequences

### Positive
- Hold-to-talk provides natural, intuitive voice input gesture
- Platform-specific hotkey handlers leverage existing, reliable infrastructure
- Daemon architecture maintains state efficiently across recordings
- Unix socket IPC is standard, fast, and supports error reporting
- PID file enables automatic recovery from crashes without manual intervention
- Service management ensures daemon reliability and auto-restart
- Audio process lifecycle is simple and aligns with ADR-002 subprocess approach
- Duration limits prevent accidents while allowing substantial dictation
- Adaptive feedback minimizes distraction while providing reassurance for slow operations
- Fail-fast error handling provides immediate feedback (ADR-004 alignment)
- Single binary simplifies installation and distribution
- Startup validation catches configuration issues before first use

### Negative
- Requires manual hotkey configuration (one-time setup cost)
- Additional dependency on macOS (skhd)
- IPC socket and PID file management adds complexity
- systemd/launchd service setup required for full reliability
- Hold-to-talk requires physically holding key (minor for short dictation)
- No queuing of concurrent requests (acceptable for personal use)
- Device unplugged after startup causes runtime error rather than graceful handling

### Neutral
- Platform-specific hotkey mechanisms reflect platform differences appropriately
- Manual configuration gives user full control over their system
- Daemon runs continuously (minimal resource usage, standard for system tools)
- Socket/PID files in runtime directory cleaned up automatically on logout
