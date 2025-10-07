# ADR 006: Hotkey & Audio Capture Implementation

## Status
Accepted

## Context
Hotkey-triggered audio capture with hold-to-talk workflow.
Covers: hotkey handling, audio subprocess management, daemon architecture, duration limits, feedback

## Decision

### Hotkey
**Hold-to-talk:** press holds → record, release → stop & transcribe

**Platform handling:**
- Linux: Hyprland `bind`/`bindrelease` in `~/.config/hypr/hyprland.conf`
- macOS: skhd in `~/.skhdrc`
Manual config (no automation)

### Architecture
**Single binary, two modes:**
- Daemon: background, maintains state, listens on Unix socket
- Command: `start-recording`/`stop-recording` via socket IPC

### IPC
Unix socket at `$XDG_RUNTIME_DIR/whisper-input.sock`
Protocol: `START`/`STOP`, responds `ACK`/`ERROR: msg`

PID file at `$XDG_RUNTIME_DIR/whisper-input.pid`
Startup checks PID, cleans stale socket, auto-recovers from crashes

### Daemon Lifecycle
systemd/launchd service (manual setup), auto-restart, starts on login

### Audio Process
Fresh subprocess per recording (pw-record/parecord/sox)
Writes `~/.local/state/whisper-input/recordings/recording-{timestamp}.wav`
SIGTERM on stop, passes file to Whisper

### Duration Limits
Max: 60s (kill, notify, wait for release, transcribe)
Min: 0.5s (discard, acts as cancel)

### Feedback
- Recording: silent (key hold is feedback)
- Transcription: silent if <3s, "Transcribing..." if ≥3s
- Errors: immediate notification (ADR-004)
- Concurrent: "Transcription in progress" notification

### Validation
Startup checks mic device exists (ADR-004)

### Concurrency
Single transcription, states: idle/recording/transcribing
Block new requests if not idle

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
- Per ADR-004 startup validation philosophy
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
- Per ADR-005 concurrent handling

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
- Natural hold-to-talk gesture
- Leverages existing infrastructure (Hyprland/skhd)
- PID file enables crash recovery
- Adaptive feedback minimizes distraction
- Single binary simplifies distribution

### Negative
- Manual hotkey config required
- macOS needs skhd dependency
- IPC/PID management adds complexity
- Service setup required
- No concurrent request queuing
