# ADR 005: Local Whisper Model Integration

## Status
Accepted

## Context
Whisper Input needs to integrate local Whisper models for speech-to-text transcription. From ADR-002, we've chosen platform-specific implementations: faster-whisper (Linux/CUDA) and whisper.cpp (macOS/Metal). This ADR covers the integration architecture, model management, process lifecycle, and error handling.

Key considerations:
- **Platform differences** - Different tools with different interfaces and model formats
- **Interactive workflow** - User expects fast feedback after recording
- **Model management** - Download, validation, and configuration
- **Reliability** - Subprocess orchestration with timeout and error handling
- **Personal use** - Single user, offline-first, simplicity over flexibility
- **Debugging** - Need visibility into transcription process and failures

Critical decisions:
1. Model selection and availability validation
2. Process lifecycle (fresh vs persistent)
3. Error handling and timeout behavior
4. Temporary file management
5. Output parsing and post-processing
6. Concurrent request handling

## Decision

### Model Support
**Support all Whisper model sizes** with user selection via configuration:
- tiny, base, small, medium, large-v1, large-v2, large-v3
- Default: medium (balance of speed and accuracy)

### Model Configuration
**Model name with optional path override:**
```dhall
whisper = {
  model = "medium"              -- Model name
  model_path = None Text        -- Optional: override with full path
  gpu_device = 0                -- Linux: CUDA device number
  -- or use_gpu = True          -- macOS: Metal on/off
}
```

Platform-specific model handling:
- **Linux/faster-whisper:** Uses model name directly, auto-downloads to `~/.cache/huggingface/hub/`
- **macOS/whisper.cpp:** Requires GGML/GGUF format, looks in conventional paths if `model_path` not specified

### Model Availability Validation
**Validate model availability on startup:**
- **Linux:** Check if model cached in `~/.cache/huggingface/hub/` or allow first-use download
- **macOS:** Verify model file exists at configured/conventional path, fail if missing
- **On validation failure:** Show critical notification with setup instructions, exit with error

Aligns with ADR-004's startup validation philosophy: "Fail fast with clear errors" and "Prevents confusing runtime errors."

### Model Download Strategy
**Hybrid approach per platform:**

**Linux:**
- faster-whisper auto-downloads models on first use
- Startup validation warns if model not cached, allows auto-download
- Optional script for pre-download (not required)

**macOS:**
- Provide `./scripts/download-model-macos.sh` wrapper script
- Wraps whisper.cpp download scripts, handles paths consistently
- Startup validation requires model exists, shows download command if missing

### Transcription Parameters
**Hardcode all parameters for English dictation:**
- Language: `"en"` (English only)
- Temperature: `0.0` (deterministic, reduces hallucinations)
- Beam size: `5` (quality/speed balance)
- No speech threshold: `0.6` (silence detection)

Follows ADR-003's principle of hardcoding technical parameters to prevent misconfiguration.

### Output Format & Parsing
**JSON output from both tools:**
- faster-whisper: `--output-json` flag
- whisper.cpp: `--output-json` flag
- Parse structured JSON for reliable interface
- Extract `text` field, ignore metadata (timestamps, confidence) for now

### Process Lifecycle
**Fresh process per transcription:**
- Start new subprocess for each transcription
- Process exits after completion
- No persistent server (defer to future streaming implementation)
- Simple state management, clean slate per request

### Timeout Handling
**Model-size-based hard timeouts:**
- tiny/base/small: 30 seconds
- medium: 60 seconds
- large/large-v3: 120 seconds

Kill process on timeout, notify user with error. Prevents indefinite hangs while allowing generous buffer (6x-30x typical transcription time).

### Temporary Audio File Management
**App state directory with retention:**
- Location: `~/.local/state/whisper-input/recordings/`
- Naming: `recording-{timestamp}.wav`
- Retention: Keep last 10 recordings
- Purpose: Debugging (inspect audio when transcription is wrong)

Rotation: Delete oldest when count exceeds 10, same pattern as log files (ADR-004).

### Error Handling
**Fail immediately, notify user:**
- Single transcription attempt, no automatic retries
- Desktop notification on failure (sanitized message)
- Full subprocess details logged at DEBUG level
- User can manually retry by pressing hotkey again

Aligns with ADR-004's "fail fast and notify" philosophy.

### GPU Fallback Behavior
**No automatic CPU fallback:**
- If GPU configured but fails, show error notification
- Don't silently fall back to CPU
- Surfaces configuration problems (drivers, CUDA, device number)
- Predictable performance (no surprising slowdowns)

### Silent Audio Handling
**Low-priority notification on empty transcription:**
- If transcription returns empty/whitespace-only text
- Show low-urgency notification: "No speech detected"
- Don't inject text
- Helps diagnose microphone issues vs actual failures

### Post-Processing
**Minimal cleanup only:**
- Trim leading/trailing whitespace
- Collapse multiple spaces to single space
- Preserve Whisper's output otherwise (capitalization, punctuation)

### Concurrent Request Handling
**Block new recordings during transcription:**
- Only one transcription at a time
- Hotkey ignored while transcription in progress
- Show notification: "Transcription in progress, please wait"
- Simple state management, prevents queue buildup

### Code Architecture
**Platform adapter pattern:**

**Module structure:**
- `Whisper/Types.hs` - Shared types (WhisperResult, ModelConfig, errors)
- `Whisper/Backend.hs` - Platform interface and transcribe function
- `Whisper/FasterWhisper.hs` - Linux implementation
- `Whisper/WhisperCpp.hs` - macOS implementation

**Backend interface:**
- Model validation (platform-specific paths and formats)
- Command construction (different flags per tool)
- Output parsing (shared JSON parsing logic)

Shared logic (timeout, temp file management, post-processing) in `Backend.hs`, platform-specific in adapters.

## Options Considered

### Model Selection
- **Support all sizes** (Selected) - Maximum flexibility, both tools support all sizes
- Limited subset - Unnecessary restriction
- Medium only - Too limiting

### Model Validation
- **Startup validation** (Selected) - Fail fast with clear errors, consistent with ADR-004
- Lazy validation - Delayed feedback, confusing UX
- No validation - Silent failures

### Model Download
- **Hybrid approach** (Selected) - Addresses platform differences appropriately
- Provide scripts for both - Over-engineering for Linux auto-download
- Document only - Less helpful for macOS complexity

### Transcription Parameters
- **Hardcode all** (Selected) - Simplicity, prevents misconfiguration
- Language configurable - Unnecessary (English-only use case)
- Advanced parameters - Deferred per ADR-003

### Output Parsing
- **JSON** (Selected) - Reliable, stable interface, trivial in Haskell
- Plain text - Brittle, format changes break parsing
- Mixed approach - Inconsistent

### Process Lifecycle
- **Fresh per transcription** (Selected) - Simple, aligns with CLI approach
- Persistent server - More complex, defer to streaming migration
- Lazy server - Added complexity without clear benefit

### Timeout Strategy
- **Model-size-based** (Selected) - Smart, proportional feedback
- Single timeout - Less responsive for fast models
- No timeout - Risk of indefinite hangs

### Temp File Management
- **State directory with retention** (Selected) - Debugging capability, minimal disk usage
- System temp with auto-delete - Loses debugging information
- Keep on error only - More complex conditional logic

### Error Handling
- **Fail immediately** (Selected) - Fast feedback, aligns with ADR-004
- Retry once - Delays feedback, transient errors rare
- Smart retry - Complex, marginal benefit

### GPU Fallback
- **No fallback** (Selected) - Surfaces issues, predictable performance
- Auto fallback to CPU - Masks problems, surprising slowdowns
- Conditional fallback - Complex error parsing

### Concurrent Handling
- **Block new requests** (Selected) - Simple state, clear feedback
- Queue requests - Complex, can build up
- Allow concurrent - Out-of-order injection, complex state

### Code Structure
- **Platform adapters** (Selected) - Clean abstraction, shared logic stays DRY
- Platform-specific modules - Code duplication
- Conditional logic - Messy, hard to test

## Consequences

### Positive
- Clear model management with startup validation
- Platform differences cleanly abstracted
- Simple, predictable error handling (fail fast)
- Debugging capability with retained audio files
- Robust timeout handling prevents hangs
- Single-request state eliminates concurrency complexity
- JSON parsing provides stable interface
- Clean architecture enables future server mode migration

### Negative
- Model loading overhead per transcription (1-5 seconds)
- No automatic recovery from GPU failures
- No concurrent transcription support
- Platform-specific download workflows
- Retained audio files use disk space (~3MB for 10 recordings)

### Neutral
- Fresh process per transcription trades startup cost for simplicity
- Hardcoded parameters reduce flexibility (acceptable for personal use)
- Can migrate to persistent server mode later without architecture changes

## Implementation Notes

### Startup Validation Commands

**Linux:**
```bash
# Check faster-whisper available
which faster-whisper || python -c "import faster_whisper"

# Check model cached (optional, warns if missing)
ls ~/.cache/huggingface/hub/models--*/snapshots/*/medium*
```

**macOS:**
```bash
# Check whisper.cpp binary
which whisper-cpp

# Check model file exists
ls ~/.whisper-models/ggml-medium.bin
```

### Transcription Commands

**Linux (faster-whisper):**
```bash
faster-whisper /path/to/audio.wav \
  --model medium \
  --device cuda \
  --device_index 0 \
  --language en \
  --output_format json
```

**macOS (whisper.cpp):**
```bash
whisper-cpp \
  -m /path/to/ggml-medium.bin \
  -f /path/to/audio.wav \
  -l en \
  --output-json
```

### Model Download Script (macOS)

`./scripts/download-model-macos.sh`:
- Wraps whisper.cpp download scripts
- Places models in `~/.whisper-models/`
- Updates config template with correct paths

### Temp File Rotation

Same logic as log rotation (ADR-004):
- Check file count in `~/.local/state/whisper-input/recordings/`
- Sort by timestamp
- Delete oldest when count > 10

### Notification Messages

**Errors:**
- "Transcription failed" (critical)
- "Model 'medium' not found" (critical, on startup)
- "GPU initialization failed" (critical)

**Warnings:**
- "Transcription in progress, please wait" (normal)

**Info:**
- "No speech detected" (low priority)

## Related Decisions
- **ADR-001**: System Architecture - Monolithic architecture simplifies subprocess orchestration
- **ADR-002**: Technology Stack - Established faster-whisper and whisper.cpp as platform-specific choices
- **ADR-003**: Configuration & Settings Storage - Model configuration follows Dhall pattern with platform-specific sections
- **ADR-004**: Error Handling & Logging - Startup validation, fail-fast philosophy, notification strategy
