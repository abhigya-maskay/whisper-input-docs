# ADR 005: Local Whisper Model Integration

## Status
Accepted

## Context
Integration of faster-whisper (Linux/CUDA) and whisper.cpp (macOS/Metal) per [[002-technology-stack-selection|ADR-002]].
Covers: model management, process lifecycle, timeouts, file management, output parsing, concurrency

## Decision

### Models
All sizes supported (tiny/base/small/medium/large-v1/v2/v3), default: medium.en
Config: model name + optional path override

### Validation
Startup checks per [[004-error-handling-and-logging|ADR-004]]:
- Linux: faster-whisper lib, wrapper script at `/usr/local/lib/whisper-input/whisper-transcribe.py`
- macOS: model file exists; whisper.cpp built with CoreML (ANE) preferred, Metal fallback
Auto-download on Linux, manual on macOS via `./scripts/download-model-macos.sh`

### Parameters (Hardcoded)
English only, temp 0.0, beam 5, no-speech 0.6

### Output
JSON from both tools, parse `text` field

### Process
Fresh subprocess per transcription, exits after completion

### Timeouts
30s (tiny/base/small), 60s (medium), 120s (large)
Kill on timeout, notify user

### Audio Files
`~/.local/state/whisper-input/recordings/recording-{timestamp}.wav`
Keep last 10, rotate like logs

### Errors
Per [[004-error-handling-and-logging|ADR-004]] fail-fast: single attempt, manual retry
No GPU fallback (surfaces config issues)
Empty transcription → "No speech detected" notification

### Post-Processing
Trim whitespace, collapse multiple spaces, preserve capitalization/punctuation

### Concurrency
One transcription at a time, block new recordings

### Code
Platform adapters:
- `Whisper/Types.hs` - shared types
- `Whisper/Backend.hs` - interface, shared logic (timeout, files)
- `Whisper/FasterWhisper.hs` / `Whisper/WhisperCpp.hs` - platform-specific

## Options Considered

### Model Selection
- **Support all sizes** (Selected) - Maximum flexibility, both tools support all sizes
- Limited subset - Unnecessary restriction
- Medium only - Too limiting

### Model Validation
- **Startup validation** (Selected) - Fail fast with clear errors, consistent with [[004-error-handling-and-logging|ADR-004]]
- Lazy validation - Delayed feedback, confusing UX
- No validation - Silent failures

### Model Download
- **Hybrid approach** (Selected) - Addresses platform differences appropriately
- Provide scripts for both - Over-engineering for Linux auto-download
- Document only - Less helpful for macOS complexity

### Transcription Parameters
- **Hardcode all** (Selected) - Simplicity, prevents misconfiguration
- Language configurable - Unnecessary (English-only use case)
- Advanced parameters - Deferred per [[003-configuration-and-settings-storage|ADR-003]]

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
- **Fail immediately** (Selected) - Fast feedback, aligns with [[004-error-handling-and-logging|ADR-004]]
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
- Startup validation catches issues early
- Platform abstraction clean
- Retained audio aids debugging
- Timeout prevents hangs
- JSON provides stable interface

### Negative
- Model loading overhead per transcription (1-5s)
- No GPU fallback or concurrent transcription
- Platform-specific downloads
- Audio files use ~3MB disk

## Implementation Notes

### Startup Validation Commands

**Linux:**
```bash
# Check faster-whisper Python library installed
python -c "import faster_whisper"

# Check custom wrapper script exists
test -f /usr/local/lib/whisper-input/whisper-transcribe.py
```

**macOS:**
```bash
# Check whisper.cpp binary
which whisper-cpp

# Check model file exists
ls ~/.whisper-models/ggml-medium.bin
```

### Transcription Commands

**Linux (faster-whisper via custom Python wrapper):**
```bash
# Custom wrapper script: ./scripts/whisper-transcribe.py
# Takes: audio_file model_name device device_index
# Outputs: JSON with transcription text
python /usr/local/lib/whisper-input/whisper-transcribe.py /path/to/audio.wav medium.en cuda 0
```

**macOS (whisper.cpp):**
```bash
# See [[002-technology-stack-selection|ADR-002]] for full command details
whisper-cpp -m /path/to/model -f /path/to/audio.wav -l en --output-json
```

### Model Download Script (macOS)

`./scripts/download-model-macos.sh`:
- Wraps whisper.cpp download scripts
- Places models in `~/.whisper-models/`
- Updates config template with correct paths

### Notification Messages

**Errors:**
- "Transcription failed" (critical)
- "Model 'medium' not found" (critical, on startup)
- "GPU initialization failed" (critical)

**Info:**
- "No speech detected" (low priority)

## Related Decisions
- [[001-system-architecture-and-component-structure|ADR-001: System Architecture]] - Monolithic architecture simplifies subprocess orchestration
- [[002-technology-stack-selection|ADR-002: Technology Stack]] - Established faster-whisper (via custom Python wrapper) and whisper.cpp as platform-specific choices
- [[003-configuration-and-settings-storage|ADR-003: Configuration & Settings Storage]] - Model configuration follows Dhall pattern with platform-specific sections
- [[004-error-handling-and-logging|ADR-004: Error Handling & Logging]] - Startup validation, fail-fast philosophy, notification strategy

## Notes

**Custom Python Wrapper (Linux):**
The faster-whisper library is a Python API, not a standalone CLI tool. Rather than depending on unmaintained third-party CLI wrappers, we install a minimal Python script at a fixed path (`/usr/local/lib/whisper-input/whisper-transcribe.py`). This wrapper:
- Accepts command-line arguments (audio file, model, device, device index)
- Uses the faster-whisper Python API
- Outputs JSON with transcription text to stdout
- Provides full control over the interface
- Remains simple and maintainable (single-file script)
