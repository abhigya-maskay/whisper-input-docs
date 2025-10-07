# ADR 001: System Architecture & Component Structure

## Status
Accepted

## Context
Whisper Input is a personal voice-to-text input tool for local use only. The system needs to capture audio, process it through speech recognition, and deliver text to applications on a single machine.

Key considerations:
- **Personal use only** - no deployment, scaling, or multi-user concerns
- **Offline-first** - all processing happens locally for privacy
- Need to handle audio capture from system microphone
- Integration with local Whisper model for speech recognition
- Text delivery to active applications/text fields
- User interface for control and configuration
- System tray/background operation
- Maintainability for a single developer/user
- Simplicity over flexibility

## Decision
**We will use a simple monolithic application architecture** (Option 1).

The system will be a single desktop application with well-defined internal modules for audio capture, speech recognition, text injection, UI, and state management. This approach prioritizes simplicity and maintainability - the right fit for a personal tool.

## Options Considered

### Option 1: Simple Monolithic Application
A single desktop application with internal modules:
- **Audio Capture**: Microphone input and audio buffering
- **Speech Recognition**: Local Whisper model integration
- **Text Injection**: Keyboard simulation/clipboard integration
- **UI**: System tray, hotkeys, and settings
- **State Management**: Simple in-memory state

**Pros:**
- Minimal complexity - easiest to build and maintain
- Single codebase, single process
- Simple debugging
- No IPC overhead
- Perfect for personal use

**Cons:**
- Less modular than alternatives
- All components share same runtime

### Option 2: Separate UI and Background Service
Two processes:
- Background service: audio capture + recognition + text injection
- UI application: settings and controls

**Pros:**
- Can close UI while service runs
- UI crashes don't affect core functionality
- Cleaner separation

**Cons:**
- Need IPC mechanism
- More complex for single-user scenario
- Overkill for personal use

### Option 3: Script-Based with Loose Components
Collection of scripts tied together:
- Audio capture script
- API caller script
- Text injection script
- Simple launcher

**Pros:**
- Very flexible for experimentation
- Easy to modify individual pieces

**Cons:**
- Harder to create polished UI
- More coordination overhead
- Less cohesive user experience

## Consequences

- Fast to develop and iterate
- Easy to understand the entire system at once
- Good fit for desktop application frameworks (Electron, Tauri, Python + Qt, etc.)
- Single codebase simplifies debugging and maintenance
- Can refactor to multi-process later if requirements change (unlikely for personal use)
- All components will need to be compatible with the chosen tech stack

