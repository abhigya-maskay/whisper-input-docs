# ADR 001: System Architecture & Component Structure

## Status
Accepted

## Context
Personal voice-to-text tool for local use. Requirements:
- Offline audio capture, speech recognition, text injection
- System tray/background operation
- Single developer maintainability
- Simplicity over flexibility

## Decision
**Single binary with daemon+client process model**

Two modes:
- **Daemon mode**: Background process managing state and orchestrating audio capture, speech recognition, text injection
- **Command mode**: Lightweight IPC clients for hotkey triggers

Single codebase. Daemon maintains state; commands are instant and lightweight.

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

- Single codebase simplifies development and debugging
- Daemon maintains state across recordings (idle/recording/transcribing)
- Lightweight IPC enables instant hotkey response
- Unix domain socket for start/stop commands

