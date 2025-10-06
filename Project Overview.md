# Whisper-Input Project Overview

## Project Summary
A system-wide voice dictation tool using OpenAI's Whisper model running locally. Allows voice-to-text input in any text field across Linux and macOS systems.

**Status:** Planning Phase
**Primary User:** Personal use
**Main Repository:** [whisper-input](https://github.com/abhigya-maskay/whisper-input)

---

## Core Objectives

### Primary Goal
Enable reliable voice dictation into any text box on Linux and macOS machines using local Whisper model processing.

### Key Principles
- **Offline-first**: Must function without internet connection
- **Privacy-focused**: All processing happens locally, no data sent to external services
- **Accuracy over speed**: Prioritize transcription quality, but maintain practical usability for daily work
- **Simplicity**: Focus on core dictation functionality

---

## Technical Stack

### Language
- **Haskell** - Primary development language

### Platform Support
- **Current Focus:** Linux (Wayland support required)
- **Next:** macOS

### Core Technology
- **Whisper Model** - Running locally (not via API)
- No specific frameworks/libraries selected yet

---

## Feature Specifications

### Core Features
- **Push-to-talk activation** - User-triggered recording
- **Record-then-transcribe** - Audio recording followed by transcription
- **System-wide text insertion** - Works in any text field
- **English language support** - Single language
- **Wayland compatibility** - Required for Linux support

