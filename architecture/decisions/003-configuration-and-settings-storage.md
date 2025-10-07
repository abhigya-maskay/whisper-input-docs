# ADR 003: Configuration & Settings Storage

## Status
Accepted

## Context
Configuration for audio, Whisper models, and logging. Requirements:
- Manual editing support
- Platform-specific settings (GPU configs differ)
- Type safety
- Sensible defaults

Note: Hotkeys configured externally (Hyprland/skhd, see ADR-006)

## Decision

### Format: Dhall (type-safe with validation)

### Location: `~/.config/whisper-input/config.dhall` (XDG compliant)

### Schema

```dhall
{ linux =
    { audio = { microphone_device = "default" }
    , whisper =
        { model = "medium.en"
        , model_path = None Text  -- Optional: override model file path
        , gpu_device = 0  -- CUDA device number
        }
    , log_level = "INFO"  -- DEBUG, INFO, WARN, ERROR
    }
, macos =
    { audio = { microphone_device = "default" }
    , whisper =
        { model = "medium.en"
        , model_path = None Text  -- Optional: override model file path
        , use_gpu = True  -- Enable Metal/ANE acceleration
        }
    , log_level = "INFO"  -- DEBUG, INFO, WARN, ERROR
    }
}
```

### Settings
- **Audio:** Microphone device only (format hardcoded: 16kHz, mono, 16-bit PCM)
- **Whisper:** Model name (tiny/base/small/medium/large/large-v3), optional path override
- **GPU:** Linux uses CUDA device number, macOS uses boolean (Metal on/off)
- **Logging:** DEBUG/INFO/WARN/ERROR

### Defaults
Generated on first run: default mic, medium model, GPU enabled, INFO logging
Fail on malformed config (Dhall type errors)

### Reload
Restart required (no hot-reload)

## Options Considered

### Configuration Format

#### Option 1: Dhall (Selected)
**Pros:**
- ✅ **Type-safe** - Strong typing catches errors before runtime
- ✅ **Validation built-in** - Schema enforcement via Dhall types
- ✅ **Haskell integration** - Excellent `dhall` library with automatic decoding
- ✅ **Programmable** - Can use functions, imports, and expressions if needed
- ✅ **No invalid states** - Type system prevents malformed configs
- ✅ **Good error messages** - Clear type mismatch errors

**Cons:**
- Less common than YAML/TOML (but user is technical)
- Requires learning Dhall syntax (minimal for simple configs)

#### Option 2: YAML
**Pros:**
- Very human-friendly and readable
- Ubiquitous, familiar to most developers
- Good Haskell libraries (`yaml` package)

**Cons:**
- ❌ **No type safety** - Easy to write invalid configs
- ❌ **Runtime validation needed** - Must implement schema checks manually
- ❌ **Surprising syntax** - Indentation-sensitive, special characters
- ❌ **Security issues** - YAML parsing historically problematic

#### Option 3: TOML
**Pros:**
- Readable and strict
- Type-aware format
- Common in modern tools

**Cons:**
- Less type-safe than Dhall
- Still requires manual validation in Haskell
- Platform-specific sections less elegant

#### Option 4: JSON
**Pros:**
- Ubiquitous, simple parser
- Good Haskell support (`aeson`)

**Cons:**
- ❌ **Not human-friendly** - Trailing commas, quoted keys, no comments
- ❌ **No type safety** - Manual validation required
- ❌ **Poor for hand-editing** - Easy to make syntax errors

### Configuration Location

#### Option 1: XDG Base Directory (Selected)
`~/.config/whisper-input/config.dhall`

**Pros:**
- ✅ **Standard** - Follows XDG Base Directory specification for configuration files
- ✅ **Organized** - Keeps `~/.config` clean with directory per app
- ✅ **Cross-platform** - Works on Linux and macOS
- ✅ **Expandable** - Room for multiple config files later
- ✅ **Respects `$XDG_CONFIG_HOME`** - User can override location

**Cons:**
- Slightly more complex path

**Note:** XDG Base Directory specification separates concerns:
- Config: `$XDG_CONFIG_HOME` (default `~/.config`)
- State/logs: `$XDG_STATE_HOME` (default `~/.local/state`) - see ADR-004
- Runtime: `$XDG_RUNTIME_DIR` - see ADR-006

#### Option 2: Home directory dotfile
`~/.whisper-input.dhall`

**Pros:**
- Simpler path
- Traditional Unix approach

**Cons:**
- Clutters home directory
- No room for multiple config files
- Less modern

#### Option 3: Platform-specific locations
Linux: `~/.config/whisper-input/config.dhall`
macOS: `~/Library/Application Support/whisper-input/config.dhall`

**Pros:**
- Native to each platform

**Cons:**
- ❌ **Unnecessary complexity** - Adds platform-specific logic
- ❌ **User confusion** - Different paths per platform
- ❌ **No benefit** - XDG works fine on macOS

### Audio Settings Granularity

#### Option 1: Minimal - Device only (Selected)
Configure microphone device, hardcode format parameters.

**Pros:**
- ✅ **Simple** - Only one setting to understand
- ✅ **Prevents misconfiguration** - Can't set wrong sample rate
- ✅ **Whisper-optimized** - 16kHz is ideal, no reason to change
- ✅ **Fewer bugs** - Less surface area for errors

**Cons:**
- Less flexibility (but no use case for it)

#### Option 2: Moderate - Device + sample rate
Allow overriding sample rate.

**Pros:**
- Flexibility for experimentation

**Cons:**
- ❌ **Unnecessary** - 16kHz is optimal for Whisper
- ❌ **Footgun** - User might set 44.1kHz and wonder why it's slow
- ❌ **More validation** - Need to check valid rates

#### Option 3: Full control - All audio parameters
Device, sample rate, channels, bit depth all configurable.

**Pros:**
- Maximum flexibility

**Cons:**
- ❌ **Complexity** - Many ways to break it
- ❌ **No benefit** - Whisper expects specific format
- ❌ **Poor UX** - User must understand audio formats

### GPU Configuration

#### Option 1: Platform-specific (Selected)
```dhall
{ linux = { gpu_device = 0 }    -- CUDA device number
, macos = { use_gpu = True }    -- Metal on/off
}
```

**Pros:**
- ✅ **Explicit platform differences** - Clear what each means
- ✅ **Type-safe** - Dhall enforces correct structure per platform
- ✅ **Matches underlying APIs** - CUDA uses device numbers, Metal is auto
- ✅ **No confusion** - User knows exactly what's configured

**Cons:**
- Slightly more verbose

#### Option 2: Unified with interpretation
`gpu_device = Some 0` (None = CPU, Some n = GPU)

**Pros:**
- Single field
- Works across platforms

**Cons:**
- ❌ **Unclear semantics** - What does "device 1" mean on macOS?
- ❌ **Hidden translation** - Number ignored on macOS
- ❌ **Less explicit** - Doesn't match platform reality

#### Option 3: Boolean + optional device
```dhall
{ use_gpu = True
, gpu_device = Some 0
}
```

**Pros:**
- Unified structure

**Cons:**
- ❌ **Confusion** - When does `gpu_device` matter?
- ❌ **Invalid states** - What if `use_gpu = False` but `gpu_device = Some 0`?
- ❌ **Less type-safe** - Can't enforce "Linux needs device number"

### Missing/Incomplete Configuration

#### Option 1: Fail fast
Require complete config file, error if missing.

**Pros:**
- Explicit configuration
- No hidden defaults

**Cons:**
- ❌ **Poor first-run UX** - Must write config before trying tool
- ❌ **Annoying** - Can't just install and run

#### Option 2: Sensible defaults + generated config (Selected)
Create default config on first run, use reasonable defaults.

**Pros:**
- ✅ **Great first-run experience** - Works immediately
- ✅ **Discoverable** - User can see and edit generated config
- ✅ **Explicit** - Real file with all settings visible
- ✅ **Type-safe** - Once generated, Dhall enforces schema
- ✅ **Best of both worlds** - Works out-of-box, fully customizable

**Cons:**
- Must implement default config generation

#### Option 3: Interactive setup
Prompt user to create config on first run.

**Pros:**
- Guided configuration

**Cons:**
- ❌ **Friction** - Slows down first run
- ❌ **CLI complexity** - Prompts in a daemon tool?
- ❌ **Unnecessary** - Defaults work fine, user can edit later

### Configuration Reload

#### Option 1: Restart required (Selected)
App must restart to pick up config changes.

**Pros:**
- ✅ **Simple implementation** - Load config once at startup
- ✅ **Predictable** - No edge cases with partial reloads
- ✅ **Infrequent changes** - Config rarely modified after setup
- ✅ **Easy to restart** - systemd/launchd restart is trivial
- ✅ **Functional paradigm** - Immutable config fits Haskell nicely
- ✅ **No threading complexity** - No file watching needed

**Cons:**
- Less convenient for rapid config testing (minor issue)

#### Option 2: Hot reload
Watch config file and reload on changes.

**Pros:**
- Nice UX for config experimentation

**Cons:**
- ❌ **Complexity** - File watching, threading, inotify/fswatch
- ❌ **Edge cases** - What if reload fails mid-run?
- ❌ **Dependencies** - File watching library (fsnotify, hinotify)
- ❌ **Rare use case** - Config changes infrequent
- ❌ **Overkill** - Not worth it for personal tool

#### Option 3: Manual reload command
User sends signal or CLI command to trigger reload.

**Pros:**
- Middle ground - explicit but no restart

**Cons:**
- Still need reload logic and error handling
- More complex than restart
- Marginal benefit over restart

## Consequences

### Positive
- Type safety catches errors before runtime
- Zero-config first run with sensible defaults
- Hardcoded audio params prevent misconfiguration
- XDG compliant

### Negative
- Restart required for changes
- Platform-specific sections add verbosity

## Implementation Notes

### Default Configuration Template

```dhall
-- ~/.config/whisper-input/config.dhall
-- Generated on first run, customize as needed
-- Note: Hotkeys are configured externally (see ADR-006)
--   Linux: ~/.config/hypr/hyprland.conf
--   macOS: ~/.skhdrc

{ linux =
    { audio = { microphone_device = "default" }
    , whisper =
        { model = "medium.en"        -- Options: tiny, base, small, medium, large, large-v3
        , model_path = None Text     -- Optional: custom model path
        , gpu_device = 0             -- CUDA device number (0 for first GPU)
        }
    , log_level = "INFO"             -- Options: DEBUG, INFO, WARN, ERROR
    }
, macos =
    { audio = { microphone_device = "default" }
    , whisper =
        { model = "medium.en"        -- Options: tiny, base, small, medium, large, large-v3
        , model_path = None Text     -- Optional: custom model path
        , use_gpu = True             -- Enable Metal/ANE acceleration
        }
    , log_level = "INFO"             -- Options: DEBUG, INFO, WARN, ERROR
    }
}
```

### Haskell Integration

Use `dhall` package with automatic decoding:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Dhall

data Config = Config
  { linux :: LinuxConfig
  , macos :: MacOSConfig
  } deriving (Generic, FromDhall)

data LinuxConfig = LinuxConfig
  { audio :: AudioConfig
  , whisper :: LinuxWhisperConfig
  , log_level :: Text
  } deriving (Generic, FromDhall)

data MacOSConfig = MacOSConfig
  { audio :: AudioConfig
  , whisper :: MacOSWhisperConfig
  , log_level :: Text
  } deriving (Generic, FromDhall)

data AudioConfig = AudioConfig
  { microphone_device :: Text
  } deriving (Generic, FromDhall)

data LinuxWhisperConfig = LinuxWhisperConfig
  { model :: Text
  , model_path :: Maybe Text
  , gpu_device :: Natural
  } deriving (Generic, FromDhall)

data MacOSWhisperConfig = MacOSWhisperConfig
  { model :: Text
  , model_path :: Maybe Text
  , use_gpu :: Bool
  } deriving (Generic, FromDhall)

loadConfig :: IO Config
loadConfig = input auto "./config.dhall"
```

### Hardcoded Audio Parameters

```haskell
-- Never exposed in config file
audioSampleRate :: Int
audioSampleRate = 16000  -- 16kHz

audioChannels :: Int
audioChannels = 1  -- Mono

audioBitDepth :: Int
audioBitDepth = 16  -- 16-bit PCM
```

### First-Run Behavior

```haskell
ensureConfig :: IO ()
ensureConfig = do
  configDir <- getXdgDirectory XdgConfig "whisper-input"
  createDirectoryIfMissing True configDir

  let configPath = configDir </> "config.dhall"
  exists <- doesFileExist configPath

  unless exists $ do
    writeFile configPath defaultConfigTemplate
    putStrLn $ "Created default config at: " ++ configPath
```

### Configuration Reload Process

User workflow for config changes:
1. Edit `~/.config/whisper-input/config.dhall`
2. Restart application:
   - Linux systemd: `systemctl --user restart whisper-input`
   - macOS launchd: `launchctl unload/load ~/Library/LaunchAgents/...`
   - Manual: Kill process and restart

## Related Decisions
- **ADR-001**: System Architecture - Monolithic application loads config once at startup
- **ADR-002**: Technology Stack - Haskell chosen, Dhall is natural fit with excellent library support
- **ADR-004**: Error Handling & Logging - Log level configuration with environment variable override
- **ADR-005**: Local Whisper Model Integration - Model path override for custom model locations
- **ADR-006**: Hotkey & Audio Capture - Hotkeys configured externally in Hyprland/skhd, not in application config

## Future Considerations

Settings deferred to later ADRs:
- **Text injection behavior** - Typing speed, special character handling
- **File paths** - Model directory, temp files, log location
- **Logging configuration** - Log levels, output destinations
- **Advanced Whisper parameters** - Temperature, language hints, beam size

These can be added to the config schema incrementally without breaking changes (Dhall supports backward-compatible schema evolution).
