# ADR 003: Configuration & Settings Storage

## Status
Accepted

## Context
Whisper Input needs a configuration system for user-customizable settings. As a personal tool running on both Linux and macOS, the configuration must:
- Be easy to understand and edit manually
- Support platform-specific settings (different hotkeys, GPU configurations)
- Provide sensible defaults for first-run experience
- Handle missing or incomplete configurations gracefully
- Integrate well with Haskell's type system

Initial scope covers three essential setting categories:
1. **Hotkey configuration** - What key combination triggers voice recording
2. **Audio settings** - Which microphone device to use
3. **Whisper model settings** - Model size and GPU device selection

Key considerations:
- **Personal use only** - No need for complex validation or migration systems
- **Type safety** - Leverage Haskell's strengths to catch config errors early
- **Platform differences** - Linux uses CUDA device numbers, macOS uses Metal/ANE; different hotkey conventions
- **Simplicity over flexibility** - Avoid over-engineering for edge cases
- **Infrequent changes** - Configuration set once at setup, rarely modified afterward

## Decision

### Configuration Format: Dhall
Use **Dhall** as the configuration language with strong typing and validation.

### Configuration Location: XDG Base Directory
Store configuration at:
- **Linux & macOS:** `~/.config/whisper-input/config.dhall`
- Follow XDG Base Directory specification (`$XDG_CONFIG_HOME` or `~/.config`)

### Configuration Schema
Platform-specific sections with explicit settings per OS:

```dhall
{ linux =
    { hotkey = "Super+Shift+Space"
    , audio = { microphone_device = "default" }
    , whisper =
        { model = "medium"
        , gpu_device = 0  -- CUDA device number
        }
    }
, macos =
    { hotkey = "Cmd+Shift+Space"
    , audio = { microphone_device = "default" }
    , whisper =
        { model = "medium"
        , use_gpu = True  -- Enable Metal/ANE acceleration
        }
    }
}
```

### Audio Settings (Minimal)
- **Microphone device only** - Device name or "default"
- **Hardcoded parameters:**
  - Sample rate: 16kHz (Whisper optimized)
  - Channels: Mono
  - Bit depth: 16-bit PCM

### GPU Configuration (Platform-Specific)
- **Linux:** `gpu_device` field with CUDA device number (0, 1, 2...)
- **macOS:** `use_gpu` boolean (True = Metal/ANE, False = CPU)

### Default Configuration Behavior
- **Generate default config on first run** if `config.dhall` doesn't exist
- **Use sensible defaults:**
  - Hotkey: `Super+Shift+Space` (Linux), `Cmd+Shift+Space` (macOS)
  - Microphone: `"default"` (system default device)
  - Model: `"medium"` (balance of speed/accuracy)
  - GPU: Enabled (device 0 on Linux, Metal on macOS)
- **Fail on invalid config** - If config exists but is malformed, Dhall type errors prevent startup

### Configuration Reload
- **Restart required** - Changes take effect after application restart
- No hot-reloading or file watching

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
- ✅ **Standard** - Follows Linux/Unix conventions
- ✅ **Organized** - Keeps `~/.config` clean with directory per app
- ✅ **Works on macOS** - XDG standard respected by many tools
- ✅ **Expandable** - Room for multiple config files later (models, logs)
- ✅ **Respects `$XDG_CONFIG_HOME`** - User can override location

**Cons:**
- Slightly more complex path

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

### Hotkey Configuration

#### Option 1: Platform-specific sections (Selected)
```dhall
{ linux = { hotkey = "Super+Shift+Space" }
, macos = { hotkey = "Cmd+Shift+Space" }
}
```

**Pros:**
- ✅ **Explicit** - No confusion about what key triggers on which platform
- ✅ **Type-safe** - Dhall enforces both sections exist
- ✅ **No surprises** - User sees exact hotkey per platform
- ✅ **Flexible** - Different hotkeys per platform if desired
- ✅ **No translation logic** - Direct mapping to platform APIs

**Cons:**
- Must configure both platforms even if similar

#### Option 2: Unified format with translation
`hotkey = "Ctrl+Shift+Space"` → translate Ctrl→Cmd on macOS

**Pros:**
- Single configuration
- Simpler for identical hotkeys

**Cons:**
- ❌ **Hidden complexity** - Translation rules not obvious
- ❌ **Surprising** - User might not expect Ctrl→Cmd mapping
- ❌ **Platform conventions differ** - Super vs Cmd have different semantics
- ❌ **Edge cases** - What about platform-specific modifiers?

#### Option 3: Raw keycodes
Specify scancodes/keycodes directly

**Pros:**
- Maximum control

**Cons:**
- ❌ **Extremely unfriendly** - User must look up keycodes
- ❌ **Unmaintainable** - Hard to remember what `0x39` means
- ❌ **Overkill** - No benefit for simple hotkeys

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
- ✅ **Type safety** - Dhall catches config errors before runtime
- ✅ **Excellent error messages** - Clear feedback on malformed configs
- ✅ **Zero-config first run** - Works out of box with sensible defaults
- ✅ **Fully customizable** - Generated config file easy to edit
- ✅ **Platform-explicit** - No hidden translation logic or surprises
- ✅ **Simple implementation** - Minimal code, restart-based reload
- ✅ **Prevents misconfiguration** - Hardcoded audio params avoid footguns
- ✅ **Standard location** - XDG Base Directory compliance
- ✅ **Haskell-friendly** - Excellent `dhall` library integration

### Negative
- Dhall less familiar than YAML (minor - user is technical)
- Must implement default config generation logic
- Restart required for config changes (acceptable - rare occurrence)
- Platform-specific sections more verbose (worthwhile for explicitness)

### Neutral
- Config directory grows if we add more files later (acceptable, organized)
- Dhall adds dependency (small, stable library)

## Implementation Notes

### Default Configuration Template

```dhall
-- ~/.config/whisper-input/config.dhall
-- Generated on first run, customize as needed

{ linux =
    { hotkey = "Super+Shift+Space"
    , audio = { microphone_device = "default" }
    , whisper =
        { model = "medium"  -- Options: tiny, base, small, medium, large, large-v3
        , gpu_device = 0    -- CUDA device number (0 for first GPU)
        }
    }
, macos =
    { hotkey = "Cmd+Shift+Space"
    , audio = { microphone_device = "default" }
    , whisper =
        { model = "medium"
        , use_gpu = True    -- Enable Metal/ANE acceleration
        }
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
  { hotkey :: Text
  , audio :: AudioConfig
  , whisper :: LinuxWhisperConfig
  } deriving (Generic, FromDhall)

data MacOSConfig = MacOSConfig
  { hotkey :: Text
  , audio :: AudioConfig
  , whisper :: MacOSWhisperConfig
  } deriving (Generic, FromDhall)

data AudioConfig = AudioConfig
  { microphone_device :: Text
  } deriving (Generic, FromDhall)

data LinuxWhisperConfig = LinuxWhisperConfig
  { model :: Text
  , gpu_device :: Natural
  } deriving (Generic, FromDhall)

data MacOSWhisperConfig = MacOSWhisperConfig
  { model :: Text
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

## Future Considerations

Settings deferred to later ADRs:
- **Text injection behavior** - Typing speed, special character handling
- **File paths** - Model directory, temp files, log location
- **Logging configuration** - Log levels, output destinations
- **Advanced Whisper parameters** - Temperature, language hints, beam size

These can be added to the config schema incrementally without breaking changes (Dhall supports backward-compatible schema evolution).
