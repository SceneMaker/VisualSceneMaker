# Plugin Descriptor Usage

This document describes the intended structure and use of
`sceneflow-interface.json` files inside plugin folders.

## Location

Each plugin may provide a descriptor at:

```
plugins/<plugin>/sceneflow-interface.json
```

## Purpose

The descriptor is used to make sceneflow modeling easier by exposing:

- Which commands a plugin accepts
- Which parameters each command supports
- Which sceneflow variables are written or read
- Which plugin config keys exist
- High-level category (input/output/processing/mixed)

## Top-level structure

```
{
  "schemaVersion": "1.0",
  "generated": true|false,
  "plugin": { ... },
  "categories": { ... },
  "commands": [ ... ],
  "writes": [ ... ],
  "reads": [ ... ],
  "config": [ ... ]
}
```

## Fields

### plugin

```
{
  "id": "plugin-folder-name",
  "name": "Human readable name",
  "className": "Primary executor class (optional)",
  "executors": ["List of executor classes"],
  "description": "Short description",
  "tags": ["input","websocket","robot"]
}
```

### categories

```
{
  "primary": "input|output|processing|mixed|unknown",
  "secondary": ["input","output"],
  "confidence": 0.0,
  "inferred": true|false,
  "reason": ["io:inbound","io:outbound","logic:state"]
}
```

### commands

Each command describes how to call a plugin action.

```
[
  {
    "name": "play",
    "summary": "Play a media file",
    "type": "action|speech|event|system",
    "blocking": "true|false|default",
    "aliases": ["start"],
    "params": [
      {
        "name": "file",
        "type": "string|int|float|bool|enum|list|struct|ref",
        "required": true,
        "default": "",
        "enum": ["a","b"],
        "refType": "string",
        "description": "Relative path"
      }
    ],
    "examples": [
      {
        "playAction": "PlayAction(\"[agent play file='demo.mp4']\")",
        "notes": "Uses media root"
      }
    ]
  }
]
```

### writes

Variables written by the plugin.

```
[
  {
    "var": "unitytrigger",
    "type": "string|int|float|bool|list|struct",
    "scope": "global|local",
    "description": "Trigger emitted by Unity",
    "when": "On #TRIGGER# feedback",
    "exampleValue": "gaze_left"
  }
]
```

### reads

Variables read by the plugin (optional, for completeness).

```
[
  {
    "var": "someVar",
    "type": "string",
    "scope": "global",
    "description": "Used to compose payload"
  }
]
```

### config

Plugin configuration keys read from `mConfig.getProperty(...)`.

```
[
  {
    "key": "port",
    "type": "int|string|bool|path",
    "required": true,
    "default": "8080",
    "description": "WebSocket port"
  }
]
```

## Authoring guidance

- Keep command names exact (case-insensitive at runtime, but prefer canonical).
- Add parameter types and enums where possible.
- Mark `"generated": false` after manual edits to prevent full overwrites.
- Use the generator for partial updates:
  - `-PupdateDescriptorSections=commands,writes,reads,config`

## Descriptor maintenance workflow

1. Generate draft descriptors with `./gradlew generatePluginDescriptors`.
2. Manually refine entries (types, descriptions, examples, categories).
3. Set `"generated": false` after manual edits.
4. Use partial updates for machine-derived sections only:
   `./gradlew generatePluginDescriptors -PupdateDescriptorSections=commands,writes,reads,config`

## Editor/runtime usage (intended)

- Editor uses `commands` + `params` for autocomplete and validation.
- Editor can display `writes` and `config` hints to users.
- Runtime can fall back to legacy mode if a descriptor is missing.

## Example: Timer plugin

This is a short but realistic example for `plugins/timer/sceneflow-interface.json`:

```
{
  "schemaVersion": "1.0",
  "generated": false,
  "plugin": {
    "id": "timer",
    "name": "Timer",
    "className": "TimerExecutor",
    "executors": ["TimerExecutor"],
    "description": "Simple time utilities for sceneflow variables",
    "tags": ["processing", "utility"]
  },
  "categories": {
    "primary": "processing",
    "secondary": [],
    "confidence": 0.9,
    "inferred": false,
    "reason": ["logic:state"]
  },
  "commands": [
    {
      "name": "clear",
      "summary": "Reset all timers",
      "type": "action",
      "params": [],
      "examples": [
        { "playAction": "PlayAction(\"[timer clear]\")" }
      ]
    },
    {
      "name": "init",
      "summary": "Start a timer",
      "type": "action",
      "params": [
        {
          "name": "id",
          "type": "string",
          "required": true,
          "description": "Timer id"
        }
      ],
      "examples": [
        { "playAction": "PlayAction(\"[timer init id='t1']\")" }
      ]
    },
    {
      "name": "time",
      "summary": "Elapsed time since init",
      "type": "action",
      "params": [
        { "name": "id", "type": "string", "required": true },
        {
          "name": "var",
          "type": "string",
          "required": true,
          "description": "Target variable"
        }
      ],
      "examples": [
        { "playAction": "PlayAction(\"[timer time id='t1' var='elapsed']\")" }
      ]
    },
    {
      "name": "systime",
      "summary": "Store system time in ms",
      "type": "action",
      "params": [
        { "name": "var", "type": "string", "required": true }
      ]
    },
    {
      "name": "timediff",
      "summary": "Difference to a given timestamp",
      "type": "action",
      "params": [
        { "name": "lasttime", "type": "string", "required": true },
        { "name": "var", "type": "string", "required": true }
      ]
    },
    {
      "name": "day",
      "summary": "Short day name (locale)",
      "type": "action",
      "params": [
        { "name": "var", "type": "string", "required": true }
      ]
    },
    {
      "name": "dayverbose",
      "summary": "Full day name (locale)",
      "type": "action",
      "params": [
        { "name": "var", "type": "string", "required": true }
      ]
    },
    {
      "name": "partofday",
      "summary": "Time-of-day bucket",
      "type": "action",
      "params": [
        { "name": "var", "type": "string", "required": true }
      ]
    }
  ],
  "writes": [
    {
      "var": "<dynamic>",
      "type": "string",
      "scope": "global",
      "description": "Value assigned to the variable named by the 'var' param"
    }
  ],
  "reads": [],
  "config": []
}
```
