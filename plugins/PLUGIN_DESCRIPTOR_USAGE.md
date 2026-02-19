# Plugin Descriptor Usage

This document describes the `plugin-properties.json` format used by VSM plugins.

## Location

Each plugin provides:

```
plugins/<plugin>/src/main/resources/plugin-properties.json
```

## Purpose

The descriptor is used by the web editor/runtime integration to expose:

- Plugin metadata (id, name, class)
- Android compatibility flag
- Config keys (required/optional)
- Commands and parameters
- Variable read/write hints

## Top-level structure

```
{
  "schemaVersion": "1.0",
  "plugin": { ... },
  "categories": { ... },
  "config": { ... },
  "commands": [ ... ],
  "variables": { ... }
}
```

## Plugin metadata

```
"plugin": {
  "id": "timer",
  "name": "Timer",
  "className": "de.dfki.vsm.xtension.timer.TimerExecutor",
  "androidCompatible": true,
  "description": "...",
  "tags": ["timer", "utility"]
}
```

`androidCompatible` must be set explicitly to `true` or `false`.

## Config section

```
"config": {
  "required": [ { "name": "key", "type": "string" } ],
  "optional": [ { "name": "other", "type": "string", "default": "" } ]
}
```

Optional `agent` config can be provided as nested object when needed.

## Commands section

```
"commands": [
  {
    "name": "start",
    "type": "action",
    "summary": "Start runtime behavior",
    "params": [
      { "name": "id", "type": "string", "required": false }
    ]
  }
]
```

## Variables section

```
"variables": {
  "writes": [ { "var": "state", "type": "String", "scope": "global" } ],
  "reads":  [ { "var": "input", "type": "String", "scope": "global" } ]
}
```

## Notes

- Keep `schemaVersion` at `"1.0"`.
- Descriptors are maintained manually.
- `sceneflow-interface.json` generator workflow is deprecated and removed.
