# Semantic Annotations Schema v2

This document defines the target JSON format stored in `semantic-annotations.json`.

## Goals

- Keep annotations in a separate project file.
- Preserve backward compatibility with existing v1 payloads.
- Add explicit provenance per layer for hybrid processing:
  - `basic` (S/V/O) via UD parser (planned).
  - `dialogueAct` and `themeRheme` via LLM.
- Prepare storage for future inline semantic markup in script text.

## Top-level document

```json
{
  "version": 2,
  "schema": {
    "id": "vsm.semantic.annotations",
    "version": 2
  },
  "scriptHash": "sha256:...",
  "generatedAt": "2026-02-11T18:00:59.822035Z",
  "updatedAt": "2026-02-11T18:00:59.822071Z",
  "provenance": {
    "source": "editor-web-ui",
    "service": "semantic-ud|llm|hybrid|...",
    "model": "stanza-de|gpt-5.2|...",
    "analyzedAt": "2026-02-11T18:00:59.822071Z",
    "layers": {
      "basic": "ud|llm|heuristic|unknown",
      "dialogueAct": "llm|heuristic|unknown",
      "themeRheme": "llm|heuristic|unknown"
    }
  },
  "annotations": []
}
```

## Annotation object

Each item in `annotations` keeps existing fields and may include richer layer metadata.

```json
{
  "id": "line5_ann1",
  "line": 5,
  "speaker": "Anne",
  "text": "Willkommen!",
  "basic": {
    "subject": { "text": "...", "from": 0, "to": 0, "confidence": 0.0 },
    "verb": { "text": "...", "from": 0, "to": 0, "confidence": 0.0 },
    "object": { "text": "...", "from": 0, "to": 0, "confidence": 0.0 }
  },
  "dialogueAct": {
    "label": "greeting",
    "scheme": "dailydialog-v1",
    "confidence": 0.93
  },
  "themeRheme": {
    "theme": "Willkommen",
    "rheme": "",
    "confidence": 0.62
  },
  "provenance": {
    "analyzedAt": "2026-02-11T18:00:59.822071Z",
    "layers": {
      "basic": "ud|llm|heuristic|unknown",
      "dialogueAct": "llm|heuristic|unknown",
      "themeRheme": "llm|heuristic|unknown"
    }
  },
  "markup": {
    "inline": [],
    "embedded": []
  }
}
```

## Markup usage planning

`markup` is reserved for future semantic text embedding:

- `inline`: token or span-aligned marks shown in editor overlays.
- `embedded`: persisted projection for explicit script markup rendering/export.

Both collections are optional in v2 and may remain empty until embedding is implemented.

## Compatibility and migration

- v1 documents (without `schema`/`provenance`) are valid input.
- On load, missing v2 metadata is normalized with default values.
- Existing renderer logic (`basic`, `dialogueAct`, `themeRheme`) remains unchanged.
