# Scene Support (Scene Title Generator + Dangling PlayScene Suggestions)

This document describes the new scene support features in the web editor:

- Scene Title Generator (semantic title suggestions)
- Semantic replacement suggestions when scenes are removed but still referenced by PlayScene commands

It also summarizes the core algorithmic approach and integration points.

## 1) Scene Title Generator

**Goal:** Propose better scene titles based on the semantic content of scene turns.

**How it works (high level):**

1. Extract scene content (dialog turns) from the scene script.
2. Compute embeddings for each scene group.
3. Cluster scenes by semantic similarity so similar scenes share a title.
4. For each cluster, compare the cluster text against a project-defined list of title concepts.
5. Show the top 3 suggestions per scene; accept/dismiss in UI.

**Why concepts list?**

We found that keyword extraction alone can produce literal or low-quality titles. Instead, the editor uses a project-specific list of **scene title concepts** (English, 1–3 words, CamelBack ID style), and chooses the best semantic match.

### Where to configure

Project Settings → **Scene title concepts** (textarea)

- One concept per line
- English
- 1–3 words
- CamelBack ID style (e.g. `WarmGreeting`, `BadArgument`, `GoodBye`)

These are stored in project config and used as the **only** candidate list for the generator.

### UI behavior

- A **Scene Title Generator** button sits next to Generate Scenes.
- Suggestions appear under each scene name:
  1) top suggestion
  2) second suggestion
  3) third suggestion
- Click any suggestion to apply it immediately.
- Dismiss per scene with the `×` button.
- Accept/Dismiss all controls remain in the toolbar.

### CamelBack normalization

Concepts preserve their internal capitalization, so `BadArgument` stays as-is.

## 2) Dangling PlayScene References (Scene Removed)

**Goal:** When a scene disappears but PlayScene commands still reference it, offer semantic replacements.

**Trigger:**

- The editor detects scene list changes immediately when a scene disappears in the script (no save required).
- If there are dangling PlayScene references, a dialog opens.

**Semantic replacement approach:**

Instead of comparing only scene **names**, this compares **scene content**:

1. When a scene is removed, capture its *previous* content (scene turns) from the script.
2. For each remaining scene, compute embeddings for its content.
3. Compare removed-scene embedding against each candidate scene embedding.
4. Rank candidates by cosine similarity.
5. Display top 3 semantic suggestions per removed scene.

The dialog shows:

- Removed scene name
- Top 3 semantic suggestions (clickable)
- A full dropdown of all remaining scenes for manual override

This gives good recommendations even when names are misleading or out of date.

## 3) Algorithmic Summary

### Embedding generation

- The embeddings service is a small local Javalin app (`services/embeddings`).
- It provides `/embed` and `/similarity` endpoints.
- The editor calls it via HTTP and falls back gracefully when offline.

### Similarity

- We use cosine similarity on normalized embeddings.
- For title generation, we match **cluster text** vs **concept list**.
- For dangling scene replacement, we match **removed scene text** vs **remaining scene texts**.

### Clustering (Scene Title Generator)

- Scenes are grouped by similarity to avoid inconsistent naming of near-identical content.
- A scene cluster receives a single title suggestion (top-ranked concept).

## 4) Relevant Code Locations

**Editor (web UI)**

- Scene title generator logic: `editor/web-ui/src/App.svelte`
- Scene removal detection and dangling dialog: `editor/web-ui/src/App.svelte`
- Scene list UI suggestions: `editor/web-ui/src/App.svelte`
- Styling: `editor/web-ui/src/app.css`

**Core (server)**

- ProjectConfig serialization: `core/src/main/java/de/dfki/vsm/web/WebUiServer.java`
- Scene title concepts config: `core/src/main/java/de/dfki/vsm/model/project/ProjectConfig.java`

**Embeddings service**

- `services/embeddings/src/main/java/de/dfki/vsm/services/embeddings/`
- Download task: `:services:embeddings:downloadModel`

## 5) Operational Notes

- The embeddings service auto-starts from the editor if available.
- If the service is offline, the UI indicates that semantic suggestions are unavailable.
- Suggestions can be accepted at any time; PlayScene references are renamed accordingly.

## 6) Tips for Better Results

- Ensure scene text is representative (remove noisy boilerplate lines if possible).
- Keep concept list concise and intentionally abstract.
- Avoid overlapping concepts (e.g., both `Welcome` and `WarmGreeting` if they’re meant to be the same idea).

