<script>
  import { onDestroy, onMount } from "svelte";
  import { EditorState, StateEffect, StateField, Prec } from "@codemirror/state";
  import { EditorView, keymap, Decoration, WidgetType, dropCursor } from "@codemirror/view";
  import { indentUnit } from "@codemirror/language";
  import { lintGutter, setDiagnostics } from "@codemirror/lint";
  import { indentWithTab } from "@codemirror/commands";
  import { basicSetup } from "codemirror";
  import { sceneScript } from "./sceneScriptLanguage";
  import { sceneScriptHighlighting, sceneScriptTheme } from "./sceneScriptTheme";

  export let value = "";
  export let readOnly = false;
  export let onChange = null;
  export let hasServerError = false;
  export let diagnostics = [];
  export let sceneHighlights = [];
  export let semanticHighlights = { marks: [], lines: [] };
  export let playableTurns = []; // [{speaker, text, firstLineEndOffset, instanceName, loaded}] — M10
  export let onPlayTurn = null;  // callback(turn) — send this turn to its character's preview
  export let onCommandMenu = null; // (charOffset, clientX, clientY) => boolean — double-click on an existing
                                    // action span jumps straight into editing it (no right-click anywhere else
                                    // in the app); true = handled. Insertion moved to Ctrl+I (onInsertShortcut).
  export let onGhostClick = null;      // (charOffset, slot) => void — a suggested-position marker was clicked
  export let onSuggestShortcut = null; // (charOffset) => void — Ctrl+Shift+I suggests a position in this turn
  export let onInsertShortcut = null; // (charOffset) => void — Ctrl+I opens the extended insert dialog at the
                                       // cursor's position (replaces the old double-click-on-plain-text popup)
  export let actionSpans = [];      // [{offsetStart, offsetEnd, actionActor, actionName, features, raw}] — M13b
  export let markdownSpans = [];    // [{offsetStart, offsetEnd, kind: "section"|"note", level, body, raw}]
  export let compactCommands = false; // M13c: global compact/full view toggle — also drives markdownSpans rendering

  // Scene highlight decoration machinery
  const setSceneHighlightsEffect = StateEffect.define();
  const setSemanticHighlightsEffect = StateEffect.define();

  const scenePlayedLine = Decoration.line({ attributes: { class: "cm-scene-line cm-scene-line-played" } });
  const sceneActiveLine = Decoration.line({ attributes: { class: "cm-scene-line cm-scene-line-active" } });
  const sceneActiveTurnLine = Decoration.line({ attributes: { class: "cm-scene-line cm-scene-line-active-turn" } });

  const sceneHighlightField = StateField.define({
    create() { return Decoration.none; },
    update(decos, tr) {
      for (const effect of tr.effects) {
        if (effect.is(setSceneHighlightsEffect)) return effect.value;
      }
      return tr.docChanged ? decos.map(tr.changes) : decos;
    },
    provide: (f) => EditorView.decorations.from(f)
  });

  const semanticSubjectMark = Decoration.mark({
    class: "cm-semantic-subject",
    attributes: {
      style: "background: rgba(28,110,164,0.18); border-bottom: 3px solid rgba(28,110,164,0.95); border-radius: 2px;"
    }
  });
  const semanticVerbMark = Decoration.mark({
    class: "cm-semantic-verb",
    attributes: {
      style: "background: rgba(183,86,28,0.18); border-bottom: 3px solid rgba(183,86,28,0.95); border-radius: 2px;"
    }
  });
  const semanticObjectMark = Decoration.mark({
    class: "cm-semantic-object",
    attributes: {
      style: "background: rgba(70,132,54,0.18); border-bottom: 3px solid rgba(70,132,54,0.95); border-radius: 2px;"
    }
  });
  const semanticPredicateMark = Decoration.mark({
    class: "cm-semantic-predicate",
    attributes: {
      style: "background: rgba(88,96,120,0.18); border-bottom: 3px solid rgba(88,96,120,0.95); border-radius: 2px;"
    }
  });
  const semanticAddressMark = Decoration.mark({
    class: "cm-semantic-address",
    attributes: {
      style: "background: rgba(132,98,42,0.18); border-bottom: 3px solid rgba(132,98,42,0.95); border-radius: 2px;"
    }
  });
  const semanticAddressHeadMark = Decoration.mark({
    class: "cm-semantic-address-head",
    attributes: {
      style: "background: rgba(132,98,42,0.16); border-bottom: 3px solid rgba(132,98,42,0.95); border-radius: 2px;"
    }
  });
  // Clause linker ("wie", "dass", "weil"): teal, deliberately outside every role colour, because it
  // belongs to no role — it marks the seam between two clauses.
  const semanticLinkerMark = Decoration.mark({
    class: "cm-semantic-linker",
    attributes: {
      style: "background: rgba(32,116,116,0.12); border-bottom: 2px solid rgba(32,116,116,0.85); border-radius: 2px;"
    }
  });
  const semanticVerbAdjMark = Decoration.mark({
    class: "cm-semantic-verb-adjective",
    attributes: {
      style: "background: rgba(183,86,28,0.10); border-bottom: 2px dashed rgba(183,86,28,0.95); border-radius: 2px;"
    }
  });
  const semanticVerbAdvMark = Decoration.mark({
    class: "cm-semantic-verb-adverb",
    attributes: {
      style: "background: rgba(183,86,28,0.08); border-bottom: 2px dotted rgba(183,86,28,0.95); border-radius: 2px;"
    }
  });
  const semanticVerbCompMark = Decoration.mark({
    class: "cm-semantic-verb-comparison",
    attributes: {
      style: "background: rgba(183,86,28,0.08); border-bottom: 3px double rgba(183,86,28,0.95); border-radius: 2px;"
    }
  });
  const semanticSubjectAdjMark = Decoration.mark({
    class: "cm-semantic-subject-adjective",
    attributes: {
      style: "background: rgba(28,110,164,0.10); border-bottom: 2px dashed rgba(28,110,164,0.95); border-radius: 2px;"
    }
  });
  const semanticSubjectAdvMark = Decoration.mark({
    class: "cm-semantic-subject-adverb",
    attributes: {
      style: "background: rgba(28,110,164,0.08); border-bottom: 2px dotted rgba(28,110,164,0.95); border-radius: 2px;"
    }
  });
  const semanticSubjectCompMark = Decoration.mark({
    class: "cm-semantic-subject-comparison",
    attributes: {
      style: "background: rgba(28,110,164,0.08); border-bottom: 3px double rgba(28,110,164,0.95); border-radius: 2px;"
    }
  });
  const semanticObjectAdjMark = Decoration.mark({
    class: "cm-semantic-object-adjective",
    attributes: {
      style: "background: rgba(70,132,54,0.10); border-bottom: 2px dashed rgba(70,132,54,0.95); border-radius: 2px;"
    }
  });
  const semanticObjectAdvMark = Decoration.mark({
    class: "cm-semantic-object-adverb",
    attributes: {
      style: "background: rgba(70,132,54,0.08); border-bottom: 2px dotted rgba(70,132,54,0.95); border-radius: 2px;"
    }
  });
  const semanticObjectCompMark = Decoration.mark({
    class: "cm-semantic-object-comparison",
    attributes: {
      style: "background: rgba(70,132,54,0.08); border-bottom: 3px double rgba(70,132,54,0.95); border-radius: 2px;"
    }
  });
  const semanticPredicateAdjMark = Decoration.mark({
    class: "cm-semantic-predicate-adjective",
    attributes: {
      style: "background: rgba(88,96,120,0.10); border-bottom: 2px dashed rgba(88,96,120,0.95); border-radius: 2px;"
    }
  });
  const semanticPredicateAdvMark = Decoration.mark({
    class: "cm-semantic-predicate-adverb",
    attributes: {
      style: "background: rgba(88,96,120,0.08); border-bottom: 2px dotted rgba(88,96,120,0.95); border-radius: 2px;"
    }
  });
  const semanticPredicateCompMark = Decoration.mark({
    class: "cm-semantic-predicate-comparison",
    attributes: {
      style: "background: rgba(88,96,120,0.08); border-bottom: 3px double rgba(88,96,120,0.95); border-radius: 2px;"
    }
  });
  const semanticAddressAdjMark = Decoration.mark({
    class: "cm-semantic-address-adjective",
    attributes: {
      style: "background: rgba(132,98,42,0.10); border-bottom: 2px dashed rgba(132,98,42,0.95); border-radius: 2px;"
    }
  });
  const semanticAddressAdvMark = Decoration.mark({
    class: "cm-semantic-address-adverb",
    attributes: {
      style: "background: rgba(132,98,42,0.08); border-bottom: 2px dotted rgba(132,98,42,0.95); border-radius: 2px;"
    }
  });
  const semanticAddressCompMark = Decoration.mark({
    class: "cm-semantic-address-comparison",
    attributes: {
      style: "background: rgba(132,98,42,0.08); border-bottom: 3px double rgba(132,98,42,0.95); border-radius: 2px;"
    }
  });

  // --- Schema v3 marks (clauses, object kinds, phrase spans) -----------------------------------
  // The v2 marks above colour a single head token per role. v3 adds three distinctions the flat
  // block cannot express, and they are drawn so as not to fight the existing role colours:
  //   * clause  — a bracket, drawn only at the edges, so nesting stays readable
  //   * phrase  — a faint wash behind the whole constituent ("den roten Ball"), with the role's own
  //               head mark still on top of its head token
  //   * objects — direct / indirect / prepositional get distinct hues, because which object a
  //               command attaches to is exactly what placement learning is about
  const semanticClauseMark = Decoration.mark({
    class: "cm-semantic-clause",
    attributes: {
      style: "border-left: 2px solid rgba(120,120,140,0.45); border-right: 2px solid rgba(120,120,140,0.45); "
           + "border-radius: 3px; padding: 0 1px;"
    }
  });
  const semanticPhraseMark = Decoration.mark({
    class: "cm-semantic-phrase",
    attributes: { style: "background: rgba(70,132,54,0.08); border-radius: 2px;" }
  });
  const semanticObjectDirectMark = Decoration.mark({
    class: "cm-semantic-object-direct",
    attributes: {
      style: "background: rgba(70,132,54,0.14); border-bottom: 2px dashed rgba(70,132,54,0.85); border-radius: 2px;"
    }
  });
  const semanticObjectIndirectMark = Decoration.mark({
    class: "cm-semantic-object-indirect",
    attributes: {
      style: "background: rgba(150,110,30,0.14); border-bottom: 2px dashed rgba(150,110,30,0.9); border-radius: 2px;"
    }
  });
  const semanticObjectPrepMark = Decoration.mark({
    class: "cm-semantic-object-prepositional",
    attributes: {
      style: "background: rgba(120,80,150,0.14); border-bottom: 2px dotted rgba(120,80,150,0.9); border-radius: 2px;"
    }
  });
  // Anchor slots: where a behavior command could go. Zero-width, so drawn as a thin marker between
  // characters — a preview of what Phase 4's ghost suggestions will occupy.
  const semanticAnchorMark = Decoration.mark({
    class: "cm-semantic-anchor",
    attributes: { style: "border-left: 2px dotted rgba(28,110,164,0.55); margin-left: -1px;" }
  });

  const semanticHighlightField = StateField.define({
    create() { return Decoration.none; },
    update(decos, tr) {
      for (const effect of tr.effects) {
        if (effect.is(setSemanticHighlightsEffect)) return effect.value;
      }
      return tr.docChanged ? decos.map(tr.changes) : decos;
    },
    provide: (f) => EditorView.decorations.from(f)
  });

  // Per-turn Play button decoration machinery (M10) — mirrors the scene/semantic highlight
  // fields above: a StateEffect carries a fresh Decoration.set, a StateField holds/remaps it.
  const setPlayButtonsEffect = StateEffect.define();

  /**
   * Ghost marker for a suggested placement position (plan 4.1).
   *
   * <p>A widget, not a mark: an anchor slot is a *position*, so from === to, and CodeMirror mark
   * decorations require a non-empty range — the first version filtered every one of them out and
   * rendered nothing at all. A widget is the only decoration that can sit at a caret position.
   */
  class GhostWidget extends WidgetType {
    constructor(slot, label, offset, onClick) {
      super();
      this.slot = slot;
      this.label = label;
      this.offset = offset;
      this.onClick = onClick;
    }
    eq(other) {
      return other.slot === this.slot && other.offset === this.offset && other.label === this.label;
    }
    toDOM() {
      const btn = document.createElement("button");
      btn.type = "button";
      btn.className = "cm-ghost-marker";
      btn.title = this.label;
      btn.setAttribute("aria-label", this.label);
      btn.textContent = "\u2325"; // caret-ish glyph; the marker is the dashed rule in CSS
      btn.addEventListener("mousedown", (e) => { e.preventDefault(); e.stopPropagation(); });
      btn.addEventListener("click", (e) => {
        e.preventDefault();
        e.stopPropagation();
        this.onClick?.(this.offset, this.slot);
      });
      return btn;
    }
    ignoreEvent() { return true; }
  }

  class PlayButtonWidget extends WidgetType {
    constructor(turn, enabled, onPlay) {
      super();
      this.turn = turn;
      this.enabled = enabled;
      this.onPlay = onPlay;
    }
    eq(other) {
      return other.turn.firstLineEndOffset === this.turn.firstLineEndOffset
        && other.turn.speaker === this.turn.speaker
        && other.enabled === this.enabled;
    }
    toDOM() {
      const btn = document.createElement("button");
      btn.type = "button";
      btn.className = "cm-turn-play-btn" + (this.enabled ? "" : " cm-turn-play-btn-disabled");
      btn.disabled = !this.enabled;
      btn.title = this.enabled
        ? `Play this turn on ${this.turn.speaker}`
        : `${this.turn.speaker} preview isn't loaded yet — show it from the script toolbar first`;
      btn.setAttribute("aria-label", btn.title);
      btn.innerHTML = '<svg viewBox="0 0 24 24" width="10" height="10" fill="currentColor" aria-hidden="true"><path d="M8 6l10 6-10 6V6z"/></svg>';
      btn.addEventListener("mousedown", (e) => { e.preventDefault(); e.stopPropagation(); });
      btn.addEventListener("click", (e) => {
        e.preventDefault();
        e.stopPropagation();
        if (this.enabled) this.onPlay?.(this.turn);
      });
      return btn;
    }
    ignoreEvent() { return true; }
  }

  const playButtonsField = StateField.define({
    create() { return Decoration.none; },
    update(decos, tr) {
      for (const effect of tr.effects) {
        if (effect.is(setPlayButtonsEffect)) return effect.value;
      }
      return tr.docChanged ? decos.map(tr.changes) : decos;
    },
    provide: (f) => EditorView.decorations.from(f)
  });

  // Compact/full command display (M13c) — a pure view decoration: full mode shows nothing
  // (the raw stored text as-is), compact mode replaces each action span with a shortened
  // read-only label. Never edits the document itself.
  const setActionDisplayEffect = StateEffect.define();

  // M13g: click-to-mark a compact command chip, then Backspace/Delete removes it — identified by
  // offsets rather than object identity, since parseActionSpans rebuilds fresh span objects on
  // every reactive recompute (even for spans whose text didn't change).
  let selectedActionKey = null; // {offsetStart, offsetEnd} | null

  const SVG_NS = "http://www.w3.org/2000/svg";
  // Same glyphs as the SceneFlow node command badges (SceneFlowView.svelte) — blocking actions
  // pause the turn until they finish, non-blocking ones fire-and-forget (rocket).
  const BLOCKING_ICON_PATHS = ["M12 6v6l4 2", "M20 12v5", "M20 21h.01", "M21.25 8.2A10 10 0 1 0 16 21.16"];
  const NONBLOCKING_ICON_PATHS = [
    "M12 15v5s3.03-.55 4-2c1.08-1.62 0-5 0-5",
    "M4.5 16.5c-1.5 1.26-2 5-2 5s3.74-.5 5-2c.71-.84.7-2.13-.09-2.91a2.18 2.18 0 0 0-2.91-.09",
    "M9 12a22 22 0 0 1 2-3.95A12.88 12.88 0 0 1 22 2c0 2.72-.78 7.5-6 11a22.4 22.4 0 0 1-4 2z",
    "M9 12H4s.55-3.03 2-4c1.62-1.08 5 .05 5 .05"
  ];

  function isBlockingSpan(span) {
    // "pause" always halts the utterance by definition — show the clock regardless of any
    // `blocking` feature, since that's the intuitive read for authors even if the runtime
    // classifies it differently (core's ActionBlockingUtil).
    if (span.actionName === "pause") return true;
    const feature = span.features?.find((f) => f.key === "blocking");
    return !!feature && String(feature.value) === "true";
  }

  // M13f: drag-reorder inline commands within a turn — only meaningful in compact view, since
  // full view already lets authors cut/paste the raw [...] text directly.
  const ACTION_DRAG_MIME = "application/x-vsm-action-span";

  function isSpaceChar(ch) {
    return ch === " " || ch === "\t";
  }

  // Adds a leading/trailing space around already-bracketed `text` if it would otherwise land
  // directly against a non-whitespace character at `pos` — shared by the drag-drop move below
  // and by paste (see domEventHandlers.paste), both of which insert a raw "[...]" span at an
  // arbitrary point mid-turn where word-gluing would otherwise result (e.g. "mich[emotion]einen").
  function padForInsertAt(doc, text, pos) {
    const before = pos > 0 ? doc.sliceString(pos - 1, pos) : "";
    const after = pos < doc.length ? doc.sliceString(pos, pos + 1) : "";
    const needsLeading = before !== "" && !/\s/.test(before);
    const needsTrailing = after !== "" && !/\s/.test(after);
    return `${needsLeading ? " " : ""}${text}${needsTrailing ? " " : ""}`;
  }

  // Builds the two-change transaction that moves an action span from its old offsets to
  // dropPos, keeping surrounding text sane: collapses the double space left behind at the old
  // location, and adds a space on either side of the drop point if it would otherwise butt up
  // against a word. All positions are resolved against `doc` as it stood before either change —
  // CodeMirror composes a multi-entry `changes` array against that same original document
  // regardless of array order, so no manual offset-shifting is needed between the two edits.
  function buildActionMoveChanges(doc, offsetStart, offsetEnd, raw, dropPos) {
    const docLength = doc.length;
    if (dropPos == null || dropPos < 0 || dropPos > docLength) return null;

    const before = offsetStart > 0 ? doc.sliceString(offsetStart - 1, offsetStart) : "";
    const after = offsetEnd < docLength ? doc.sliceString(offsetEnd, offsetEnd + 1) : "";
    const eatsTrailingSpace = isSpaceChar(before) && isSpaceChar(after);
    const srcFrom = offsetStart;
    const srcTo = eatsTrailingSpace ? offsetEnd + 1 : offsetEnd;

    if (dropPos >= srcFrom && dropPos <= srcTo) return null; // dropped onto/next to itself

    const insertText = padForInsertAt(doc, raw, dropPos);

    return {
      changes: [
        { from: srcFrom, to: srcTo, insert: "" },
        { from: dropPos, insert: insertText }
      ]
    };
  }

  // Ctrl/Cmd+C on a selected (clicked) command chip copies its raw "[...]" text to the system
  // clipboard. ActionCompactWidget's click handler sets a real CodeMirror selection over the
  // span and focuses the view so the browser's native copy command actually fires; the copy
  // handler below then overrides the clipboard payload with the clean span.raw text regardless
  // of what that underlying selection contains. Ctrl/Cmd+V detects a single bracketed action on
  // the clipboard and inserts it with the same word-boundary padding as drag-drop; anything else
  // (including a normal multi-word text copy) falls through to CodeMirror's own default paste.
  const ACTION_LIKE_RE = /^\[[\s\S]*\]$/;

  // Backspace/Delete removes the currently marked (clicked) command chip instead of the usual
  // character-before/after-cursor behavior. Registered at Prec.highest so it runs before
  // basicSetup's default Backspace/Delete bindings, which would otherwise claim the key first.
  function deleteSelectedActionSpan(cmView) {
    if (!selectedActionKey) return false;
    const key = selectedActionKey;
    selectedActionKey = null;
    const span = (actionSpans || []).find(
      (s) => s.offsetStart === key.offsetStart && s.offsetEnd === key.offsetEnd
    );
    if (!span) return false;
    const doc = cmView.state.doc;
    let { offsetStart: from, offsetEnd: to } = span;
    const before = from > 0 ? doc.sliceString(from - 1, from) : "";
    const after = to < doc.length ? doc.sliceString(to, to + 1) : "";
    if (isSpaceChar(before) && isSpaceChar(after)) to += 1; // collapse the double space left behind
    cmView.dispatch({ changes: { from, to, insert: "" } });
    return true;
  }

  function buildActionCompactIcon(blocking) {
    const svg = document.createElementNS(SVG_NS, "svg");
    svg.setAttribute("class", "cm-action-compact-icon");
    svg.setAttribute("viewBox", "0 0 24 24");
    svg.setAttribute("aria-hidden", "true");
    for (const d of (blocking ? BLOCKING_ICON_PATHS : NONBLOCKING_ICON_PATHS)) {
      const path = document.createElementNS(SVG_NS, "path");
      path.setAttribute("d", d);
      svg.appendChild(path);
    }
    return svg;
  }

  // A hex code or CSS color name reads less clearly than the actual color at a glance — swap it
  // for a small patch instead. `background-color` accepts any valid CSS color string (hex or
  // named) and silently no-ops on an invalid one, so no validation needed here.
  function buildBackgroundSwatch(colorValue) {
    const swatch = document.createElement("span");
    swatch.className = "cm-action-compact-swatch";
    swatch.style.backgroundColor = colorValue || "transparent";
    return swatch;
  }

  class ActionCompactWidget extends WidgetType {
    constructor(span, selected) {
      super();
      this.span = span;
      this.selected = selected;
    }
    eq(other) {
      return other.span.offsetStart === this.span.offsetStart
        && other.span.offsetEnd === this.span.offsetEnd
        && other.span.raw === this.span.raw
        && other.selected === this.selected;
    }
    toDOM() {
      const el = document.createElement("span");
      el.className = this.selected ? "cm-action-compact cm-action-compact-selected" : "cm-action-compact";
      el.draggable = true;
      el.appendChild(buildActionCompactIcon(isBlockingSpan(this.span)));
      if (this.span.actionName === "background") {
        // A single wrapper (rather than appending text/swatch/text as separate flex children of
        // `el`) keeps `.cm-action-compact`'s own gap from inserting extra space around the
        // swatch — the swatch's own margin handles that spacing instead.
        const prefix = this.span.actionActor ? `${this.span.actionActor}: ` : "";
        const colorFeature = this.span.features?.find((f) => f.key === "color");
        const label = document.createElement("span");
        label.appendChild(document.createTextNode(`[${prefix}background`));
        label.appendChild(buildBackgroundSwatch(colorFeature?.value));
        label.appendChild(document.createTextNode(`]`));
        el.appendChild(label);
      } else {
        el.appendChild(document.createTextNode(compactLabelForSpan(this.span)));
      }
      el.title = this.span.raw;
      el.addEventListener("click", (event) => {
        event.preventDefault();
        event.stopPropagation();
        selectedActionKey = { offsetStart: this.span.offsetStart, offsetEnd: this.span.offsetEnd };
        // Also set a real CodeMirror selection over the span's (visually replaced) range, and
        // explicitly focus the editor — selectedActionKey alone is a custom highlight the browser
        // knows nothing about, so without both a real selection AND focus, the native Ctrl/Cmd+C
        // copy command never fires at all (browsers only dispatch "copy" targeting whatever
        // element is currently focused, and only when something is actually selected; this
        // widget's own preventDefault()/stopPropagation() above suppress the click's default
        // focus-shift). The domEventHandlers "copy" handler below still overrides what gets
        // copied with the clean span.raw text regardless of what the real selection contains.
        view?.dispatch({ selection: { anchor: this.span.offsetStart, head: this.span.offsetEnd } });
        view?.focus();
      });
      el.addEventListener("dragstart", (event) => {
        event.dataTransfer.effectAllowed = "move";
        event.dataTransfer.setData(ACTION_DRAG_MIME, JSON.stringify({
          offsetStart: this.span.offsetStart,
          offsetEnd: this.span.offsetEnd,
          raw: this.span.raw
        }));
        el.classList.add("cm-action-compact-dragging");
      });
      el.addEventListener("dragend", () => {
        el.classList.remove("cm-action-compact-dragging");
      });
      // ignoreEvent() below stops CodeMirror's own dblclick handling (and the domEventHandlers
      // dblclick, which is gated the same way) from ever seeing events that land on this widget,
      // so open the menu directly here instead — we already know the exact span, no hit-testing
      // needed.
      el.addEventListener("dblclick", (event) => {
        if (!onCommandMenu) return;
        event.preventDefault();
        event.stopPropagation();
        onCommandMenu(this.span.offsetStart, event.clientX, event.clientY);
      });
      return el;
    }
    ignoreEvent() { return true; }
  }

  function compactLabelForSpan(span) {
    const prefix = span.actionActor ? `${span.actionActor}: ` : "";
    const primary = span.features.length ? ` ${span.features[0].value}` : "";
    return `[${prefix}${span.actionName}${primary}]`;
  }

  const actionDisplayField = StateField.define({
    create() { return Decoration.none; },
    update(decos, tr) {
      for (const effect of tr.effects) {
        if (effect.is(setActionDisplayEffect)) return effect.value;
      }
      return tr.docChanged ? decos.map(tr.changes) : decos;
    },
    provide: (f) => EditorView.decorations.from(f)
  });

  // Compact rendering for the new top-level "# Section" / "Note: ..." constructs. Plain mode
  // shows the raw markdown text as typed; compact mode applies styling via Decoration.mark
  // rather than replacing the text with an opaque widget — marks style real, still-editable
  // text in place, so clicking into a heading/note to extend it behaves like any other text
  // (an earlier Decoration.replace-based widget version broke click-to-position-cursor here).
  const setMarkdownDisplayEffect = StateEffect.define();
  const INLINE_MARKDOWN_RE = /\*\*([^*]+)\*\*|\*([^*]+)\*|_([^_]+)_/g;

  const headingMarks = [1, 2, 3].map((level) =>
    Decoration.mark({ class: `cm-heading-compact cm-heading-compact-${level}` })
  );
  const noteMark = Decoration.mark({ class: "cm-note-compact" });
  const mdBoldMark = Decoration.mark({ class: "cm-md-bold" });
  const mdItalicMark = Decoration.mark({ class: "cm-md-italic" });

  // Adds nested **bold**/*italic*/_italic_ styling within a line's absolute [from, to) range,
  // keeping the delimiter characters visible (no hidden markup, so no atomic-widget cursor risk).
  function pushInlineMarkdownRanges(ranges, raw, from) {
    INLINE_MARKDOWN_RE.lastIndex = 0;
    let match;
    while ((match = INLINE_MARKDOWN_RE.exec(raw))) {
      const mark = match[1] !== undefined ? mdBoldMark : mdItalicMark;
      ranges.push(mark.range(from + match.index, from + match.index + match[0].length));
    }
  }

  const markdownDisplayField = StateField.define({
    create() { return Decoration.none; },
    update(decos, tr) {
      for (const effect of tr.effects) {
        if (effect.is(setMarkdownDisplayEffect)) return effect.value;
      }
      return tr.docChanged ? decos.map(tr.changes) : decos;
    },
    provide: (f) => EditorView.decorations.from(f)
  });

  let host;
  let view;
  let suppress = false;
  let currentReadOnly = readOnly;
  let currentSearchQuery = "";

  function collectDiagnostics(text) {
    const diagnostics = [];
    const stack = [];
    let inComment = false;
    let commentStart = 0;

    for (let i = 0; i < text.length; i += 1) {
      if (!inComment && text[i] === "/" && text[i + 1] === "*") {
        inComment = true;
        commentStart = i;
        i += 1;
        continue;
      }
      if (inComment) {
        if (text[i] === "*" && text[i + 1] === "/") {
          inComment = false;
          i += 1;
        }
        continue;
      }
      if (text[i] === "[") {
        stack.push(i);
        continue;
      }
      if (text[i] === "]") {
        if (stack.length) {
          stack.pop();
        } else {
          diagnostics.push({
            from: i,
            to: i + 1,
            severity: "error",
            message: "Unmatched closing bracket."
          });
        }
        continue;
      }
      if (text[i] === "$") {
        const next = text[i + 1];
        if (!next || !/[\p{L}@_]/u.test(next)) {
          diagnostics.push({
            from: i,
            to: i + 1,
            severity: "warning",
            message: "Placeholder must be followed by an identifier."
          });
        }
      }
    }

    if (inComment) {
      diagnostics.push({
        from: commentStart,
        to: Math.min(commentStart + 2, text.length),
        severity: "error",
        message: "Unclosed block comment."
      });
    }

    while (stack.length) {
      const pos = stack.pop();
      diagnostics.push({
        from: pos,
        to: pos + 1,
        severity: "error",
        message: "Unclosed opening bracket."
      });
    }

    return diagnostics;
  }

  function normalizeDiagnostics(list, docLength) {
    if (!Array.isArray(list)) {
      return [];
    }
    return list
      .map((diag) => {
        const fromRaw = Number.isFinite(diag.from) ? diag.from : 0;
        const toRaw = Number.isFinite(diag.to) ? diag.to : fromRaw + 1;
        const from = Math.max(0, Math.min(docLength, fromRaw));
        let to = Math.max(from, Math.min(docLength, toRaw));
        if (to === from && docLength > from) {
          to = from + 1;
        }
        return {
          from,
          to,
          severity: diag.severity || "error",
          message: diag.message || "Script error"
        };
      })
      .filter((diag) => diag.from < diag.to);
  }

  function refreshDiagnostics(text) {
    if (!view) return;
    const docText = text ?? view.state.doc.toString();
    const merged = [
      ...normalizeDiagnostics(diagnostics, docText.length),
      ...collectDiagnostics(docText)
    ];
    view.dispatch(setDiagnostics(view.state, merged));
  }

  function buildState(docText) {
    const updateListener = EditorView.updateListener.of((update) => {
      if (!update.docChanged || suppress) return;
      const text = update.state.doc.toString();
      if (typeof onChange === "function") {
        onChange(text);
      }
      refreshDiagnostics(text);
    });
    const extensions = [
      basicSetup,
      sceneScript(),
      sceneScriptTheme,
      sceneScriptHighlighting,
      lintGutter(),
      Prec.highest(keymap.of([
        { key: "Backspace", run: deleteSelectedActionSpan },
        { key: "Delete", run: deleteSelectedActionSpan },
        // Mod-i (Ctrl+I / Cmd+I) opens the extended insert-action dialog at the cursor. Must win
        // over basicSetup's defaultKeymap, which already binds Mod-i to selectParentSyntax —
        // Prec.highest is what makes this run first (same reasoning as Backspace/Delete above).
        {
          key: "Mod-i",
          run: (cmView) => {
            if (!onInsertShortcut) return false;
            onInsertShortcut(cmView.state.selection.main.from);
            return true;
          },
          preventDefault: true
        },
        // Mod-Shift-i asks where a behavior belongs in the turn under the cursor, instead of
        // inserting at the cursor itself. Bound next to Mod-i deliberately: it is the same act with
        // the position chosen by the project's model rather than by where you happen to be.
        {
          key: "Mod-Shift-i",
          run: (cmView) => {
            if (!onSuggestShortcut) return false;
            onSuggestShortcut(cmView.state.selection.main.from);
            return true;
          },
          preventDefault: true
        }
      ])),
      keymap.of([indentWithTab]),
      indentUnit.of("  "),
      EditorState.tabSize.of(2),
      EditorView.lineWrapping,
      updateListener,
      sceneHighlightField,
      semanticHighlightField,
      playButtonsField,
      actionDisplayField,
      markdownDisplayField,
      dropCursor(),
      EditorView.domEventHandlers({
        // Only fires for clicks outside any widget — ActionCompactWidget.ignoreEvent() blocks
        // this handler entirely for clicks that land on a chip, which set selectedActionKey
        // themselves instead (see the widget's own "click" listener).
        click() {
          if (selectedActionKey) selectedActionKey = null;
          return false;
        },
        dblclick(event, cmView) {
          if (!onCommandMenu) return false;
          const pos = cmView.posAtCoords({ x: event.clientX, y: event.clientY });
          if (pos == null) return false;
          if (onCommandMenu(pos, event.clientX, event.clientY)) {
            event.preventDefault(); // suppress the browser's default double-click word selection
            return true;
          }
          return false;
        },
        dragover(event) {
          if (readOnly || !event.dataTransfer?.types?.includes(ACTION_DRAG_MIME)) return false;
          event.preventDefault();
          return true;
        },
        drop(event, cmView) {
          if (readOnly) return false;
          const payloadRaw = event.dataTransfer?.getData(ACTION_DRAG_MIME);
          if (!payloadRaw) return false;
          event.preventDefault();
          let payload;
          try {
            payload = JSON.parse(payloadRaw);
          } catch {
            return true;
          }
          const { offsetStart, offsetEnd, raw } = payload;
          const dropPos = cmView.posAtCoords({ x: event.clientX, y: event.clientY });
          const move = buildActionMoveChanges(cmView.state.doc, offsetStart, offsetEnd, raw, dropPos);
          if (!move) return true;
          try {
            cmView.dispatch(move);
          } catch {
            // Stale offsets (document changed since dragstart) — ignore the drop.
          }
          return true;
        },
        copy(event) {
          if (!selectedActionKey) return false; // no chip selected — let default copy proceed
          const span = (actionSpans || []).find(
            (s) => s.offsetStart === selectedActionKey.offsetStart && s.offsetEnd === selectedActionKey.offsetEnd
          );
          if (!span) return false;
          event.clipboardData?.setData("text/plain", span.raw);
          event.preventDefault();
          return true;
        },
        paste(event, cmView) {
          if (readOnly) return false;
          const text = event.clipboardData?.getData("text/plain")?.trim();
          if (!text || !ACTION_LIKE_RE.test(text)) return false; // not a single bracketed action — default paste
          event.preventDefault();
          const { from, to } = cmView.state.selection.main;
          const insertText = padForInsertAt(cmView.state.doc, text, from);
          cmView.dispatch({
            changes: { from, to, insert: insertText },
            selection: { anchor: from + insertText.length }
          });
          return true;
        }
      }),
      EditorState.readOnly.of(readOnly),
      EditorView.editable.of(!readOnly)
    ];
    return EditorState.create({
      doc: docText || "",
      extensions
    });
  }

  function mountEditor() {
    if (!host) return;
    if (view) {
      view.destroy();
    }
    currentReadOnly = readOnly;
    view = new EditorView({
      state: buildState(value),
      parent: host
    });
    refreshDiagnostics(value);
  }

  onMount(() => {
    mountEditor();
  });

  onDestroy(() => {
    if (view) {
      view.destroy();
    }
  });

  function findMatch(query, direction) {
    if (!view || !query) return;
    const text = view.state.doc.toString();
    if (!text) return;
    const selection = view.state.selection.main;
    const start = direction > 0 ? selection.to : selection.from - 1;
    let index = direction > 0 ? text.indexOf(query, start) : text.lastIndexOf(query, start);
    if (index === -1) {
      index = direction > 0 ? text.indexOf(query) : text.lastIndexOf(query);
    }
    if (index === -1) return;
    view.dispatch({
      selection: { anchor: index, head: index + query.length },
      scrollIntoView: true
    });
    view.focus();
  }

  export function setSearchQuery(query) {
    currentSearchQuery = String(query || "");
  }

  export function findNext() {
    findMatch(currentSearchQuery, 1);
  }

  export function findPrevious() {
    findMatch(currentSearchQuery, -1);
  }

  // M13h: the SIA panel's "insert at cursor" buttons need the current cursor position to
  // resolve which turn/speaker it's in (for the actor-prefix decision) before building the
  // bracket text — insertText() itself already targets the current selection when called with
  // no explicit pos, so no separate "insert at this offset" path is needed here.
  export function getCursorOffset() {
    return view ? view.state.selection.main.from : null;
  }

  // pos (optional, M11): insert at this exact character offset instead of the current
  // selection — needed for the double-click "insert emotion" flow, where the cursor may not
  // still be at the point the user double-clicked by the time they confirm the modal.
  export function insertText(text, pos) {
    if (!view || readOnly) return;
    const insert = text == null ? "" : String(text);
    if (!insert) return;
    const selection = view.state.selection.main;
    const from = Number.isFinite(pos) ? Math.max(0, Math.min(pos, view.state.doc.length)) : selection.from;
    const to = Number.isFinite(pos) ? from : selection.to;
    view.dispatch({
      changes: { from, to, insert },
      selection: { anchor: from + insert.length }
    });
    view.focus();
  }

  // M13d: replaces an exact [from, to) range with text — used for editing an existing action
  // span in place (preserving its surrounding spacing) and for deleting one (text = ""). A
  // separate method from insertText rather than overloading its empty-string semantics, since
  // insertText("") deliberately no-ops even against an active selection.
  export function replaceRange(text, from, to) {
    if (!view || readOnly) return;
    const insert = text == null ? "" : String(text);
    const doc = view.state.doc;
    const resolvedFrom = Math.max(0, Math.min(from, doc.length));
    const resolvedTo = Math.max(resolvedFrom, Math.min(to, doc.length));
    view.dispatch({
      changes: { from: resolvedFrom, to: resolvedTo, insert },
      selection: { anchor: resolvedFrom + insert.length }
    });
    view.focus();
  }

  export function focusEditor() {
    view?.focus();
  }

  $: if (view && value !== view.state.doc.toString()) {
    const currentText = view.state.doc.toString();
    suppress = true;
    try {
      view.dispatch({
        changes: { from: 0, to: currentText.length, insert: value || "" }
      });
    } finally {
      suppress = false;
    }
    refreshDiagnostics(value || "");
  }

  $: if (view && readOnly !== currentReadOnly) {
    mountEditor();
  }

  $: if (view && diagnostics) {
    refreshDiagnostics();
  }

  function applySceneHighlights(highlights) {
    if (!view) return;
    const doc = view.state.doc;
    const maxLine = doc.lines;
    const lineTypes = new Map();
    const weight = { played: 1, active: 2, activeTurn: 3 };

    function setLineType(lineNo, type) {
      if (lineNo < 1 || lineNo > maxLine) return;
      const current = lineTypes.get(lineNo);
      if (!current || weight[type] > weight[current]) {
        lineTypes.set(lineNo, type);
      }
    }

    function startBoundaryLine(offset) {
      if (!Number.isFinite(offset)) return -1;
      const clamped = Math.max(0, Math.min(doc.length, Math.floor(offset)));
      if (doc.length === 0) return 1;
      if (clamped >= doc.length) return doc.lines;
      return doc.lineAt(clamped).number;
    }

    function endBoundaryLine(offset) {
      if (!Number.isFinite(offset)) return -1;
      const clamped = Math.max(0, Math.min(doc.length, Math.floor(offset)));
      if (clamped <= 0) return 1;
      return doc.lineAt(clamped - 1).number;
    }

    function firstNonEmptyLine(fromLine, toLine) {
      const start = Math.max(1, fromLine);
      const end = Math.min(maxLine, Math.max(start, toLine));
      for (let lineNo = start; lineNo <= end; lineNo += 1) {
        if (doc.line(lineNo).text.trim().length > 0) {
          return lineNo;
        }
      }
      return start;
    }

    const ranges = [];
    for (const h of highlights) {
      // lower/upper are character offsets into the script text
      const fromLine = startBoundaryLine(h.lower);
      const toLine = endBoundaryLine(h.upper);
      if (fromLine < 1 || toLine < fromLine) continue;
      const type = h.type === "activeTurn" ? "activeTurn"
                 : h.type === "active" ? "active" : "played";
      const anchorLine = firstNonEmptyLine(fromLine, toLine);
      if (anchorLine > 0) {
        setLineType(anchorLine, type);
      }
    }

    for (const [lineNo, type] of lineTypes) {
      const lineFrom = doc.line(lineNo).from;
      const deco = type === "activeTurn" ? sceneActiveTurnLine
        : type === "active" ? sceneActiveLine : scenePlayedLine;
      ranges.push(deco.range(lineFrom));
    }

    ranges.sort((a, b) => a.from - b.from || a.to - b.to);
    view.dispatch({ effects: setSceneHighlightsEffect.of(Decoration.set(ranges)) });
  }

  function semanticLineDecoration(badge) {
    return Decoration.line({
      attributes: {
        class: "cm-semantic-line",
        "data-semantic-badge": String(badge || "")
      }
    });
  }

  function applySemanticHighlights(payload) {
    if (!view) return;
    const doc = view.state.doc;
    const docLen = doc.length;
    const ranges = [];
    const marks = Array.isArray(payload?.marks) ? payload.marks : [];
    const lines = Array.isArray(payload?.lines) ? payload.lines : [];

    for (const mark of marks) {
      const from = Number.isFinite(mark?.from) ? Math.max(0, Math.min(docLen, mark.from)) : -1;
      const to = Number.isFinite(mark?.to) ? Math.max(0, Math.min(docLen, mark.to)) : -1;
      const kind = String(mark?.kind || "").toLowerCase();
      // Ghost markers are positions, so they are widgets and are handled before the non-empty-range
      // check that every other mark has to pass.
      if (kind === "ghost") {
        if (from < 0) continue;
        ranges.push(Decoration.widget({
          widget: new GhostWidget(mark.slot || "", mark.label || "Suggested position", from, onGhostClick),
          side: -1
        }).range(from));
        continue;
      }
      if (from < 0 || to <= from) continue;
      let deco = semanticSubjectMark;
      if (kind === "verb") deco = semanticVerbMark;
      else if (kind === "object") deco = semanticObjectMark;
      else if (kind === "predicate") deco = semanticPredicateMark;
      else if (kind === "address") deco = semanticAddressMark;
      else if (kind === "address-head") deco = semanticAddressHeadMark;
      else if (kind === "linker") deco = semanticLinkerMark;
      else if (kind === "verb-adjective") deco = semanticVerbAdjMark;
      else if (kind === "verb-adverb") deco = semanticVerbAdvMark;
      else if (kind === "verb-comparison") deco = semanticVerbCompMark;
      else if (kind === "subject-adjective") deco = semanticSubjectAdjMark;
      else if (kind === "subject-adverb") deco = semanticSubjectAdvMark;
      else if (kind === "subject-comparison") deco = semanticSubjectCompMark;
      else if (kind === "object-adjective") deco = semanticObjectAdjMark;
      else if (kind === "object-adverb") deco = semanticObjectAdvMark;
      else if (kind === "object-comparison") deco = semanticObjectCompMark;
      else if (kind === "predicate-adjective") deco = semanticPredicateAdjMark;
      else if (kind === "predicate-adverb") deco = semanticPredicateAdvMark;
      else if (kind === "predicate-comparison") deco = semanticPredicateCompMark;
      else if (kind === "address-adjective") deco = semanticAddressAdjMark;
      else if (kind === "address-adverb") deco = semanticAddressAdvMark;
      else if (kind === "address-comparison") deco = semanticAddressCompMark;
      else if (kind === "clause") deco = semanticClauseMark;
      else if (kind === "phrase") deco = semanticPhraseMark;
      else if (kind === "object-direct") deco = semanticObjectDirectMark;
      else if (kind === "object-indirect") deco = semanticObjectIndirectMark;
      else if (kind === "object-prepositional") deco = semanticObjectPrepMark;
      else if (kind === "object-clausal" || kind === "object-oblique") deco = semanticObjectDirectMark;
      else if (kind === "anchor") deco = semanticAnchorMark;
      ranges.push(deco.range(from, to));
    }

    for (const meta of lines) {
      const lineNo = Number.isFinite(meta?.line) ? Math.floor(meta.line) : -1;
      const badge = String(meta?.badge || "").trim();
      if (lineNo < 1 || !badge) continue;
      const clampedLineNo = Math.min(lineNo, doc.lines);
      const lineFrom = doc.line(clampedLineNo).from;
      ranges.push(semanticLineDecoration(badge).range(lineFrom));
    }

    ranges.sort((a, b) => a.from - b.from || a.to - b.to);
    view.dispatch({ effects: setSemanticHighlightsEffect.of(Decoration.set(ranges, true)) });
  }

  $: if (view && sceneHighlights) {
    applySceneHighlights(sceneHighlights);
  }

  $: if (view && semanticHighlights) {
    applySemanticHighlights(semanticHighlights);
  }

  function applyPlayButtons(turns) {
    if (!view) return;
    const doc = view.state.doc;
    const ranges = [];
    for (const turn of turns || []) {
      const offset = Math.floor(turn?.firstLineEndOffset);
      if (!Number.isFinite(offset) || offset < 0 || offset > doc.length) continue;
      const widget = new PlayButtonWidget(turn, !!turn.loaded, onPlayTurn);
      ranges.push(Decoration.widget({ widget, side: 1 }).range(offset));
    }
    ranges.sort((a, b) => a.from - b.from || a.to - b.to);
    view.dispatch({ effects: setPlayButtonsEffect.of(Decoration.set(ranges)) });
  }

  $: if (view && playableTurns) {
    applyPlayButtons(playableTurns);
  }

  function applyActionDisplay(spans, compact, selectedKey) {
    if (!view) return;
    const doc = view.state.doc;
    const ranges = [];
    if (compact) {
      for (const span of spans || []) {
        const from = Math.floor(span.offsetStart);
        const to = Math.floor(span.offsetEnd);
        if (!Number.isFinite(from) || !Number.isFinite(to) || from < 0 || to > doc.length || to <= from) continue;
        const selected = !!selectedKey && selectedKey.offsetStart === span.offsetStart && selectedKey.offsetEnd === span.offsetEnd;
        const widget = new ActionCompactWidget(span, selected);
        ranges.push(Decoration.replace({ widget }).range(from, to));
      }
    }
    ranges.sort((a, b) => a.from - b.from || a.to - b.to);
    view.dispatch({ effects: setActionDisplayEffect.of(Decoration.set(ranges)) });
  }

  $: if (view) {
    applyActionDisplay(actionSpans, compactCommands, selectedActionKey);
  }

  function applyMarkdownDisplay(spans, compact) {
    if (!view) return;
    const doc = view.state.doc;
    const ranges = [];
    if (compact) {
      for (const span of spans || []) {
        const from = Math.floor(span.offsetStart);
        const to = Math.floor(span.offsetEnd);
        if (!Number.isFinite(from) || !Number.isFinite(to) || from < 0 || to > doc.length || to <= from) continue;
        const lineMark = span.kind === "section" ? headingMarks[Math.min(span.level, 3) - 1] : noteMark;
        ranges.push(lineMark.range(from, to));
        pushInlineMarkdownRanges(ranges, span.raw, from);
      }
    }
    ranges.sort((a, b) => a.from - b.from || a.to - b.to);
    view.dispatch({ effects: setMarkdownDisplayEffect.of(Decoration.set(ranges, true)) });
  }

  $: if (view) {
    applyMarkdownDisplay(markdownSpans, compactCommands);
  }
</script>

<div class="script-editor" class:has-error={hasServerError} bind:this={host}></div>
