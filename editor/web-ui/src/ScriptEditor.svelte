<script>
  import { onDestroy, onMount } from "svelte";
  import { EditorState, StateEffect, StateField } from "@codemirror/state";
  import { EditorView, keymap, Decoration } from "@codemirror/view";
  import { indentUnit } from "@codemirror/language";
  import { lintGutter, nextDiagnostic, previousDiagnostic, setDiagnostics } from "@codemirror/lint";
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

  // Scene highlight decoration machinery
  const setSceneHighlightsEffect = StateEffect.define();
  const setSemanticHighlightsEffect = StateEffect.define();

  const playedMark = Decoration.mark({ class: "cm-scene-played" });
  const activeMark = Decoration.mark({ class: "cm-scene-active" });
  const activeTurnMark = Decoration.mark({ class: "cm-scene-activeTurn" });

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
      keymap.of([indentWithTab]),
      indentUnit.of("  "),
      EditorState.tabSize.of(2),
      EditorView.lineWrapping,
      updateListener,
      sceneHighlightField,
      semanticHighlightField,
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

  export function insertText(text) {
    if (!view || readOnly) return;
    const insert = text == null ? "" : String(text);
    if (!insert) return;
    const selection = view.state.selection.main;
    view.dispatch({
      changes: { from: selection.from, to: selection.to, insert },
      selection: { anchor: selection.from + insert.length }
    });
    view.focus();
  }

  export function jumpToNextDiagnostic() {
    if (!view) return;
    nextDiagnostic(view);
    view.focus();
  }

  export function jumpToPreviousDiagnostic() {
    if (!view) return;
    previousDiagnostic(view);
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
    const docLen = doc.length;
    const ranges = [];
    for (const h of highlights) {
      // lower/upper are character offsets into the script text
      if (h.lower < 0 || h.upper < 0 || h.lower >= docLen) continue;
      const from = doc.lineAt(Math.min(h.lower, docLen)).from;
      const to = doc.lineAt(Math.min(h.upper, docLen)).to;
      if (from >= to) continue;
      const mark = h.type === "activeTurn" ? activeTurnMark
                 : h.type === "active" ? activeMark : playedMark;
      ranges.push(mark.range(from, to));
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
      if (from < 0 || to <= from) continue;
      const kind = String(mark?.kind || "").toLowerCase();
      let deco = semanticSubjectMark;
      if (kind === "verb") deco = semanticVerbMark;
      else if (kind === "object") deco = semanticObjectMark;
      else if (kind === "predicate") deco = semanticPredicateMark;
      else if (kind === "address") deco = semanticAddressMark;
      else if (kind === "address-head") deco = semanticAddressHeadMark;
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
</script>

<div class="script-editor" class:has-error={hasServerError} bind:this={host}></div>
