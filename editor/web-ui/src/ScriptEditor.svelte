<script>
  import { onDestroy, onMount } from "svelte";
  import { EditorState } from "@codemirror/state";
  import { EditorView, keymap } from "@codemirror/view";
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
</script>

<div class="script-editor" class:has-error={hasServerError} bind:this={host}></div>
