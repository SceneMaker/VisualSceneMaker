<script>
  import { onMount, onDestroy } from "svelte";
  import { EditorView, basicSetup } from "codemirror";
  import { EditorState } from "@codemirror/state";
  import { json } from "@codemirror/lang-json";
  import { linter, lintGutter } from "@codemirror/lint";

  export let projectId = null;
  export let plugin = null;
  export let apiGet;
  export let apiPut;
  export let onClose = () => {};

  // ── derived ──────────────────────────────────────────────────────────────
  $: pluginLabel = plugin?.meta?.plugin?.name || plugin?.instanceName || "Screen Editor";

  // Parse screen names reactively from the live JSON in the editor
  $: screenNames = (() => {
    try {
      const parsed = JSON.parse(schema);
      return Object.keys(parsed?.screens ?? {});
    } catch { return []; }
  })();

  // Keep selectedScreen valid when screenNames changes
  $: if (screenNames.length > 0 && !screenNames.includes(selectedScreen)) {
    selectedScreen = screenNames[0];
  }

  // Preview is served via the main WebUiServer (port 8090) — works without
  // the plugin runtime server being active.
  $: previewUrl = `/screens-preview.html?project=${projectId}&screen=${encodeURIComponent(selectedScreen)}&r=${previewKey}`;

  // ── state ─────────────────────────────────────────────────────────────────
  let editorContainer;   // bound to the always-present div
  let view = null;
  let schema       = "";
  let variables    = [];
  let selectedScreen = "";
  let dirty        = false;
  let saveBusy     = false;
  let saveError    = "";
  let saveOk       = false;
  let loadError    = "";
  let loading      = true;
  let previewKey   = 0;

  // ── JSON linter ───────────────────────────────────────────────────────────
  const jsonLinter = linter((editorView) => {
    const text = editorView.state.doc.toString();
    if (!text.trim()) return [];
    try { JSON.parse(text); return []; }
    catch (e) {
      const m = e.message.match(/position (\d+)/);
      const pos = m ? Math.min(parseInt(m[1]), text.length - 1) : 0;
      return [{ from: pos, to: pos + 1, severity: "error", message: e.message }];
    }
  });

  // ── lifecycle ─────────────────────────────────────────────────────────────
  onMount(async () => {
    mountEditor("");
    await loadData();
  });

  onDestroy(() => {
    if (view) { view.destroy(); view = null; }
  });

  // ── data ──────────────────────────────────────────────────────────────────
  async function loadData() {
    loading   = true;
    loadError = "";
    try {
      const [screensRes, varsRes] = await Promise.all([
        apiGet(`/api/v1/projects/${projectId}/screens`),
        apiGet(`/api/v1/projects/${projectId}/variables`),
      ]);
      const isEmpty = !screensRes || Object.keys(screensRes).length === 0;
      const content = JSON.stringify(isEmpty ? minimalTemplate() : screensRes, null, 2);
      setEditorContent(content);
      variables = varsRes?.variables ?? [];
    } catch (e) {
      loadError = e.message || "Failed to load screens data.";
    } finally {
      loading = false;
      dirty   = false;
    }
  }

  function minimalTemplate() {
    return {
      version: 1,
      screens: {
        welcome: {
          background: "#ffffff",
          layout: "flex-column",
          elements: [
            { type: "sl-text", content: "Hello from VSM!" },
            { type: "sl-button", id: "btn1", label: "Continue",
              sendsVar: "gui_info", sendsValue: "user_ready" },
          ],
        },
      },
    };
  }

  // ── editor ────────────────────────────────────────────────────────────────
  function mountEditor(initialContent) {
    if (!editorContainer || view) return;
    view = new EditorView({
      state: EditorState.create({
        doc: initialContent,
        extensions: [
          basicSetup,
          json(),
          jsonLinter,
          lintGutter(),
          EditorView.updateListener.of((update) => {
            if (update.docChanged) {
              schema = update.state.doc.toString();
              dirty  = true;
              saveOk = false;
            }
          }),
          EditorView.theme({
            "&":            { height: "100%", fontSize: "0.875rem" },
            ".cm-scroller": { overflow: "auto",
                              fontFamily: "'IBM Plex Mono','Fira Mono',monospace" },
          }),
        ],
      }),
      parent: editorContainer,
    });
  }

  function setEditorContent(content) {
    schema = content;
    if (!view) return;
    view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: content } });
  }

  // ── save ──────────────────────────────────────────────────────────────────
  async function save() {
    if (saveBusy) return;
    saveError = "";
    saveOk    = false;
    let parsed;
    try { parsed = JSON.parse(schema); }
    catch { saveError = "Fix JSON errors before saving."; return; }
    saveBusy = true;
    try {
      await apiPut(`/api/v1/projects/${projectId}/screens`, parsed);
      dirty      = false;
      saveOk     = true;
      previewKey++;
    } catch (e) {
      saveError = e.message || "Save failed.";
    } finally {
      saveBusy = false;
    }
  }

  function handleKeydown(e) {
    if ((e.metaKey || e.ctrlKey) && e.key === "s") { e.preventDefault(); save(); }
    if (e.key === "Escape") onClose();
  }
</script>

<!-- svelte-ignore a11y-no-noninteractive-element-interactions -->
<div class="se-overlay" role="dialog" aria-modal="true" on:keydown={handleKeydown}>

  <!-- Header -->
  <div class="se-header">
    <div class="se-title">
      <span class="se-title-main">Screen Editor</span>
      <span class="se-title-sep">—</span>
      <span class="se-title-plugin">{pluginLabel}</span>
    </div>
    <div class="se-header-actions">
      {#if loadError}
        <span class="se-badge se-badge-error" title={loadError}>Load error</span>
      {:else if saveError}
        <span class="se-badge se-badge-error">{saveError}</span>
      {:else if saveOk}
        <span class="se-badge se-badge-ok">Saved</span>
      {:else if dirty}
        <span class="se-badge se-badge-warn">Unsaved changes</span>
      {/if}
      <button class="se-btn se-btn-primary" disabled={saveBusy || !dirty}
              on:click={save} title="Save (Cmd/Ctrl+S)">
        {saveBusy ? "Saving…" : "Save"}
      </button>
      <button class="se-btn" on:click={onClose} title="Close (Esc)">Close</button>
    </div>
  </div>

  <!-- Body -->
  <div class="se-body">

    <!-- Left: JSON editor -->
    <div class="se-editor-col">

      <!-- Loading / error overlay drawn on top of the always-present editor div -->
      {#if loadError}
        <div class="se-editor-overlay">
          <span class="se-message-error">{loadError}</span>
          <button class="se-btn se-btn-sm" on:click={loadData}>Retry</button>
        </div>
      {:else if loading}
        <div class="se-editor-overlay">
          <span class="se-loading-text">Loading…</span>
        </div>
      {/if}

      <!-- Editor container: always in DOM so bind:this + onMount work -->
      <div class="se-cm-wrap" bind:this={editorContainer}></div>

      <!-- Variable hint bar -->
      {#if variables.length > 0}
        <div class="se-var-bar">
          <span class="se-var-label">SceneFlow variables</span>
          {#each variables as v}
            <span class="se-var-chip" title="type: {v.type}">{v.name}</span>
          {/each}
        </div>
      {/if}
    </div>

    <!-- Right: preview -->
    <div class="se-preview-col">
      <div class="se-preview-header">
        <span class="se-preview-label">Preview</span>

        <!-- Screen selector -->
        {#if screenNames.length > 0}
          <div class="se-screen-selector">
            <span class="se-screen-label">Screen</span>
            <select class="se-screen-select" bind:value={selectedScreen}>
              {#each screenNames as name}
                <option value={name}>{name}</option>
              {/each}
            </select>
          </div>
        {/if}

        <button class="se-btn se-btn-sm" on:click={() => previewKey++}
                title="Reload preview">↺ Reload</button>
      </div>

      {#if loading}
        <div class="se-preview-hint">Loading…</div>
      {:else}
        <iframe class="se-preview-frame" src={previewUrl}
                title="Screen preview"></iframe>
      {/if}
    </div>

  </div>
</div>

<style>
  .se-overlay {
    position: fixed; inset: 0; z-index: 600;
    display: flex; flex-direction: column;
    background: var(--panel, #ffffff);
    color: var(--ink, #1f2328);
  }

  /* Header */
  .se-header {
    display: flex; align-items: center; justify-content: space-between;
    padding: 0.45rem 0.9rem;
    border-bottom: 1px solid var(--stroke, #e3ddd4);
    flex-shrink: 0; gap: 0.75rem;
    background: var(--panel, #ffffff);
  }
  .se-title { display: flex; align-items: baseline; gap: 0.4rem; font-size: 1rem; }
  .se-title-main   { font-weight: 700; }
  .se-title-sep    { opacity: 0.35; }
  .se-title-plugin { color: var(--accent, #5b8edc); font-weight: 500; }
  .se-header-actions { display: flex; align-items: center; gap: 0.5rem; }

  /* Badges */
  .se-badge { font-size: 0.78rem; padding: 0.15rem 0.55rem; border-radius: 99px; font-weight: 500; }
  .se-badge-error { background: #fde8e5; color: var(--danger, #e26d5a); }
  .se-badge-ok    { background: #e5f5ec; color: #2a7a48; }
  .se-badge-warn  { background: #fef5e0; color: #8a6300; }

  /* Buttons */
  .se-btn {
    padding: 0.28rem 0.75rem;
    border: 1px solid var(--stroke, #e3ddd4); border-radius: 6px;
    background: var(--panel-soft, #f5f7fb); color: var(--ink, #1f2328);
    cursor: pointer; font-size: 0.875rem; font-family: inherit; white-space: nowrap;
  }
  .se-btn:hover:not(:disabled) { background: var(--accent-soft, #d6e2f6); }
  .se-btn:disabled { opacity: 0.4; cursor: default; }
  .se-btn-primary {
    background: var(--button, #5b8edc); color: #fff;
    border-color: transparent; font-weight: 600;
  }
  .se-btn-primary:hover:not(:disabled) { background: var(--button-pressed, #416aa6); }
  .se-btn-sm { padding: 0.12rem 0.5rem; font-size: 0.8rem; }

  /* Body */
  .se-body { display: flex; flex: 1; min-height: 0; overflow: hidden; }

  /* Editor column */
  .se-editor-col {
    display: flex; flex-direction: column;
    flex: 1; min-width: 0; position: relative;
    border-right: 1px solid var(--stroke, #e3ddd4);
  }

  /* Loading / error overlay */
  .se-editor-overlay {
    position: absolute; inset: 0; z-index: 2;
    display: flex; align-items: center; justify-content: center; gap: 0.75rem;
    background: var(--panel, #ffffff);
    font-size: 0.9rem;
  }
  .se-loading-text { opacity: 0.45; }
  .se-message-error { color: var(--danger, #e26d5a); }

  /* CodeMirror wrapper: always present, fills remaining space */
  .se-cm-wrap { flex: 1; min-height: 0; overflow: hidden; }

  /* Variable bar */
  .se-var-bar {
    display: flex; flex-wrap: wrap; align-items: center; gap: 0.3rem;
    padding: 0.4rem 0.7rem;
    border-top: 1px solid var(--stroke, #e3ddd4);
    font-size: 0.8rem; flex-shrink: 0;
    background: var(--panel-soft, #f5f7fb);
  }
  .se-var-label {
    opacity: 0.55; white-space: nowrap; margin-right: 0.15rem;
    font-size: 0.77rem; text-transform: uppercase; letter-spacing: 0.04em;
  }
  .se-var-chip {
    background: var(--accent-soft, #d6e2f6); color: var(--ink, #1f2328);
    border-radius: 4px; padding: 0.1rem 0.45rem;
    font-family: 'IBM Plex Mono', monospace; font-size: 0.78rem;
    cursor: default; border: 1px solid transparent;
  }
  .se-var-chip:hover { border-color: var(--accent, #5b8edc); }

  /* Preview column */
  .se-preview-col {
    display: flex; flex-direction: column;
    flex: 1; min-width: 0;
    background: var(--panel-soft, #f5f7fb);
  }
  .se-preview-header {
    display: flex; align-items: center; justify-content: space-between;
    padding: 0.3rem 0.7rem; gap: 0.6rem;
    border-bottom: 1px solid var(--stroke, #e3ddd4);
    flex-shrink: 0; background: var(--panel, #ffffff);
  }
  .se-preview-label {
    font-size: 0.8rem; font-weight: 600;
    text-transform: uppercase; letter-spacing: 0.05em; opacity: 0.55;
    flex-shrink: 0;
  }

  /* Screen selector */
  .se-screen-selector {
    display: flex; align-items: center; gap: 0.4rem;
    flex: 1; min-width: 0;
  }
  .se-screen-label {
    font-size: 0.78rem; opacity: 0.55; white-space: nowrap;
    text-transform: uppercase; letter-spacing: 0.04em;
  }
  .se-screen-select {
    flex: 1; min-width: 0; max-width: 240px;
    padding: 0.18rem 0.5rem;
    border: 1px solid var(--stroke, #e3ddd4); border-radius: 5px;
    background: var(--panel-soft, #f5f7fb); color: var(--ink, #1f2328);
    font-size: 0.875rem; font-family: 'IBM Plex Mono', monospace;
    cursor: pointer;
  }

  .se-preview-hint {
    flex: 1; display: flex; align-items: center; justify-content: center;
    padding: 2rem; opacity: 0.4; font-size: 0.9rem; text-align: center;
  }
  .se-preview-frame { flex: 1; width: 100%; border: none; background: #fff; }
</style>
