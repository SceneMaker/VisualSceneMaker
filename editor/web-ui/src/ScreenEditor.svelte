<script>
  import { onMount, onDestroy } from "svelte";
  import { EditorView, basicSetup } from "codemirror";
  import { EditorState } from "@codemirror/state";
  import { json } from "@codemirror/lang-json";
  import { linter, lintGutter } from "@codemirror/lint";

  export let projectId = null;
  export let plugin    = null;
  export let apiGet;
  export let apiPut;
  export let onClose = () => {};

  // ── alignment option sets (shared between screen, panels, elements) ───────
  const fontOpts = [
    { v: "",                                        label: "— default —"    },
    { v: "system-ui, sans-serif",                   label: "System UI"      },
    { v: "Arial, Helvetica, sans-serif",             label: "Arial"          },
    { v: "Verdana, Geneva, sans-serif",              label: "Verdana"        },
    { v: "'Trebuchet MS', Helvetica, sans-serif",    label: "Trebuchet MS"   },
    { v: "'Segoe UI', Tahoma, sans-serif",           label: "Segoe UI"       },
    { v: "Calibri, Candara, sans-serif",             label: "Calibri"        },
    { v: "Georgia, 'Times New Roman', serif",        label: "Georgia"        },
    { v: "'Times New Roman', Times, serif",          label: "Times New Roman"},
    { v: "'Courier New', Courier, monospace",        label: "Courier New"    },
    { v: "Consolas, 'Lucida Console', monospace",    label: "Consolas"       },
  ];
  const textAlignOpts = [
    { v: "left",    label: "← Left"   },
    { v: "center",  label: "⊙ Center" },
    { v: "right",   label: "→ Right"  },
    { v: "justify", label: "↔ Justify"},
  ];

  const alignItemsOpts = [
    { v: "flex-start", label: "← Left"    },
    { v: "center",     label: "⊙ Center"  },
    { v: "flex-end",   label: "→ Right"   },
    { v: "stretch",    label: "↔ Fill"    },
  ];
  const justifyOpts = [
    { v: "flex-start",    label: "↑ Top"    },
    { v: "center",        label: "⊙ Center" },
    { v: "flex-end",      label: "↓ Bottom" },
    { v: "space-between", label: "↕ Spread" },
  ];

  // ── derived ──────────────────────────────────────────────────────────────
  $: pluginLabel = plugin?.meta?.plugin?.name || plugin?.instanceName || "Screen Editor";

  $: screenNames = (() => {
    try { return Object.keys(JSON.parse(schema)?.screens ?? {}); }
    catch { return []; }
  })();

  $: if (screenNames.length > 0 && !screenNames.includes(selectedScreen))
    selectedScreen = screenNames[0];

  // When the user picks a different screen in the selector, switch the preview.
  $: previewLoaded && previewIframe?.contentWindow?.postMessage(
       { cmd: 'loadScreen', screen: selectedScreen }, '*');

  // Reset ready-flag whenever the iframe is forced to reload (previewKey++).
  $: { previewKey; previewLoaded = false; }

  $: previewUrl = `/screens-preview.html?project=${projectId}&screen=${encodeURIComponent(selectedScreen)}&r=${previewKey}`;

  $: veScreen   = parsedSchema?.screens?.[selectedScreen] ?? null;
  $: veElements = veScreen?.elements ?? [];

  // ── state ─────────────────────────────────────────────────────────────────
  let editorContainer;
  let view           = null;
  let schema         = "";
  let variables      = [];
  let selectedScreen = "";
  let dirty          = false;
  let saveBusy       = false;
  let saveError      = "";
  let saveOk         = false;
  let loadError      = "";
  let loading        = true;
  let previewKey     = 0;

  let editorMode    = "visual";
  let parsedSchema  = null;
  let expandedEl    = null;          // index of expanded top-level element
  let expandedChild = null;          // { pi, ci } for expanded panel child
  let modeError     = "";

  let previewIframe  = null;         // bound to the preview <iframe>
  let previewLoaded  = false;        // true once the iframe fires its load event

  // ── JSON linter ───────────────────────────────────────────────────────────
  const jsonLinter = linter((ev) => {
    const text = ev.state.doc.toString();
    if (!text.trim()) return [];
    try { JSON.parse(text); return []; }
    catch (e) {
      const m = e.message.match(/position (\d+)/);
      const pos = m ? Math.min(parseInt(m[1]), text.length - 1) : 0;
      return [{ from: pos, to: pos + 1, severity: "error", message: e.message }];
    }
  });

  // ── lifecycle ─────────────────────────────────────────────────────────────
  onMount(async () => { mountEditor(""); await loadData(); });
  onDestroy(() => { if (view) { view.destroy(); view = null; } });

  // ── data ──────────────────────────────────────────────────────────────────
  async function loadData() {
    loading = true; loadError = "";
    try {
      const [sRes, vRes] = await Promise.all([
        apiGet(`/api/v1/projects/${projectId}/screens`),
        apiGet(`/api/v1/projects/${projectId}/variables`),
      ]);
      const empty   = !sRes || Object.keys(sRes).length === 0;
      const content = JSON.stringify(empty ? minimalTemplate() : sRes, null, 2);
      setEditorContent(content);
      variables    = vRes?.variables ?? [];
      parsedSchema = JSON.parse(content);
    } catch (e) {
      loadError = e.message || "Failed to load screens data.";
    } finally { loading = false; dirty = false; }
  }

  function minimalTemplate() {
    return {
      version: 1,
      screens: {
        welcome: {
          background: "#ffffff", layout: "flex-column",
          elements: [
            { type: "sl-text", content: "Hello from VSM!" },
            { type: "sl-button", id: "btn1", label: "Continue",
              sendsVar: "gui_info", sendsValue: "user_ready" },
          ],
        },
      },
    };
  }

  // ── color + opacity helpers ───────────────────────────────────────────────
  // Returns { hex: '#rrggbb', opacity: 0-100 } from any supported CSS colour.
  function parseColorAlpha(val) {
    if (!val || val === 'transparent') return { hex: '#ffffff', opacity: 0 };
    if (/^#[0-9a-f]{8}$/i.test(val))
      return { hex: val.slice(0,7), opacity: Math.round(parseInt(val.slice(7,9), 16) / 255 * 100) };
    const m = val.match(/rgba\((\d+),\s*(\d+),\s*(\d+),\s*([\d.]+)\)/);
    if (m) {
      const hex = '#' + [m[1],m[2],m[3]].map(n => parseInt(n).toString(16).padStart(2,'0')).join('');
      return { hex, opacity: Math.round(parseFloat(m[4]) * 100) };
    }
    return { hex: val.startsWith('#') ? val.slice(0,7) : '#000000', opacity: 100 };
  }
  function buildColorAlpha(hex, opacity) {
    const op = Math.max(0, Math.min(100, Math.round(+opacity)));
    if (op >= 100) return hex;
    if (op <=  0) return 'transparent';
    return hex + Math.round(op / 100 * 255).toString(16).padStart(2,'0');
  }

  // ── live preview ──────────────────────────────────────────────────────────
  function sendSchemaToPreview() {
    if (!previewLoaded || !previewIframe?.contentWindow || !parsedSchema) return;
    previewIframe.contentWindow.postMessage({ cmd: 'loadSchema', schema: parsedSchema }, '*');
    previewIframe.contentWindow.postMessage({ cmd: 'loadScreen', screen: selectedScreen }, '*');
  }
  function onPreviewLoad() { previewLoaded = true; sendSchemaToPreview(); }

  // ── CodeMirror ────────────────────────────────────────────────────────────
  function mountEditor(init) {
    if (!editorContainer || view) return;
    view = new EditorView({
      state: EditorState.create({
        doc: init,
        extensions: [
          basicSetup, json(), jsonLinter, lintGutter(),
          EditorView.updateListener.of((u) => {
            if (u.docChanged) { schema = u.state.doc.toString(); dirty = true; saveOk = false; }
          }),
          EditorView.theme({
            "&":            { height: "100%", fontSize: "0.875rem" },
            ".cm-scroller": { overflow: "auto", fontFamily: "'IBM Plex Mono','Fira Mono',monospace" },
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

  // ── tab switching ─────────────────────────────────────────────────────────
  function switchMode(mode) {
    modeError = "";
    if (mode === "visual") {
      try { parsedSchema = JSON.parse(schema); }
      catch { modeError = "Fix JSON errors before switching to Visual."; return; }
    }
    editorMode = mode;
  }

  // ── visual editor: commit ────────────────────────────────────────────────
  function commitParsed() {
    const str = JSON.stringify(parsedSchema, null, 2);
    setEditorContent(str);
    dirty = true; saveOk = false;
    sendSchemaToPreview();
  }

  // ── visual editor: screen props ───────────────────────────────────────────
  function setScreenProp(key, value) {
    if (!parsedSchema?.screens?.[selectedScreen]) return;
    if (value === undefined || value === "") {
      delete parsedSchema.screens[selectedScreen][key];
    } else {
      parsedSchema.screens[selectedScreen][key] = value;
    }
    parsedSchema = { ...parsedSchema };
    commitParsed();
  }

  // ── visual editor: top-level element props ────────────────────────────────
  function setProp(i, key, value) {
    const els = [...veElements];
    if (value === undefined) delete els[i][key];
    else els[i] = { ...els[i], [key]: value };
    parsedSchema.screens[selectedScreen].elements = els;
    parsedSchema = { ...parsedSchema };
    commitParsed();
  }

  function setStyleProp(i, key, value) {
    const els = [...veElements];
    const style = { ...(els[i].style ?? {}) };
    if (value === "" || value === undefined) delete style[key];
    else style[key] = value;
    els[i] = { ...els[i], style: Object.keys(style).length ? style : undefined };
    if (!els[i].style) delete els[i].style;
    parsedSchema.screens[selectedScreen].elements = els;
    parsedSchema = { ...parsedSchema };
    commitParsed();
  }

  function addElement(el) {
    const screen = parsedSchema?.screens?.[selectedScreen];
    if (!screen) return;
    screen.elements  = [...(screen.elements ?? []), el];
    parsedSchema     = { ...parsedSchema };
    expandedEl       = screen.elements.length - 1;
    expandedChild    = null;
    commitParsed();
  }

  function deleteElement(i) {
    const screen = parsedSchema?.screens?.[selectedScreen];
    if (!screen) return;
    screen.elements = screen.elements.filter((_, idx) => idx !== i);
    parsedSchema = { ...parsedSchema };
    if (expandedEl === i) expandedEl = null;
    else if (expandedEl > i) expandedEl--;
    commitParsed();
  }

  function moveElement(i, dir) {
    const screen = parsedSchema?.screens?.[selectedScreen];
    if (!screen) return;
    const els = [...screen.elements], j = i + dir;
    if (j < 0 || j >= els.length) return;
    [els[i], els[j]] = [els[j], els[i]];
    screen.elements = els;
    parsedSchema = { ...parsedSchema };
    expandedEl = j;
    commitParsed();
  }

  // ── visual editor: panel child props ─────────────────────────────────────
  function setChildProp(pi, ci, key, value) {
    const els = [...veElements];
    const ch  = [...(els[pi].children ?? [])];
    if (value === undefined) delete ch[ci][key];
    else ch[ci] = { ...ch[ci], [key]: value };
    els[pi] = { ...els[pi], children: ch };
    parsedSchema.screens[selectedScreen].elements = els;
    parsedSchema = { ...parsedSchema };
    commitParsed();
  }

  function setChildStyleProp(pi, ci, key, value) {
    const els   = [...veElements];
    const ch    = [...(els[pi].children ?? [])];
    const style = { ...(ch[ci].style ?? {}) };
    if (value === "" || value === undefined) delete style[key];
    else style[key] = value;
    ch[ci] = { ...ch[ci], style: Object.keys(style).length ? style : undefined };
    if (!ch[ci].style) delete ch[ci].style;
    els[pi] = { ...els[pi], children: ch };
    parsedSchema.screens[selectedScreen].elements = els;
    parsedSchema = { ...parsedSchema };
    commitParsed();
  }

  function addChild(pi, el) {
    const els = [...veElements];
    els[pi] = { ...els[pi], children: [...(els[pi].children ?? []), el] };
    parsedSchema.screens[selectedScreen].elements = els;
    parsedSchema = { ...parsedSchema };
    expandedChild = { pi, ci: els[pi].children.length - 1 };
    commitParsed();
  }

  function deleteChild(pi, ci) {
    const els = [...veElements];
    els[pi] = { ...els[pi], children: (els[pi].children ?? []).filter((_, idx) => idx !== ci) };
    parsedSchema.screens[selectedScreen].elements = els;
    parsedSchema = { ...parsedSchema };
    if (expandedChild?.pi === pi && expandedChild?.ci === ci) expandedChild = null;
    commitParsed();
  }

  function moveChild(pi, ci, dir) {
    const els = [...veElements];
    const ch  = [...(els[pi].children ?? [])], j = ci + dir;
    if (j < 0 || j >= ch.length) return;
    [ch[ci], ch[j]] = [ch[j], ch[ci]];
    els[pi] = { ...els[pi], children: ch };
    parsedSchema.screens[selectedScreen].elements = els;
    parsedSchema = { ...parsedSchema };
    expandedChild = { pi, ci: j };
    commitParsed();
  }

  // ── visual editor: helpers ────────────────────────────────────────────────
  function typeLabel(type) {
    if (!type) return "?";
    if (type === "vsm-panel")       return "▣";
    if (type === "vsm-filler")      return "↕";
    if (type === "vsm-image")       return "Img";
    if (type === "vsm-video")       return "▶";
    if (type === "vsm-audio")       return "♪";
    if (type === "vsm-embed")       return "⊞";
    if (type === "vsm-bubble")      return "💬";
    if (type === "vsm-chart")       return "Ch";
    if (type === "vsm-feed")        return "📜";
    if (type.includes("textarea"))  return "A";
    if (type.includes("text"))      return "T";
    if (type.includes("button"))    return "B";
    if (type.includes("range"))     return "S";
    if (type.includes("input"))     return "I";
    if (type.includes("select"))    return "▾";
    if (type.includes("checkbox"))  return "✓";
    return "□";
  }

  function elementSummary(el) {
    if (el.type === "vsm-panel") {
      const n = (el.children ?? []).length;
      return `Panel · ${n} element${n !== 1 ? "s" : ""}`;
    }
    if (el.type === "vsm-image")  return el.src  ? `Image · ${el.src}`  : "Image";
    if (el.type === "vsm-video")  return el.src  ? `Video · ${el.src}`  : "Video";
    if (el.type === "vsm-audio")  return el.src  ? `Audio · ${el.src}`  : "Audio";
    if (el.type === "vsm-embed")  return el.src  ? `Embed · ${el.src}`  : "Embed";
    if (el.type === "vsm-bubble") {
      const spk = el.speaker ? `${el.speaker}: ` : "";
      const txt = el.bindVar ? `[${el.bindVar}]` : (el.content ?? "");
      return `${spk}${txt}` || "Bubble";
    }
    if (el.type === "vsm-chart")  return el.dataVar ? `${el.chartType ?? "bar"} · ${el.dataVar}` : (el.chartType ?? "bar");
    if (el.type === "vsm-feed")   return el.dataVar ? `Feed · ${el.dataVar}` : "Feed (no variable)";
    if (el.type === "vsm-filler") {
      if (el.flexGrow) return "Flex spacer (fills remaining space)";
      const parts = [];
      if (el.width)  parts.push(`w: ${el.width}`);
      if (el.height) parts.push(`h: ${el.height}`);
      return parts.length ? parts.join("  ") : "Filler";
    }
    return el.content ?? el.label ?? el.type ?? "";
  }

  function optionsToText(options) {
    return (options ?? []).map(o =>
      typeof o === "object"
        ? (o.label && o.label !== o.value ? `${o.value}=${o.label}` : o.value)
        : String(o)
    ).join("\n");
  }

  function textToOptions(text) {
    return text.split("\n").map(l => l.trim()).filter(Boolean).map(l => {
      const eq = l.indexOf("=");
      if (eq > 0) return { value: l.slice(0, eq).trim(), label: l.slice(eq + 1).trim() };
      return l;
    });
  }

  // ── save ──────────────────────────────────────────────────────────────────
  async function save() {
    if (saveBusy) return;
    saveError = ""; saveOk = false;
    let parsed;
    try { parsed = JSON.parse(schema); }
    catch { saveError = "Fix JSON errors before saving."; return; }
    saveBusy = true;
    try {
      await apiPut(`/api/v1/projects/${projectId}/screens`, parsed);
      parsedSchema = parsed;
      dirty = false; saveOk = true;
      sendSchemaToPreview();
    } catch (e) {
      saveError = e.message || "Save failed.";
    } finally { saveBusy = false; }
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
      {:else if modeError}
        <span class="se-badge se-badge-error">{modeError}</span>
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
    <div class="se-editor-col">

      {#if loadError}
        <div class="se-editor-overlay">
          <span class="se-message-error">{loadError}</span>
          <button class="se-btn se-btn-sm" on:click={loadData}>Retry</button>
        </div>
      {:else if loading}
        <div class="se-editor-overlay"><span class="se-loading-text">Loading…</span></div>
      {/if}

      <!-- Tab bar -->
      <div class="se-tab-bar">
        <button class="se-tab" class:se-tab-active={editorMode === "visual"}
                on:click={() => switchMode("visual")}>Visual</button>
        <button class="se-tab" class:se-tab-active={editorMode === "code"}
                on:click={() => switchMode("code")}>JSON</button>
      </div>

      <!-- ── Visual editor ── -->
      {#if editorMode === "visual"}
      <div class="ve-root">
        {#if veScreen}

          <!-- Screen settings -->
          <div class="ve-section">
            <div class="ve-section-title">Screen</div>
            <div class="ve-row">
              <label class="ve-label">Background</label>
              <input class="ve-color" type="color"
                     value={parseColorAlpha(veScreen.background ?? '#ffffff').hex}
                     on:input={e => setScreenProp("background",
                       buildColorAlpha(e.target.value, parseColorAlpha(veScreen.background ?? '#ffffff').opacity))}>
              <input class="ve-opacity" type="number" min="0" max="100"
                     value={parseColorAlpha(veScreen.background ?? '#ffffff').opacity}
                     on:input={e => setScreenProp("background",
                       buildColorAlpha(parseColorAlpha(veScreen.background ?? '#ffffff').hex, e.target.value))}>
              <span class="ve-opacity-unit">%</span>
              <label class="ve-label" style="margin-left:.5rem">Layout</label>
              <select class="ve-select"
                      value={veScreen.layout ?? "flex-column"}
                      on:change={e => setScreenProp("layout", e.target.value)}>
                <option value="flex-column">Column</option>
                <option value="flex-row">Row</option>
              </select>
            </div>
            <div class="ve-row">
              <label class="ve-label">Horizontal</label>
              <div class="ve-align-row">
                {#each ((veScreen.layout ?? 'flex-column') === 'flex-row' ? justifyOpts : alignItemsOpts) as opt}
                  <button class="ve-align-btn"
                          class:ve-align-active={veScreen.alignItems === opt.v}
                          on:click={() => setScreenProp("alignItems",
                            veScreen.alignItems === opt.v ? undefined : opt.v)}
                          title={opt.label}>{opt.label}</button>
                {/each}
              </div>
            </div>
            <div class="ve-row">
              <label class="ve-label">Vertical</label>
              <div class="ve-align-row">
                {#each ((veScreen.layout ?? 'flex-column') === 'flex-row' ? alignItemsOpts : justifyOpts) as opt}
                  <button class="ve-align-btn"
                          class:ve-align-active={veScreen.justifyContent === opt.v}
                          on:click={() => setScreenProp("justifyContent",
                            veScreen.justifyContent === opt.v ? undefined : opt.v)}
                          title={opt.label}>{opt.label}</button>
                {/each}
              </div>
            </div>
          </div>

          <!-- Elements list -->
          <div class="ve-section ve-section-grow">
            <div class="ve-elements-header">
              <span class="ve-section-title">Elements</span>
              <div class="ve-add-group">
                <button class="ve-add-btn" on:click={() => addElement({ type:"sl-text", content:"Text" })}>+Text</button>
                <button class="ve-add-btn" on:click={() => addElement({ type:"sl-button", label:"Button", sendsVar:"", sendsValue:"" })}>+Button</button>
                <button class="ve-add-btn" on:click={() => addElement({ type:"sl-range", label:"Slider", min:0, max:100, step:1 })}>+Slider</button>
                <button class="ve-add-btn" on:click={() => addElement({ type:"sl-input", label:"Input", bindVar:"" })}>+Input</button>
                <button class="ve-add-btn" on:click={() => addElement({ type:"sl-textarea", label:"Textarea", bindVar:"" })}>+Textarea</button>
                <button class="ve-add-btn" on:click={() => addElement({ type:"sl-select", label:"Select", options:["Option 1","Option 2"], bindVar:"" })}>+Select</button>
                <button class="ve-add-btn" on:click={() => addElement({ type:"sl-checkbox", label:"Checkbox", bindVar:"" })}>+Checkbox</button>
                <button class="ve-add-btn ve-add-btn-panel" on:click={() => addElement({ type:"vsm-panel", background:"#f5f5f5", layout:"flex-column", padding:"1rem", children:[] })}>+Panel</button>
                <button class="ve-add-btn" on:click={() => addElement({ type:"vsm-filler", flexGrow:true })}>+Filler</button>
                <button class="ve-add-btn" on:click={() => addElement({ type:"vsm-image", src:"", alt:"" })}>+Image</button>
                <button class="ve-add-btn" on:click={() => addElement({ type:"vsm-video", src:"", controls:true })}>+Video</button>
                <button class="ve-add-btn" on:click={() => addElement({ type:"vsm-audio", src:"", controls:true })}>+Audio</button>
                <button class="ve-add-btn" on:click={() => addElement({ type:"vsm-embed", src:"", width:"100%", height:"315px" })}>+Embed</button>
                <button class="ve-add-btn" on:click={() => addElement({ type:"vsm-bubble", content:"Hello!", tail:"bottom", background:"#e8f4fd" })}>+Bubble</button>
                <button class="ve-add-btn" on:click={() => addElement({ type:"vsm-chart", chartType:"bar", dataVar:"", label:"", color:"#5b8edc", height:"300px" })}>+Chart</button>
                <button class="ve-add-btn ve-add-btn-feed" on:click={() => addElement({ type:"vsm-feed", dataVar:"", height:"400px", agentColor:"#e8f4fd", userColor:"#eafbe8", systemColor:"#f5f5f5", agentLabel:"Agent", userLabel:"You" })}>+Feed</button>
              </div>
            </div>

            {#if veElements.length === 0}
              <div class="ve-empty">No elements yet — add one above.</div>
            {/if}

            {#each veElements as el, i}
              {@const exp = expandedEl === i}
              <div class="ve-card" class:ve-card-expanded={exp}>

                <div class="ve-card-header" role="button" tabindex="0"
                     on:click={() => { expandedEl = exp ? null : i; if (!exp) expandedChild = null; }}
                     on:keydown={e => e.key==="Enter" && (expandedEl = exp ? null : i)}>
                  <div class="ve-card-arrows">
                    <button class="ve-arrow" disabled={i===0}
                            on:click|stopPropagation={() => moveElement(i,-1)}>▲</button>
                    <button class="ve-arrow" disabled={i===veElements.length-1}
                            on:click|stopPropagation={() => moveElement(i,1)}>▼</button>
                  </div>
                  <span class="ve-type-badge">{typeLabel(el.type)}</span>
                  <span class="ve-card-summary">{elementSummary(el)}</span>
                  <span class="ve-expand-icon">{exp ? "▾" : "▸"}</span>
                  <button class="ve-delete" on:click|stopPropagation={() => deleteElement(i)}>×</button>
                </div>

                {#if exp}
                <div class="ve-props">

                  <!-- ── Align-self (common to all elements) ── -->
                  {#if el.type !== "vsm-panel"}
                  <label class="ve-prop-label">Align in layout</label>
                  <div class="ve-align-row">
                    {#each alignItemsOpts as opt}
                      <button class="ve-align-btn"
                              class:ve-align-active={el.style?.["align-self"] === opt.v}
                              on:click={() => setStyleProp(i, "align-self",
                                el.style?.["align-self"] === opt.v ? "" : opt.v)}
                              title={opt.label}>{opt.label}</button>
                    {/each}
                  </div>
                  {/if}

                  <!-- ── Type-specific props ── -->

                  {#if el.type === "sl-text" || el.type === "wa-text"}
                    <label class="ve-prop-label">Content</label>
                    <textarea class="ve-textarea" rows="2"
                              value={el.content ?? ""}
                              on:input={e => setProp(i,"content",e.target.value)}></textarea>
                    <label class="ve-prop-label">Text color</label>
                    <input class="ve-color" type="color"
                           value={parseColorAlpha(el.style?.color ?? '#000000').hex}
                           on:input={e => setStyleProp(i,"color",
                             buildColorAlpha(e.target.value, parseColorAlpha(el.style?.color ?? '#000000').opacity))}>
                    <input class="ve-opacity" type="number" min="0" max="100"
                           value={parseColorAlpha(el.style?.color ?? '#000000').opacity}
                           on:input={e => setStyleProp(i,"color",
                             buildColorAlpha(parseColorAlpha(el.style?.color ?? '#000000').hex, e.target.value))}>
                    <span class="ve-opacity-unit">%</span>
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Font size</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="1rem"
                             value={el.style?.["font-size"] ?? ""}
                             on:input={e => setStyleProp(i,"font-size",e.target.value || undefined)}>
                    </div>
                    <label class="ve-prop-label">Font</label>
                    <select class="ve-select"
                            value={fontOpts.some(f => f.v === (el.style?.["font-family"] ?? "")) ? (el.style?.["font-family"] ?? "") : "__custom__"}
                            on:change={e => setStyleProp(i,"font-family", e.target.value === "__custom__" ? undefined : (e.target.value || undefined))}>
                      {#each fontOpts as f}<option value={f.v}>{f.label}</option>{/each}
                      {#if el.style?.["font-family"] && !fontOpts.some(f => f.v === el.style?.["font-family"])}
                        <option value="__custom__">{el.style["font-family"]}</option>
                      {/if}
                    </select>
                    <label class="ve-prop-label">Style</label>
                    <div class="ve-align-row">
                      <button class="ve-align-btn"
                              class:ve-align-active={el.style?.["font-weight"] === "bold"}
                              on:click={() => setStyleProp(i,"font-weight",
                                el.style?.["font-weight"] === "bold" ? "" : "bold")}><b>B</b></button>
                      <button class="ve-align-btn"
                              class:ve-align-active={el.style?.["font-style"] === "italic"}
                              on:click={() => setStyleProp(i,"font-style",
                                el.style?.["font-style"] === "italic" ? "" : "italic")}><i>I</i></button>
                      {#each textAlignOpts as opt}
                        <button class="ve-align-btn"
                                class:ve-align-active={el.style?.["text-align"] === opt.v}
                                on:click={() => setStyleProp(i,"text-align",
                                  el.style?.["text-align"] === opt.v ? "" : opt.v)}
                                title={opt.label}>{opt.label}</button>
                      {/each}
                    </div>

                  {:else if el.type === "sl-button" || el.type === "wa-button"}
                    <label class="ve-prop-label">Label</label>
                    <input class="ve-input" type="text" value={el.label ?? ""}
                           on:input={e => setProp(i,"label",e.target.value)}>
                    <label class="ve-prop-label">Variant</label>
                    <select class="ve-select" value={el.variant ?? "default"}
                            on:change={e => setProp(i,"variant",e.target.value)}>
                      <option value="default">Default</option>
                      <option value="primary">Primary</option>
                      <option value="success">Success</option>
                      <option value="warning">Warning</option>
                      <option value="danger">Danger</option>
                    </select>
                    <label class="ve-prop-label">Sends to variable</label>
                    <select class="ve-select" value={el.sendsVar ?? ""}
                            on:change={e => setProp(i,"sendsVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <label class="ve-prop-label">Value to send</label>
                    <input class="ve-input" type="text" value={el.sendsValue ?? ""}
                           on:input={e => setProp(i,"sendsValue",e.target.value)}>

                  {:else if el.type === "sl-range" || el.type === "wa-range"}
                    <label class="ve-prop-label">Label</label>
                    <input class="ve-input" type="text" value={el.label ?? ""}
                           on:input={e => setProp(i,"label",e.target.value)}>
                    <div class="ve-row-trio">
                      <div><label class="ve-prop-label">Min</label>
                        <input class="ve-input" type="number" value={el.min ?? 0}
                               on:input={e => setProp(i,"min",+e.target.value)}></div>
                      <div><label class="ve-prop-label">Max</label>
                        <input class="ve-input" type="number" value={el.max ?? 100}
                               on:input={e => setProp(i,"max",+e.target.value)}></div>
                      <div><label class="ve-prop-label">Step</label>
                        <input class="ve-input" type="number" value={el.step ?? 1}
                               on:input={e => setProp(i,"step",+e.target.value)}></div>
                    </div>
                    <label class="ve-prop-label">Binds to variable</label>
                    <select class="ve-select" value={el.bindVar ?? ""}
                            on:change={e => setProp(i,"bindVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  {:else if el.type === "sl-input" || el.type === "wa-input"}
                    <label class="ve-prop-label">Label</label>
                    <input class="ve-input" type="text" value={el.label ?? ""}
                           on:input={e => setProp(i,"label",e.target.value)}>
                    <label class="ve-prop-label">Placeholder</label>
                    <input class="ve-input" type="text" value={el.placeholder ?? ""}
                           on:input={e => setProp(i,"placeholder",e.target.value)}>
                    <label class="ve-prop-label">Binds to variable</label>
                    <select class="ve-select" value={el.bindVar ?? ""}
                            on:change={e => setProp(i,"bindVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  {:else if el.type === "sl-textarea" || el.type === "wa-textarea"}
                    <label class="ve-prop-label">Label</label>
                    <input class="ve-input" type="text" value={el.label ?? ""}
                           on:input={e => setProp(i,"label",e.target.value)}>
                    <label class="ve-prop-label">Placeholder</label>
                    <input class="ve-input" type="text" value={el.placeholder ?? ""}
                           on:input={e => setProp(i,"placeholder",e.target.value)}>
                    <label class="ve-prop-label">Binds to variable</label>
                    <select class="ve-select" value={el.bindVar ?? ""}
                            on:change={e => setProp(i,"bindVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  {:else if el.type === "sl-select" || el.type === "wa-select"}
                    <label class="ve-prop-label">Label</label>
                    <input class="ve-input" type="text" value={el.label ?? ""}
                           on:input={e => setProp(i,"label",e.target.value)}>
                    <label class="ve-prop-label">Options <span class="ve-hint">(one per line, or value=Label)</span></label>
                    <textarea class="ve-textarea" rows="4"
                              value={optionsToText(el.options)}
                              on:change={e => setProp(i,"options",textToOptions(e.target.value))}></textarea>
                    <label class="ve-prop-label">Binds to variable</label>
                    <select class="ve-select" value={el.bindVar ?? ""}
                            on:change={e => setProp(i,"bindVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  {:else if el.type === "sl-checkbox" || el.type === "wa-checkbox"}
                    <label class="ve-prop-label">Label</label>
                    <input class="ve-input" type="text" value={el.label ?? ""}
                           on:input={e => setProp(i,"label",e.target.value)}>
                    <label class="ve-prop-label">Binds to variable <span class="ve-hint">(stores true/false)</span></label>
                    <select class="ve-select" value={el.bindVar ?? ""}
                            on:change={e => setProp(i,"bindVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  <!-- ── Image ── -->
                  {:else if el.type === "vsm-image"}
                    <div class="ve-media-hint">Place files in <code>screens-assets/</code> inside your project folder and use <code>/assets/filename.ext</code></div>
                    <label class="ve-prop-label">Source</label>
                    <input class="ve-input" type="text" placeholder="/assets/photo.jpg or https://…"
                           value={el.src ?? ""}
                           on:input={e => setProp(i,"src",e.target.value)}>
                    <label class="ve-prop-label">Alt text</label>
                    <input class="ve-input" type="text" placeholder="Description for accessibility"
                           value={el.alt ?? ""}
                           on:input={e => setProp(i,"alt",e.target.value)}>
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Width</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="100%"
                             value={el.width ?? ""}
                             on:input={e => setProp(i,"width",e.target.value || undefined)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="auto"
                             value={el.height ?? ""}
                             on:input={e => setProp(i,"height",e.target.value || undefined)}>
                    </div>
                    <label class="ve-prop-label">Object fit</label>
                    <select class="ve-select" value={el.objectFit ?? ""}
                            on:change={e => setProp(i,"objectFit",e.target.value || undefined)}>
                      <option value="">— default —</option>
                      <option value="contain">Contain (show whole image)</option>
                      <option value="cover">Cover (fill box, crop)</option>
                      <option value="fill">Fill (stretch)</option>
                      <option value="none">None</option>
                    </select>

                  <!-- ── Video ── -->
                  {:else if el.type === "vsm-video"}
                    <div class="ve-media-hint">Place files in <code>screens-assets/</code> inside your project folder and use <code>/assets/filename.ext</code></div>
                    <label class="ve-prop-label">Source</label>
                    <input class="ve-input" type="text" placeholder="/assets/video.mp4 or https://…"
                           value={el.src ?? ""}
                           on:input={e => setProp(i,"src",e.target.value)}>
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Width</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="100%"
                             value={el.width ?? ""}
                             on:input={e => setProp(i,"width",e.target.value || undefined)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="auto"
                             value={el.height ?? ""}
                             on:input={e => setProp(i,"height",e.target.value || undefined)}>
                    </div>
                    <div class="ve-row" style="gap:1rem;flex-wrap:wrap">
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={el.controls !== false}
                               on:change={e => setProp(i,"controls",e.target.checked)}>
                        Controls</label>
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={!!el.autoplay}
                               on:change={e => setProp(i,"autoplay",e.target.checked || undefined)}>
                        Autoplay</label>
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={!!el.loop}
                               on:change={e => setProp(i,"loop",e.target.checked || undefined)}>
                        Loop</label>
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={!!el.muted}
                               on:change={e => setProp(i,"muted",e.target.checked || undefined)}>
                        Muted <span class="ve-hint">(required for autoplay)</span></label>
                    </div>

                  <!-- ── Audio ── -->
                  {:else if el.type === "vsm-audio"}
                    <div class="ve-media-hint">Place files in <code>screens-assets/</code> inside your project folder and use <code>/assets/filename.ext</code></div>
                    <label class="ve-prop-label">Source</label>
                    <input class="ve-input" type="text" placeholder="/assets/sound.mp3 or https://…"
                           value={el.src ?? ""}
                           on:input={e => setProp(i,"src",e.target.value)}>
                    <div class="ve-row" style="gap:1rem;flex-wrap:wrap">
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={el.controls !== false}
                               on:change={e => setProp(i,"controls",e.target.checked)}>
                        Controls</label>
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={!!el.autoplay}
                               on:change={e => setProp(i,"autoplay",e.target.checked || undefined)}>
                        Autoplay</label>
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={!!el.loop}
                               on:change={e => setProp(i,"loop",e.target.checked || undefined)}>
                        Loop</label>
                    </div>

                  <!-- ── Embed (YouTube / iframe) ── -->
                  {:else if el.type === "vsm-embed"}
                    <div class="ve-media-hint">For YouTube use the embed URL: <code>https://www.youtube.com/embed/VIDEO_ID</code></div>
                    <label class="ve-prop-label">Embed URL</label>
                    <input class="ve-input" type="text" placeholder="https://www.youtube.com/embed/…"
                           value={el.src ?? ""}
                           on:input={e => setProp(i,"src",e.target.value)}>
                    <label class="ve-prop-label">Title <span class="ve-hint">(accessibility)</span></label>
                    <input class="ve-input" type="text" placeholder="Video title"
                           value={el.title ?? ""}
                           on:input={e => setProp(i,"title",e.target.value || undefined)}>
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Width</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="100%"
                             value={el.width ?? "100%"}
                             on:input={e => setProp(i,"width",e.target.value)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="315px"
                             value={el.height ?? "315px"}
                             on:input={e => setProp(i,"height",e.target.value)}>
                    </div>

                  <!-- ── Filler ── -->
                  {:else if el.type === "vsm-filler"}
                    <div class="ve-row" style="align-items:center;gap:.5rem">
                      <label class="ve-prop-label" style="min-width:0">Flex grow</label>
                      <input type="checkbox" checked={el.flexGrow ?? false}
                             on:change={e => setProp(i,"flexGrow", e.target.checked)}>
                      <span class="ve-hint" style="margin-left:.25rem">fills remaining space</span>
                    </div>
                    {#if !el.flexGrow}
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Width</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="e.g. 100%"
                             value={el.width ?? ""}
                             on:input={e => setProp(i,"width", e.target.value || undefined)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="e.g. 2rem"
                             value={el.height ?? ""}
                             on:input={e => setProp(i,"height", e.target.value || undefined)}>
                    </div>
                    {/if}

                  <!-- ── Speech Bubble ── -->
                  {:else if el.type === "vsm-bubble"}
                    <label class="ve-prop-label">Content</label>
                    <textarea class="ve-textarea" rows="2"
                              value={el.content ?? ""}
                              on:input={e => setProp(i,"content",e.target.value)}></textarea>
                    <label class="ve-prop-label">Speaker name <span class="ve-hint">(optional label above bubble)</span></label>
                    <input class="ve-input" type="text" placeholder="Agent, User, …"
                           value={el.speaker ?? ""}
                           on:input={e => setProp(i,"speaker",e.target.value || undefined)}>
                    <label class="ve-prop-label">Tail direction <span class="ve-hint">(left/right follows "Align in layout")</span></label>
                    <select class="ve-select" value={el.tail ?? "bottom"}
                            on:change={e => setProp(i,"tail",e.target.value)}>
                      <option value="bottom">Bottom</option>
                      <option value="top">Top</option>
                      <option value="">None</option>
                    </select>
                    <label class="ve-prop-label">Background</label>
                    <input class="ve-color" type="color"
                           value={parseColorAlpha(el.background ?? '#e8f4fd').hex}
                           on:input={e => setProp(i,"background",
                             buildColorAlpha(e.target.value, parseColorAlpha(el.background ?? '#e8f4fd').opacity))}>
                    <input class="ve-opacity" type="number" min="0" max="100"
                           value={parseColorAlpha(el.background ?? '#e8f4fd').opacity}
                           on:input={e => setProp(i,"background",
                             buildColorAlpha(parseColorAlpha(el.background ?? '#e8f4fd').hex, e.target.value))}>
                    <span class="ve-opacity-unit">%</span>
                    <label class="ve-prop-label">Text color</label>
                    <input class="ve-color" type="color"
                           value={parseColorAlpha(el.style?.color ?? '#000000').hex}
                           on:input={e => setStyleProp(i,"color",
                             buildColorAlpha(e.target.value, parseColorAlpha(el.style?.color ?? '#000000').opacity))}>
                    <input class="ve-opacity" type="number" min="0" max="100"
                           value={parseColorAlpha(el.style?.color ?? '#000000').opacity}
                           on:input={e => setStyleProp(i,"color",
                             buildColorAlpha(parseColorAlpha(el.style?.color ?? '#000000').hex, e.target.value))}>
                    <span class="ve-opacity-unit">%</span>
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Font size</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="1rem"
                             value={el.style?.["font-size"] ?? ""}
                             on:input={e => setStyleProp(i,"font-size",e.target.value || undefined)}>
                    </div>
                    <label class="ve-prop-label">Font</label>
                    <select class="ve-select"
                            value={fontOpts.some(f => f.v === (el.style?.["font-family"] ?? "")) ? (el.style?.["font-family"] ?? "") : "__custom__"}
                            on:change={e => setStyleProp(i,"font-family", e.target.value === "__custom__" ? undefined : (e.target.value || undefined))}>
                      {#each fontOpts as f}<option value={f.v}>{f.label}</option>{/each}
                      {#if el.style?.["font-family"] && !fontOpts.some(f => f.v === el.style?.["font-family"])}
                        <option value="__custom__">{el.style["font-family"]}</option>
                      {/if}
                    </select>
                    <label class="ve-prop-label">Style</label>
                    <div class="ve-align-row">
                      <button class="ve-align-btn"
                              class:ve-align-active={el.style?.["font-weight"] === "bold"}
                              on:click={() => setStyleProp(i,"font-weight",
                                el.style?.["font-weight"] === "bold" ? "" : "bold")}><b>B</b></button>
                      <button class="ve-align-btn"
                              class:ve-align-active={el.style?.["font-style"] === "italic"}
                              on:click={() => setStyleProp(i,"font-style",
                                el.style?.["font-style"] === "italic" ? "" : "italic")}><i>I</i></button>
                      {#each textAlignOpts as opt}
                        <button class="ve-align-btn"
                                class:ve-align-active={el.style?.["text-align"] === opt.v}
                                on:click={() => setStyleProp(i,"text-align",
                                  el.style?.["text-align"] === opt.v ? "" : opt.v)}
                                title={opt.label}>{opt.label}</button>
                      {/each}
                    </div>
                    <label class="ve-prop-label">Binds to variable <span class="ve-hint">(overrides content)</span></label>
                    <select class="ve-select" value={el.bindVar ?? ""}
                            on:change={e => setProp(i,"bindVar",e.target.value || undefined)}>
                      <option value="">— static content —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  <!-- ── Chart ── -->
                  {:else if el.type === "vsm-chart"}
                    <label class="ve-prop-label">Chart type</label>
                    <select class="ve-select" value={el.chartType ?? "bar"}
                            on:change={e => setProp(i,"chartType",e.target.value)}>
                      <option value="bar">Bar</option>
                      <option value="line">Line</option>
                    </select>
                    <label class="ve-prop-label">Data variable <span class="ve-hint">(holds JSON dataset)</span></label>
                    <select class="ve-select" value={el.dataVar ?? ""}
                            on:change={e => setProp(i,"dataVar",e.target.value || undefined)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <label class="ve-prop-label">Dataset label</label>
                    <input class="ve-input" type="text" placeholder="My data"
                           value={el.label ?? ""}
                           on:input={e => setProp(i,"label",e.target.value || undefined)}>
                    <label class="ve-prop-label">Color</label>
                    <input class="ve-color" type="color"
                           value={parseColorAlpha(el.color ?? '#5b8edc').hex}
                           on:input={e => setProp(i,"color",e.target.value)}>
                    {#if (el.chartType ?? "bar") === "line"}
                    <div class="ve-row" style="align-items:center;gap:.5rem;margin-top:.25rem">
                      <label class="ve-prop-label" style="min-width:0">Fill area</label>
                      <input type="checkbox" checked={!!el.fill}
                             on:change={e => setProp(i,"fill",e.target.checked || undefined)}>
                    </div>
                    {/if}
                    <div class="ve-row" style="gap:.5rem;margin-top:.25rem">
                      <label class="ve-prop-label">Width</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="100%"
                             value={el.width ?? ""}
                             on:input={e => setProp(i,"width",e.target.value || undefined)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="300px"
                             value={el.height ?? "300px"}
                             on:input={e => setProp(i,"height",e.target.value || undefined)}>
                    </div>
                    <div class="ve-media-hint">
                      Variable must hold JSON: <code>{"{"}"labels":["A","B"],"data":[10,25]{"}"}</code><br>
                      Multi-series: <code>{"{"}"labels":[…],"datasets":[{"{"}"label":"S1","data":[…],"color":"#f00"{"}"}]{"}"}  </code>
                    </div>

                  <!-- ── Feed ── -->
                  {:else if el.type === "vsm-feed"}
                    <label class="ve-prop-label">Data variable <span class="ve-hint">(JSON array of messages)</span></label>
                    <select class="ve-select" value={el.dataVar ?? ""}
                            on:change={e => setProp(i,"dataVar",e.target.value || undefined)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <label class="ve-prop-label">Height</label>
                    <input class="ve-input" type="text" placeholder="400px"
                           value={el.height ?? "400px"}
                           on:input={e => setProp(i,"height",e.target.value || undefined)}>
                    <label class="ve-prop-label">Agent label</label>
                    <input class="ve-input" type="text" placeholder="Agent"
                           value={el.agentLabel ?? "Agent"}
                           on:input={e => setProp(i,"agentLabel",e.target.value || undefined)}>
                    <label class="ve-prop-label">User label</label>
                    <input class="ve-input" type="text" placeholder="You"
                           value={el.userLabel ?? "You"}
                           on:input={e => setProp(i,"userLabel",e.target.value || undefined)}>
                    <div class="ve-row" style="gap:.5rem;margin-top:.1rem">
                      <label class="ve-prop-label">Agent color</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(el.agentColor ?? '#e8f4fd').hex}
                             on:input={e => setProp(i,"agentColor",e.target.value)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">User color</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(el.userColor ?? '#eafbe8').hex}
                             on:input={e => setProp(i,"userColor",e.target.value)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">System color</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(el.systemColor ?? '#f5f5f5').hex}
                             on:input={e => setProp(i,"systemColor",e.target.value)}>
                    </div>
                    <div class="ve-row" style="align-items:center;gap:.5rem;margin-top:.25rem">
                      <label class="ve-prop-label" style="min-width:0">Show timestamps</label>
                      <input type="checkbox" checked={!!el.showTimestamps}
                             on:change={e => setProp(i,"showTimestamps",e.target.checked || undefined)}>
                    </div>
                    <div class="ve-media-hint">
                      Use <code>appendMessage(var='…', role='agent', text='…')</code> PlayAction to add messages at runtime.<br>
                      Roles: <code>agent</code> (left) · <code>user</code> (right) · <code>system</code> (center, italic)
                    </div>

                  <!-- ── Panel ── -->
                  {:else if el.type === "vsm-panel"}
                    <div class="ve-row">
                      <label class="ve-label">Background</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(el.background ?? '#f5f5f5').hex}
                             on:input={e => setProp(i,"background",
                               buildColorAlpha(e.target.value, parseColorAlpha(el.background ?? '#f5f5f5').opacity))}>
                      <input class="ve-opacity" type="number" min="0" max="100"
                             value={parseColorAlpha(el.background ?? '#f5f5f5').opacity}
                             on:input={e => setProp(i,"background",
                               buildColorAlpha(parseColorAlpha(el.background ?? '#f5f5f5').hex, e.target.value))}>
                      <span class="ve-opacity-unit">%</span>
                      <label class="ve-label" style="margin-left:.5rem">Padding</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="1rem"
                             value={el.padding ?? ""}
                             on:input={e => setProp(i,"padding",e.target.value)}>
                    </div>
                    <div class="ve-row">
                      <label class="ve-label">Layout</label>
                      <select class="ve-select" value={el.layout ?? "flex-column"}
                              on:change={e => setProp(i,"layout",e.target.value)}>
                        <option value="flex-column">Column</option>
                        <option value="flex-row">Row</option>
                      </select>
                    </div>
                    {@const panelIsRow = (el.layout ?? 'flex-column') === 'flex-row'}
                    <label class="ve-prop-label">Horizontal</label>
                    <div class="ve-align-row">
                      {#each (panelIsRow ? justifyOpts : alignItemsOpts) as opt}
                        <button class="ve-align-btn"
                                class:ve-align-active={el.alignItems === opt.v}
                                on:click={() => setProp(i,"alignItems",
                                  el.alignItems === opt.v ? undefined : opt.v)}
                                title={opt.label}>{opt.label}</button>
                      {/each}
                    </div>
                    <label class="ve-prop-label">Vertical</label>
                    <div class="ve-align-row">
                      {#each (panelIsRow ? alignItemsOpts : justifyOpts) as opt}
                        <button class="ve-align-btn"
                                class:ve-align-active={el.justifyContent === opt.v}
                                on:click={() => setProp(i,"justifyContent",
                                  el.justifyContent === opt.v ? undefined : opt.v)}
                                title={opt.label}>{opt.label}</button>
                      {/each}
                    </div>
                    <label class="ve-prop-label">Align in screen</label>
                    <div class="ve-align-row">
                      {#each alignItemsOpts as opt}
                        <button class="ve-align-btn"
                                class:ve-align-active={el.style?.["align-self"] === opt.v}
                                on:click={() => setStyleProp(i,"align-self",
                                  el.style?.["align-self"] === opt.v ? "" : opt.v)}
                                title={opt.label}>{opt.label}</button>
                      {/each}
                    </div>
                    <div class="ve-row" style="align-items:center;gap:.5rem;margin-top:.25rem">
                      <label class="ve-prop-label" style="min-width:0">Grow to fill</label>
                      <input type="checkbox" checked={el.flexGrow ?? false}
                             on:change={e => setProp(i,"flexGrow", e.target.checked || undefined)}>
                      <span class="ve-hint">claims remaining space in layout</span>
                    </div>

                    <!-- Panel children -->
                    <div class="ve-children-bar">
                      <span class="ve-prop-label">Children</span>
                      <div class="ve-add-group">
                        <button class="ve-add-btn" on:click={() => addChild(i,{type:"sl-text",content:"Text"})}>+Text</button>
                        <button class="ve-add-btn" on:click={() => addChild(i,{type:"sl-button",label:"Button",sendsVar:"",sendsValue:""})}>+Button</button>
                        <button class="ve-add-btn" on:click={() => addChild(i,{type:"sl-range",label:"Slider",min:0,max:100,step:1})}>+Slider</button>
                        <button class="ve-add-btn" on:click={() => addChild(i,{type:"sl-input",label:"Input",bindVar:""})}>+Input</button>
                        <button class="ve-add-btn" on:click={() => addChild(i,{type:"sl-select",label:"Select",options:["Option 1","Option 2"],bindVar:""})}>+Select</button>
                        <button class="ve-add-btn" on:click={() => addChild(i,{type:"sl-checkbox",label:"Checkbox",bindVar:""})}>+Check</button>
                        <button class="ve-add-btn" on:click={() => addChild(i,{type:"vsm-filler",flexGrow:true})}>+Filler</button>
                        <button class="ve-add-btn" on:click={() => addChild(i,{type:"vsm-image",src:"",alt:""})}>+Image</button>
                        <button class="ve-add-btn" on:click={() => addChild(i,{type:"vsm-video",src:"",controls:true})}>+Video</button>
                        <button class="ve-add-btn" on:click={() => addChild(i,{type:"vsm-audio",src:"",controls:true})}>+Audio</button>
                        <button class="ve-add-btn" on:click={() => addChild(i,{type:"vsm-embed",src:"",width:"100%",height:"315px"})}>+Embed</button>
                        <button class="ve-add-btn" on:click={() => addChild(i,{type:"vsm-bubble",content:"Hello!",tail:"bottom",background:"#e8f4fd"})}>+Bubble</button>
                        <button class="ve-add-btn" on:click={() => addChild(i,{type:"vsm-chart",chartType:"bar",dataVar:"",label:"",color:"#5b8edc",height:"300px"})}>+Chart</button>
                        <button class="ve-add-btn ve-add-btn-feed" on:click={() => addChild(i,{type:"vsm-feed",dataVar:"",height:"400px",agentColor:"#e8f4fd",userColor:"#eafbe8",systemColor:"#f5f5f5",agentLabel:"Agent",userLabel:"You"})}>+Feed</button>
                      </div>
                    </div>

                    {#if (el.children ?? []).length === 0}
                      <div class="ve-empty">No children yet.</div>
                    {/if}

                    {#each (el.children ?? []) as child, ci}
                      {@const cexp = expandedChild?.pi === i && expandedChild?.ci === ci}
                      <div class="ve-child-card" class:ve-child-expanded={cexp}>
                        <div class="ve-child-header" role="button" tabindex="0"
                             on:click={() => expandedChild = cexp ? null : {pi:i,ci}}
                             on:keydown={e => e.key==="Enter" && (expandedChild = cexp ? null : {pi:i,ci})}>
                          <div class="ve-card-arrows">
                            <button class="ve-arrow" disabled={ci===0}
                                    on:click|stopPropagation={() => moveChild(i,ci,-1)}>▲</button>
                            <button class="ve-arrow" disabled={ci===(el.children?.length??0)-1}
                                    on:click|stopPropagation={() => moveChild(i,ci,1)}>▼</button>
                          </div>
                          <span class="ve-type-badge ve-type-badge-sm">{typeLabel(child.type)}</span>
                          <span class="ve-card-summary">{elementSummary(child)}</span>
                          <span class="ve-expand-icon">{cexp ? "▾" : "▸"}</span>
                          <button class="ve-delete" on:click|stopPropagation={() => deleteChild(i,ci)}>×</button>
                        </div>

                        {#if cexp}
                        <div class="ve-child-props">
                          <label class="ve-prop-label">Align in panel</label>
                          <div class="ve-align-row">
                            {#each alignItemsOpts as opt}
                              <button class="ve-align-btn"
                                      class:ve-align-active={child.style?.["align-self"] === opt.v}
                                      on:click={() => setChildStyleProp(i,ci,"align-self",
                                        child.style?.["align-self"] === opt.v ? "" : opt.v)}
                                      title={opt.label}>{opt.label}</button>
                            {/each}
                          </div>

                          {#if child.type === "sl-text" || child.type === "wa-text"}
                            <label class="ve-prop-label">Content</label>
                            <textarea class="ve-textarea" rows="2"
                                      value={child.content ?? ""}
                                      on:input={e => setChildProp(i,ci,"content",e.target.value)}></textarea>
                            <label class="ve-prop-label">Text color</label>
                            <input class="ve-color" type="color"
                                   value={parseColorAlpha(child.style?.color ?? '#000000').hex}
                                   on:input={e => setChildStyleProp(i,ci,"color",
                                     buildColorAlpha(e.target.value, parseColorAlpha(child.style?.color ?? '#000000').opacity))}>
                            <input class="ve-opacity" type="number" min="0" max="100"
                                   value={parseColorAlpha(child.style?.color ?? '#000000').opacity}
                                   on:input={e => setChildStyleProp(i,ci,"color",
                                     buildColorAlpha(parseColorAlpha(child.style?.color ?? '#000000').hex, e.target.value))}>
                            <span class="ve-opacity-unit">%</span>
                            <div class="ve-row" style="gap:.5rem;margin-top:.25rem">
                              <label class="ve-prop-label">Font size</label>
                              <input class="ve-input ve-input-short" type="text" placeholder="1rem"
                                     value={child.style?.["font-size"] ?? ""}
                                     on:input={e => setChildStyleProp(i,ci,"font-size",e.target.value || undefined)}>
                            </div>
                            <label class="ve-prop-label">Font</label>
                            <select class="ve-select"
                                    value={fontOpts.some(f => f.v === (child.style?.["font-family"] ?? "")) ? (child.style?.["font-family"] ?? "") : "__custom__"}
                                    on:change={e => setChildStyleProp(i,ci,"font-family", e.target.value === "__custom__" ? undefined : (e.target.value || undefined))}>
                              {#each fontOpts as f}<option value={f.v}>{f.label}</option>{/each}
                              {#if child.style?.["font-family"] && !fontOpts.some(f => f.v === child.style?.["font-family"])}
                                <option value="__custom__">{child.style["font-family"]}</option>
                              {/if}
                            </select>
                            <label class="ve-prop-label">Style</label>
                            <div class="ve-align-row">
                              <button class="ve-align-btn"
                                      class:ve-align-active={child.style?.["font-weight"] === "bold"}
                                      on:click={() => setChildStyleProp(i,ci,"font-weight",
                                        child.style?.["font-weight"] === "bold" ? "" : "bold")}><b>B</b></button>
                              <button class="ve-align-btn"
                                      class:ve-align-active={child.style?.["font-style"] === "italic"}
                                      on:click={() => setChildStyleProp(i,ci,"font-style",
                                        child.style?.["font-style"] === "italic" ? "" : "italic")}><i>I</i></button>
                              {#each textAlignOpts as opt}
                                <button class="ve-align-btn"
                                        class:ve-align-active={child.style?.["text-align"] === opt.v}
                                        on:click={() => setChildStyleProp(i,ci,"text-align",
                                          child.style?.["text-align"] === opt.v ? "" : opt.v)}
                                        title={opt.label}>{opt.label}</button>
                              {/each}
                            </div>

                          {:else if child.type === "sl-button" || child.type === "wa-button"}
                            <label class="ve-prop-label">Label</label>
                            <input class="ve-input" type="text" value={child.label ?? ""}
                                   on:input={e => setChildProp(i,ci,"label",e.target.value)}>
                            <label class="ve-prop-label">Variant</label>
                            <select class="ve-select" value={child.variant ?? "default"}
                                    on:change={e => setChildProp(i,ci,"variant",e.target.value)}>
                              <option value="default">Default</option>
                              <option value="primary">Primary</option>
                              <option value="success">Success</option>
                              <option value="warning">Warning</option>
                              <option value="danger">Danger</option>
                            </select>
                            <label class="ve-prop-label">Sends to variable</label>
                            <select class="ve-select" value={child.sendsVar ?? ""}
                                    on:change={e => setChildProp(i,ci,"sendsVar",e.target.value)}>
                              <option value="">— none —</option>
                              {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                            </select>
                            <label class="ve-prop-label">Value to send</label>
                            <input class="ve-input" type="text" value={child.sendsValue ?? ""}
                                   on:input={e => setChildProp(i,ci,"sendsValue",e.target.value)}>

                          {:else if child.type === "sl-range" || child.type === "wa-range"}
                            <label class="ve-prop-label">Label</label>
                            <input class="ve-input" type="text" value={child.label ?? ""}
                                   on:input={e => setChildProp(i,ci,"label",e.target.value)}>
                            <div class="ve-row-trio">
                              <div><label class="ve-prop-label">Min</label>
                                <input class="ve-input" type="number" value={child.min ?? 0}
                                       on:input={e => setChildProp(i,ci,"min",+e.target.value)}></div>
                              <div><label class="ve-prop-label">Max</label>
                                <input class="ve-input" type="number" value={child.max ?? 100}
                                       on:input={e => setChildProp(i,ci,"max",+e.target.value)}></div>
                              <div><label class="ve-prop-label">Step</label>
                                <input class="ve-input" type="number" value={child.step ?? 1}
                                       on:input={e => setChildProp(i,ci,"step",+e.target.value)}></div>
                            </div>
                            <label class="ve-prop-label">Binds to variable</label>
                            <select class="ve-select" value={child.bindVar ?? ""}
                                    on:change={e => setChildProp(i,ci,"bindVar",e.target.value)}>
                              <option value="">— none —</option>
                              {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                            </select>

                          {:else if child.type === "sl-input" || child.type === "wa-input"}
                            <label class="ve-prop-label">Label</label>
                            <input class="ve-input" type="text" value={child.label ?? ""}
                                   on:input={e => setChildProp(i,ci,"label",e.target.value)}>
                            <label class="ve-prop-label">Placeholder</label>
                            <input class="ve-input" type="text" value={child.placeholder ?? ""}
                                   on:input={e => setChildProp(i,ci,"placeholder",e.target.value)}>
                            <label class="ve-prop-label">Binds to variable</label>
                            <select class="ve-select" value={child.bindVar ?? ""}
                                    on:change={e => setChildProp(i,ci,"bindVar",e.target.value)}>
                              <option value="">— none —</option>
                              {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                            </select>

                          {:else if child.type === "sl-select" || child.type === "wa-select"}
                            <label class="ve-prop-label">Label</label>
                            <input class="ve-input" type="text" value={child.label ?? ""}
                                   on:input={e => setChildProp(i,ci,"label",e.target.value)}>
                            <label class="ve-prop-label">Options <span class="ve-hint">(one per line)</span></label>
                            <textarea class="ve-textarea" rows="3"
                                      value={optionsToText(child.options)}
                                      on:change={e => setChildProp(i,ci,"options",textToOptions(e.target.value))}></textarea>
                            <label class="ve-prop-label">Binds to variable</label>
                            <select class="ve-select" value={child.bindVar ?? ""}
                                    on:change={e => setChildProp(i,ci,"bindVar",e.target.value)}>
                              <option value="">— none —</option>
                              {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                            </select>

                          {:else if child.type === "sl-checkbox" || child.type === "wa-checkbox"}
                            <label class="ve-prop-label">Label</label>
                            <input class="ve-input" type="text" value={child.label ?? ""}
                                   on:input={e => setChildProp(i,ci,"label",e.target.value)}>
                            <label class="ve-prop-label">Binds to variable</label>
                            <select class="ve-select" value={child.bindVar ?? ""}
                                    on:change={e => setChildProp(i,ci,"bindVar",e.target.value)}>
                              <option value="">— none —</option>
                              {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                            </select>

                          {:else if child.type === "vsm-filler"}
                            <div class="ve-row" style="align-items:center;gap:.5rem">
                              <label class="ve-prop-label" style="min-width:0">Flex grow</label>
                              <input type="checkbox" checked={child.flexGrow ?? false}
                                     on:change={e => setChildProp(i,ci,"flexGrow", e.target.checked)}>
                              <span class="ve-hint" style="margin-left:.25rem">fills remaining space</span>
                            </div>
                            {#if !child.flexGrow}
                            <div class="ve-row" style="gap:.5rem">
                              <label class="ve-prop-label">Width</label>
                              <input class="ve-input ve-input-short" type="text" placeholder="e.g. 100%"
                                     value={child.width ?? ""}
                                     on:input={e => setChildProp(i,ci,"width", e.target.value || undefined)}>
                              <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                              <input class="ve-input ve-input-short" type="text" placeholder="e.g. 2rem"
                                     value={child.height ?? ""}
                                     on:input={e => setChildProp(i,ci,"height", e.target.value || undefined)}>
                            </div>
                            {/if}

                          {:else if child.type === "vsm-bubble"}
                            <label class="ve-prop-label">Content</label>
                            <textarea class="ve-textarea" rows="2"
                                      value={child.content ?? ""}
                                      on:input={e => setChildProp(i,ci,"content",e.target.value)}></textarea>
                            <label class="ve-prop-label">Speaker name</label>
                            <input class="ve-input" type="text" placeholder="Agent, User, …"
                                   value={child.speaker ?? ""}
                                   on:input={e => setChildProp(i,ci,"speaker",e.target.value || undefined)}>
                            <label class="ve-prop-label">Tail direction <span class="ve-hint">(left/right follows alignment)</span></label>
                            <select class="ve-select" value={child.tail ?? "bottom"}
                                    on:change={e => setChildProp(i,ci,"tail",e.target.value)}>
                              <option value="bottom">Bottom</option>
                              <option value="top">Top</option>
                              <option value="">None</option>
                            </select>
                            <label class="ve-prop-label">Background</label>
                            <input class="ve-color" type="color"
                                   value={parseColorAlpha(child.background ?? '#e8f4fd').hex}
                                   on:input={e => setChildProp(i,ci,"background",
                                     buildColorAlpha(e.target.value, parseColorAlpha(child.background ?? '#e8f4fd').opacity))}>
                            <input class="ve-opacity" type="number" min="0" max="100"
                                   value={parseColorAlpha(child.background ?? '#e8f4fd').opacity}
                                   on:input={e => setChildProp(i,ci,"background",
                                     buildColorAlpha(parseColorAlpha(child.background ?? '#e8f4fd').hex, e.target.value))}>
                            <span class="ve-opacity-unit">%</span>
                            <label class="ve-prop-label">Text color</label>
                            <input class="ve-color" type="color"
                                   value={parseColorAlpha(child.style?.color ?? '#000000').hex}
                                   on:input={e => setChildStyleProp(i,ci,"color",
                                     buildColorAlpha(e.target.value, parseColorAlpha(child.style?.color ?? '#000000').opacity))}>
                            <input class="ve-opacity" type="number" min="0" max="100"
                                   value={parseColorAlpha(child.style?.color ?? '#000000').opacity}
                                   on:input={e => setChildStyleProp(i,ci,"color",
                                     buildColorAlpha(parseColorAlpha(child.style?.color ?? '#000000').hex, e.target.value))}>
                            <span class="ve-opacity-unit">%</span>
                            <div class="ve-row" style="gap:.5rem">
                              <label class="ve-prop-label">Font size</label>
                              <input class="ve-input ve-input-short" type="text" placeholder="1rem"
                                     value={child.style?.["font-size"] ?? ""}
                                     on:input={e => setChildStyleProp(i,ci,"font-size",e.target.value || undefined)}>
                            </div>
                            <label class="ve-prop-label">Font</label>
                            <select class="ve-select"
                                    value={fontOpts.some(f => f.v === (child.style?.["font-family"] ?? "")) ? (child.style?.["font-family"] ?? "") : "__custom__"}
                                    on:change={e => setChildStyleProp(i,ci,"font-family", e.target.value === "__custom__" ? undefined : (e.target.value || undefined))}>
                              {#each fontOpts as f}<option value={f.v}>{f.label}</option>{/each}
                              {#if child.style?.["font-family"] && !fontOpts.some(f => f.v === child.style?.["font-family"])}
                                <option value="__custom__">{child.style["font-family"]}</option>
                              {/if}
                            </select>
                            <label class="ve-prop-label">Style</label>
                            <div class="ve-align-row">
                              <button class="ve-align-btn"
                                      class:ve-align-active={child.style?.["font-weight"] === "bold"}
                                      on:click={() => setChildStyleProp(i,ci,"font-weight",
                                        child.style?.["font-weight"] === "bold" ? "" : "bold")}><b>B</b></button>
                              <button class="ve-align-btn"
                                      class:ve-align-active={child.style?.["font-style"] === "italic"}
                                      on:click={() => setChildStyleProp(i,ci,"font-style",
                                        child.style?.["font-style"] === "italic" ? "" : "italic")}><i>I</i></button>
                              {#each textAlignOpts as opt}
                                <button class="ve-align-btn"
                                        class:ve-align-active={child.style?.["text-align"] === opt.v}
                                        on:click={() => setChildStyleProp(i,ci,"text-align",
                                          child.style?.["text-align"] === opt.v ? "" : opt.v)}
                                        title={opt.label}>{opt.label}</button>
                              {/each}
                            </div>
                            <label class="ve-prop-label">Binds to variable</label>
                            <select class="ve-select" value={child.bindVar ?? ""}
                                    on:change={e => setChildProp(i,ci,"bindVar",e.target.value || undefined)}>
                              <option value="">— static content —</option>
                              {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                            </select>

                          {:else if child.type === "vsm-chart"}
                            <label class="ve-prop-label">Chart type</label>
                            <select class="ve-select" value={child.chartType ?? "bar"}
                                    on:change={e => setChildProp(i,ci,"chartType",e.target.value)}>
                              <option value="bar">Bar</option>
                              <option value="line">Line</option>
                            </select>
                            <label class="ve-prop-label">Data variable</label>
                            <select class="ve-select" value={child.dataVar ?? ""}
                                    on:change={e => setChildProp(i,ci,"dataVar",e.target.value || undefined)}>
                              <option value="">— none —</option>
                              {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                            </select>
                            <label class="ve-prop-label">Dataset label</label>
                            <input class="ve-input" type="text" placeholder="My data"
                                   value={child.label ?? ""}
                                   on:input={e => setChildProp(i,ci,"label",e.target.value || undefined)}>
                            <label class="ve-prop-label">Color</label>
                            <input class="ve-color" type="color"
                                   value={parseColorAlpha(child.color ?? '#5b8edc').hex}
                                   on:input={e => setChildProp(i,ci,"color",e.target.value)}>
                            <div class="ve-row" style="gap:.5rem;margin-top:.25rem">
                              <label class="ve-prop-label">Height</label>
                              <input class="ve-input ve-input-short" type="text" placeholder="300px"
                                     value={child.height ?? "300px"}
                                     on:input={e => setChildProp(i,ci,"height",e.target.value || undefined)}>
                            </div>

                          {:else if child.type === "vsm-feed"}
                            <label class="ve-prop-label">Data variable</label>
                            <select class="ve-select" value={child.dataVar ?? ""}
                                    on:change={e => setChildProp(i,ci,"dataVar",e.target.value || undefined)}>
                              <option value="">— none —</option>
                              {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                            </select>
                            <label class="ve-prop-label">Height</label>
                            <input class="ve-input" type="text" placeholder="400px"
                                   value={child.height ?? "400px"}
                                   on:input={e => setChildProp(i,ci,"height",e.target.value || undefined)}>
                            <label class="ve-prop-label">Agent label</label>
                            <input class="ve-input" type="text" placeholder="Agent"
                                   value={child.agentLabel ?? "Agent"}
                                   on:input={e => setChildProp(i,ci,"agentLabel",e.target.value || undefined)}>
                            <label class="ve-prop-label">User label</label>
                            <input class="ve-input" type="text" placeholder="You"
                                   value={child.userLabel ?? "You"}
                                   on:input={e => setChildProp(i,ci,"userLabel",e.target.value || undefined)}>
                            <div class="ve-row" style="gap:.5rem;margin-top:.1rem">
                              <label class="ve-prop-label">Agent color</label>
                              <input class="ve-color" type="color"
                                     value={parseColorAlpha(child.agentColor ?? '#e8f4fd').hex}
                                     on:input={e => setChildProp(i,ci,"agentColor",e.target.value)}>
                              <label class="ve-prop-label" style="margin-left:.5rem">User color</label>
                              <input class="ve-color" type="color"
                                     value={parseColorAlpha(child.userColor ?? '#eafbe8').hex}
                                     on:input={e => setChildProp(i,ci,"userColor",e.target.value)}>
                            </div>

                          {:else}
                            <p class="ve-unknown">Type <code>{child.type}</code> — edit in JSON tab.</p>
                          {/if}
                        </div>
                        {/if}
                      </div>
                    {/each}

                  {:else}
                    <p class="ve-unknown">Type <code>{el.type}</code> — edit in JSON tab.</p>
                  {/if}

                </div>
                {/if}
              </div>
            {/each}
          </div>

        {:else}
          <div class="ve-empty ve-empty-full">No screen selected.</div>
        {/if}
      </div>
      {/if}

      <!-- CodeMirror (always mounted, hidden in visual mode) -->
      <div class="se-cm-wrap" class:se-cm-hidden={editorMode === "visual"}
           bind:this={editorContainer}></div>

      <!-- Variable bar -->
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
        {#if screenNames.length > 0}
          <div class="se-screen-selector">
            <span class="se-screen-label">Screen</span>
            <select class="se-screen-select" bind:value={selectedScreen}>
              {#each screenNames as name}<option value={name}>{name}</option>{/each}
            </select>
          </div>
        {/if}
        <button class="se-btn se-btn-sm" on:click={() => previewKey++}
                title="Reload preview">↺ Reload</button>
      </div>
      {#if loading}
        <div class="se-preview-hint">Loading…</div>
      {:else}
        <iframe class="se-preview-frame" src={previewUrl} title="Screen preview"
                bind:this={previewIframe} on:load={onPreviewLoad}></iframe>
      {/if}
    </div>
  </div>
</div>

<style>
  .se-overlay {
    position: fixed; inset: 0; z-index: 600;
    display: flex; flex-direction: column;
    background: var(--panel, #ffffff); color: var(--ink, #1f2328);
  }
  .se-header {
    display: flex; align-items: center; justify-content: space-between;
    padding: 0.45rem 0.9rem; border-bottom: 1px solid var(--stroke, #e3ddd4);
    flex-shrink: 0; gap: 0.75rem; background: var(--panel, #ffffff);
  }
  .se-title { display: flex; align-items: baseline; gap: 0.4rem; font-size: 1rem; }
  .se-title-main   { font-weight: 700; }
  .se-title-sep    { opacity: 0.35; }
  .se-title-plugin { color: var(--accent, #5b8edc); font-weight: 500; }
  .se-header-actions { display: flex; align-items: center; gap: 0.5rem; }

  .se-badge { font-size: 0.78rem; padding: 0.15rem 0.55rem; border-radius: 99px; font-weight: 500; }
  .se-badge-error { background: #fde8e5; color: var(--danger, #e26d5a); }
  .se-badge-ok    { background: #e5f5ec; color: #2a7a48; }
  .se-badge-warn  { background: #fef5e0; color: #8a6300; }

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

  .se-body { display: flex; flex: 1; min-height: 0; overflow: hidden; }

  .se-editor-col {
    display: flex; flex-direction: column;
    flex: 1; min-width: 0; position: relative;
    border-right: 1px solid var(--stroke, #e3ddd4);
  }
  .se-editor-overlay {
    position: absolute; inset: 0; z-index: 2;
    display: flex; align-items: center; justify-content: center; gap: 0.75rem;
    background: var(--panel, #ffffff); font-size: 0.9rem;
  }
  .se-loading-text { opacity: 0.45; }
  .se-message-error { color: var(--danger, #e26d5a); }

  .se-tab-bar {
    display: flex; flex-shrink: 0;
    border-bottom: 1px solid var(--stroke, #e3ddd4);
    background: var(--panel, #ffffff);
  }
  .se-tab {
    padding: 0.35rem 1rem; border: none; background: none;
    font-size: 0.85rem; font-family: inherit; cursor: pointer;
    color: var(--ink, #1f2328); opacity: 0.5;
    border-bottom: 2px solid transparent; margin-bottom: -1px;
  }
  .se-tab:hover { opacity: 0.85; }
  .se-tab-active { opacity: 1; font-weight: 600; border-bottom-color: var(--accent, #5b8edc); }

  .se-cm-wrap { flex: 1; min-height: 0; overflow: hidden; }
  .se-cm-hidden { display: none; }

  /* ── Visual editor ─────────────────────────────────────────────────────── */
  .ve-root {
    flex: 1; min-height: 0; overflow-y: auto;
    display: flex; flex-direction: column;
    background: var(--panel-soft, #f5f7fb);
  }
  .ve-section {
    background: var(--panel, #ffffff);
    border-bottom: 1px solid var(--stroke, #e3ddd4);
    padding: 0.6rem 0.8rem;
  }
  .ve-section-grow { flex: 1; padding: 0; }
  .ve-section-title {
    font-size: 0.72rem; text-transform: uppercase; letter-spacing: 0.06em;
    opacity: 0.45; font-weight: 600; margin-bottom: 0.45rem;
  }
  .ve-row { display: flex; align-items: center; gap: 0.5rem; margin-bottom: 0.35rem; flex-wrap: wrap; }
  .ve-row:last-child { margin-bottom: 0; }
  .ve-label { font-size: 0.82rem; opacity: 0.7; min-width: 72px; flex-shrink: 0; }

  .ve-color {
    width: 2.2rem; height: 1.7rem; padding: 0.1rem; border-radius: 4px;
    border: 1px solid var(--stroke, #e3ddd4); cursor: pointer; background: none;
  }
  .ve-opacity {
    width: 3.4rem; padding: 0.2rem 0.3rem; border-radius: 4px;
    border: 1px solid var(--stroke, #e3ddd4); background: var(--panel-soft, #f5f7fb);
    color: var(--ink, #1f2328); font-size: 0.82rem; text-align: right;
  }
  .ve-opacity-unit { font-size: 0.78rem; opacity: 0.6; }
  .ve-media-hint {
    font-size: 0.78rem; opacity: 0.7; margin-bottom: .4rem;
    padding: .3rem .5rem; background: var(--hover-bg, #f0f0f0);
    border-radius: 5px; line-height: 1.4;
  }
  .ve-media-hint code { font-size: 0.75rem; opacity: 0.9; }
  .ve-select {
    flex: 1; padding: 0.22rem 0.4rem;
    border: 1px solid var(--stroke, #e3ddd4); border-radius: 5px;
    background: var(--panel-soft, #f5f7fb); color: var(--ink, #1f2328);
    font-size: 0.85rem; font-family: inherit;
  }

  /* Alignment toggle buttons */
  .ve-align-row { display: flex; gap: 0.25rem; flex-wrap: wrap; }
  .ve-align-btn {
    padding: 0.18rem 0.5rem; border-radius: 5px;
    border: 1px solid var(--stroke, #e3ddd4);
    background: var(--panel-soft, #f5f7fb); color: var(--ink, #1f2328);
    font-size: 0.75rem; font-family: inherit; cursor: pointer; white-space: nowrap;
  }
  .ve-align-btn:hover { background: var(--accent-soft, #d6e2f6); }
  .ve-align-active {
    background: var(--accent, #5b8edc) !important;
    color: #fff !important; border-color: transparent;
  }

  /* Elements list */
  .ve-elements-header {
    display: flex; align-items: center; justify-content: space-between;
    flex-wrap: wrap; gap: 0.4rem;
    padding: 0.5rem 0.8rem;
    background: var(--panel, #ffffff);
    border-bottom: 1px solid var(--stroke, #e3ddd4);
    flex-shrink: 0;
  }
  .ve-add-group { display: flex; flex-wrap: wrap; gap: 0.25rem; }
  .ve-add-btn {
    padding: 0.2rem 0.5rem;
    border: 1px solid var(--stroke, #e3ddd4); border-radius: 5px;
    background: var(--panel-soft, #f5f7fb); color: var(--ink, #1f2328);
    font-size: 0.78rem; font-family: inherit; cursor: pointer;
  }
  .ve-add-btn:hover { background: var(--accent-soft, #d6e2f6); }
  .ve-add-btn-panel {
    background: var(--accent-soft, #d6e2f6);
    border-color: var(--accent, #5b8edc); color: var(--accent, #5b8edc); font-weight: 600;
  }
  .ve-add-btn-feed {
    background: #eafbe8; border-color: #5aaa6a; color: #2e6b38; font-weight: 600;
  }

  /* Element cards */
  .ve-card { background: var(--panel, #ffffff); border-bottom: 1px solid var(--stroke, #e3ddd4); }
  .ve-card-header {
    display: flex; align-items: center; gap: 0.4rem;
    padding: 0.42rem 0.6rem; cursor: pointer; user-select: none;
  }
  .ve-card-header:hover { background: var(--accent-soft, #d6e2f6); }

  .ve-card-arrows { display: flex; flex-direction: column; gap: 0; }
  .ve-arrow {
    padding: 1px 3px; border: none; background: none; cursor: pointer;
    font-size: 0.75rem; line-height: 1; color: var(--ink, #1f2328);
    opacity: 0.45; border-radius: 3px;
  }
  .ve-arrow:hover:not(:disabled) { opacity: 1; background: var(--hover-bg, #e8e8e8); }
  .ve-arrow:disabled { opacity: 0.12; cursor: default; }

  .ve-type-badge {
    width: 1.4rem; height: 1.4rem; border-radius: 4px; flex-shrink: 0;
    background: var(--accent-soft, #d6e2f6); color: var(--accent, #5b8edc);
    font-size: 0.72rem; font-weight: 700;
    display: flex; align-items: center; justify-content: center;
  }
  .ve-type-badge-sm { width: 1.2rem; height: 1.2rem; font-size: 0.65rem; }

  .ve-card-summary {
    flex: 1; min-width: 0; font-size: 0.85rem;
    overflow: hidden; text-overflow: ellipsis; white-space: nowrap; opacity: 0.8;
  }
  .ve-expand-icon { font-size: 0.8rem; opacity: 0.4; flex-shrink: 0; }
  .ve-delete {
    border: none; background: none; cursor: pointer;
    font-size: 1rem; opacity: 0.3; padding: 0 0.15rem;
    color: var(--danger, #e26d5a); flex-shrink: 0;
  }
  .ve-delete:hover { opacity: 1; }

  /* Properties panel */
  .ve-props {
    padding: 0.6rem 0.8rem 0.7rem;
    border-top: 1px solid var(--stroke, #e3ddd4);
    background: var(--panel-soft, #f5f7fb);
    display: flex; flex-direction: column; gap: 0.32rem;
  }
  .ve-prop-label { font-size: 0.78rem; opacity: 0.6; margin-top: 0.1rem; }
  .ve-input {
    padding: 0.22rem 0.45rem;
    border: 1px solid var(--stroke, #e3ddd4); border-radius: 5px;
    background: var(--panel, #ffffff); color: var(--ink, #1f2328);
    font-size: 0.875rem; font-family: inherit; width: 100%; box-sizing: border-box;
  }
  .ve-input-short { width: 5rem; flex-shrink: 0; }
  .ve-textarea {
    padding: 0.3rem 0.45rem; resize: vertical;
    border: 1px solid var(--stroke, #e3ddd4); border-radius: 5px;
    background: var(--panel, #ffffff); color: var(--ink, #1f2328);
    font-size: 0.875rem; font-family: inherit; width: 100%; box-sizing: border-box;
  }
  .ve-row-trio { display: flex; gap: 0.4rem; }
  .ve-row-trio > div { flex: 1; display: flex; flex-direction: column; gap: 0.2rem; }

  /* Panel children */
  .ve-children-bar {
    display: flex; align-items: center; justify-content: space-between;
    flex-wrap: wrap; gap: 0.3rem;
    padding: 0.4rem 0; margin-top: 0.3rem;
    border-top: 1px solid var(--stroke, #e3ddd4);
  }
  .ve-child-card {
    border: 1px solid var(--stroke, #e3ddd4); border-radius: 5px;
    margin-top: 0.3rem; overflow: hidden;
  }
  .ve-child-header {
    display: flex; align-items: center; gap: 0.35rem;
    padding: 0.32rem 0.5rem; cursor: pointer; user-select: none;
    background: var(--panel, #ffffff);
  }
  .ve-child-header:hover { background: var(--accent-soft, #d6e2f6); }
  .ve-child-props {
    padding: 0.5rem 0.6rem;
    background: var(--panel-soft, #f5f7fb);
    border-top: 1px solid var(--stroke, #e3ddd4);
    display: flex; flex-direction: column; gap: 0.3rem;
  }

  .ve-hint { font-size: 0.72rem; opacity: 0.55; font-weight: 400; }
  .ve-unknown { font-size: 0.82rem; opacity: 0.55; margin: 0; }
  .ve-unknown code { background: var(--panel, #fff); padding: 0 0.2rem; border-radius: 3px; }
  .ve-empty { padding: 0.8rem; font-size: 0.85rem; opacity: 0.4; text-align: center; }
  .ve-empty-full { flex: 1; display: flex; align-items: center; justify-content: center; }

  /* Variable bar */
  .se-var-bar {
    display: flex; flex-wrap: wrap; align-items: center; gap: 0.3rem;
    padding: 0.4rem 0.7rem; border-top: 1px solid var(--stroke, #e3ddd4);
    font-size: 0.8rem; flex-shrink: 0; background: var(--panel-soft, #f5f7fb);
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

  /* Preview */
  .se-preview-col {
    display: flex; flex-direction: column; flex: 1; min-width: 0;
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
    text-transform: uppercase; letter-spacing: 0.05em; opacity: 0.55; flex-shrink: 0;
  }
  .se-screen-selector { display: flex; align-items: center; gap: 0.4rem; flex: 1; min-width: 0; }
  .se-screen-label {
    font-size: 0.78rem; opacity: 0.55; white-space: nowrap;
    text-transform: uppercase; letter-spacing: 0.04em;
  }
  .se-screen-select {
    flex: 1; min-width: 0; max-width: 240px; padding: 0.18rem 0.5rem;
    border: 1px solid var(--stroke, #e3ddd4); border-radius: 5px;
    background: var(--panel-soft, #f5f7fb); color: var(--ink, #1f2328);
    font-size: 0.875rem; font-family: 'IBM Plex Mono', monospace; cursor: pointer;
  }
  .se-preview-hint {
    flex: 1; display: flex; align-items: center; justify-content: center;
    padding: 2rem; opacity: 0.4; font-size: 0.9rem; text-align: center;
  }
  .se-preview-frame { flex: 1; width: 100%; border: none; background: #fff; }
</style>
