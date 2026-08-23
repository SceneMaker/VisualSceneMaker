<script>
  import { onMount, onDestroy } from "svelte";
  import { EditorView, basicSetup } from "codemirror";
  import { EditorState } from "@codemirror/state";
  import { json } from "@codemirror/lang-json";
  import { linter, lintGutter } from "@codemirror/lint";
  import { insertChildAtPath, pathKey } from "./screenTree.js";
  import ScreenElementRow from "./ScreenElementRow.svelte";
  import ScreenElementProperties from "./ScreenElementProperties.svelte";

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

  const BUTTON_ICONS = [
    { v: "",            label: "— none (text only) —" },
    { v: "send",        label: "Send" },
    { v: "microphone",  label: "Microphone" },
    { v: "speaker-on",  label: "Speaker (on)" },
    { v: "speaker-off", label: "Speaker (off)" },
  ];

  // Preview-only copies of plugins/htmlgui-ws/src/main/resources/renderer/vsm-renderer.js's
  // ICONS, so the picker below can show what each option looks like. The runtime rendering
  // itself happens in the renderer, not here — keep these two lists in sync by hand.
  const ICON_SVG = {
    send: `<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" width="16" height="16"><path stroke-linecap="round" stroke-linejoin="round" d="M6 12 3.269 3.125A59.769 59.769 0 0 1 21.485 12 59.768 59.768 0 0 1 3.27 20.875L5.999 12Zm0 0h7.5" /></svg>`,
    microphone: `<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" width="16" height="16"><path stroke-linecap="round" stroke-linejoin="round" d="M12 18.75a6 6 0 0 0 6-6v-1.5m-6 7.5a6 6 0 0 1-6-6v-1.5m6 7.5v3.75m-3.75 0h7.5M12 15.75a3 3 0 0 1-3-3V4.5a3 3 0 1 1 6 0v8.25a3 3 0 0 1-3 3Z" /></svg>`,
    "speaker-on": `<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" width="16" height="16"><path stroke-linecap="round" stroke-linejoin="round" d="M19.114 5.636a9 9 0 0 1 0 12.728M16.463 8.288a5.25 5.25 0 0 1 0 7.424M6.75 8.25l4.72-4.72a.75.75 0 0 1 1.28.53v15.88a.75.75 0 0 1-1.28.53l-4.72-4.72H4.51c-.88 0-1.704-.507-1.938-1.354A9.009 9.009 0 0 1 2.25 12c0-.83.112-1.633.322-2.396C2.806 8.756 3.63 8.25 4.51 8.25H6.75Z" /></svg>`,
    "speaker-off": `<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" width="16" height="16"><path stroke-linecap="round" stroke-linejoin="round" d="M17.25 9.75 19.5 12m0 0 2.25 2.25M19.5 12l2.25-2.25M19.5 12l-2.25 2.25m-10.5-6 4.72-4.72a.75.75 0 0 1 1.28.53v15.88a.75.75 0 0 1-1.28.53l-4.72-4.72H4.51c-.88 0-1.704-.507-1.938-1.354A9.009 9.009 0 0 1 2.25 12c0-.83.112-1.633.322-2.396C2.806 8.756 3.63 8.25 4.51 8.25H6.75Z" /></svg>`,
  };

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

  // Push schema to preview whenever it changes (live visual editing).
  $: parsedSchema && sendSchemaToPreview();

  $: _charParam = charEnabled && charUrl.trim() ? `&character=${encodeURIComponent(charUrl.trim())}` : "";
  $: previewUrl = `/web-ui/screens-preview.html?project=${projectId}&screen=${encodeURIComponent(selectedScreen)}&r=${previewKey}${_charParam}`;

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
  // Which rows are expanded, at any depth — every element at every depth is now a
  // ScreenElementRow, so one Set replaces what used to be expandedEl (depth 0) and
  // expandedChild (depth 1) as separate variables. A Set rather than one exclusive "selected
  // path": several rows at different depths can be open at once, and expanding a child must
  // never depend on — or clear — whatever ancestor got you there (see ScreenElementRow's
  // top-of-file comment for what went wrong when this was a single value).
  let expandedPaths = new Set();
  let modeError     = "";

  function toggleExpanded(path) {
    const key = pathKey(path);
    const next = new Set(expandedPaths);
    if (next.has(key)) next.delete(key); else next.add(key);
    expandedPaths = next;
  }

  /** Applies a full elements tree produced by editing at any depth. */
  function applyElementsTree(newElements) {
    parsedSchema.screens[selectedScreen].elements = newElements;
    parsedSchema = { ...parsedSchema };
    commitParsed();
  }

  /** Palette clicks add at the root; expand the new element the way addElement() used to. */
  function addTopLevelElement(el) {
    const newPath = [veElements.length];
    applyElementsTree(insertChildAtPath(veElements, [], el));
    expandedPaths = new Set(expandedPaths).add(pathKey(newPath));
  }

  let previewIframe  = null;         // bound to the preview <iframe>
  let previewLoaded  = false;        // true once the iframe fires its load event

  // ── character layer ───────────────────────────────────────────────────────
  let charEnabled    = false;
  let charUrl        = "";
  let charExpanded   = false;
  let charSaving     = false;
  let charSaveError  = "";

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
      const [sRes, vRes, cRes] = await Promise.all([
        apiGet(`/api/v1/projects/${projectId}/screens`),
        apiGet(`/api/v1/projects/${projectId}/variables`),
        apiGet(`/api/v1/projects/${projectId}/character-config`).catch(() => ({})),
      ]);
      const empty   = !sRes || Object.keys(sRes).length === 0;
      const content = JSON.stringify(empty ? minimalTemplate() : sRes, null, 2);
      setEditorContent(content);
      variables    = vRes?.variables ?? [];
      parsedSchema = JSON.parse(content);
      charUrl      = cRes?.url ?? "";
      charEnabled  = !!charUrl;
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

  async function saveCharConfig() {
    charSaving = true; charSaveError = "";
    try {
      const payload = charEnabled && charUrl.trim() ? { url: charUrl.trim() } : {};
      await apiPut(`/api/v1/projects/${projectId}/character-config`, payload);
      previewKey++;
    } catch (e) {
      charSaveError = e.message || "Failed to save character config.";
    } finally { charSaving = false; }
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
    if (type === "vsm-animate")     return "Anim";
    if (type === "vsm-chat-input")  return "✉";
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
    if (el.type === "vsm-feed")    return el.dataVar ? `Feed · ${el.dataVar}` : "Feed (no variable)";
    if (el.type === "vsm-animate") {
      const rateLabel = el.rateVar ? ` · rate: ${el.rateVar}` : "";
      return `${el.animation ?? "heartbeat"}${rateLabel}`;
    }
    if (el.type === "vsm-chat-input") {
      const sends = el.sendsVar ? `→ ${el.sendsVar}` : "";
      const binds = el.bindVar ? `← ${el.bindVar}` : "";
      const vars  = [sends, binds].filter(Boolean).join(" ");
      return vars ? `Chat input ${vars}` : "Chat input (no variable)";
    }
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
  // ── palette groups (left sidebar) ────────────────────────────────────────
  const paletteGroups = [
    { label: "Text & Input", items: [
      { icon: "T",   label: "Text",     create: () => ({ type:"sl-text", content:"Text" }) },
      { icon: "Btn", label: "Button",   create: () => ({ type:"sl-button", label:"Button", sendsVar:"", sendsValue:"" }) },
      { icon: "In",  label: "Input",    create: () => ({ type:"sl-input", label:"Input", bindVar:"" }) },
      { icon: "At",  label: "Textarea", create: () => ({ type:"sl-textarea", label:"Textarea", bindVar:"" }) },
      { icon: "▾",   label: "Select",   create: () => ({ type:"sl-select", label:"Select", options:["Option 1","Option 2"], bindVar:"" }) },
      { icon: "✓",   label: "Check",    create: () => ({ type:"sl-checkbox", label:"Checkbox", bindVar:"" }) },
      { icon: "↔",   label: "Slider",   create: () => ({ type:"sl-range", label:"Slider", min:0, max:100, step:1 }) },
    ]},
    { label: "Layout", items: [
      { icon: "▣",   label: "Panel",    create: () => ({ type:"vsm-panel", background:"#f5f5f5", layout:"flex-column", padding:"1rem", children:[] }) },
      { icon: "↕",   label: "Filler",   create: () => ({ type:"vsm-filler", flexGrow:true }) },
    ]},
    { label: "Media", items: [
      { icon: "Img", label: "Image",    create: () => ({ type:"vsm-image", src:"", alt:"" }) },
      { icon: "▶",   label: "Video",    create: () => ({ type:"vsm-video", src:"", controls:true }) },
      { icon: "♪",   label: "Audio",    create: () => ({ type:"vsm-audio", src:"", controls:true }) },
      { icon: "⊞",   label: "Embed",    create: () => ({ type:"vsm-embed", src:"", width:"100%", height:"315px" }) },
    ]},
    { label: "Data & FX", items: [
      { icon: "💬",  label: "Bubble",     create: () => ({ type:"vsm-bubble", content:"Hello!", tail:"bottom", background:"#e8f4fd" }) },
      { icon: "Ch",  label: "Chart",      create: () => ({ type:"vsm-chart", chartType:"bar", dataVar:"", label:"", color:"#5b8edc", height:"300px" }) },
      { icon: "Fd",  label: "Feed",       create: () => ({ type:"vsm-feed", dataVar:"", height:"400px", agentColor:"#e8f4fd", userColor:"#eafbe8", systemColor:"#f5f5f5", agentLabel:"Agent", userLabel:"You" }) },
      { icon: "Fx",  label: "Animate",    create: () => ({ type:"vsm-animate", animation:"heartbeat", color:"#e26d5a", width:"80px", height:"80px" }) },
      { icon: "✉",   label: "Chat Input", create: () => ({ type:"vsm-chat-input", sendsVar:"", bindVar:"", placeholder:"Type your message…", buttonLabel:"Send" }) },
    ]},
  ];

  // ── delete screen ─────────────────────────────────────────────────────────
  function deleteScreen(name) {
    if (!parsedSchema?.screens) return;
    if (screenNames.length <= 1) return; // refuse to delete the last screen
    delete parsedSchema.screens[name];
    parsedSchema = { ...parsedSchema };
    if (selectedScreen === name) {
      const remaining = Object.keys(parsedSchema.screens ?? {});
      selectedScreen = remaining.length > 0 ? remaining[0] : null;
    }
    commitParsed();
  }

  // ── add new screen ────────────────────────────────────────────────────────
  function addScreen() {
    if (!parsedSchema) {
      try { parsedSchema = JSON.parse(schema); } catch { return; }
    }
    let name = "screen"; let n = 2;
    while (parsedSchema.screens?.[name]) name = `screen_${n++}`;
    if (!parsedSchema.screens) parsedSchema.screens = {};
    parsedSchema.screens[name] = { background: "#ffffff", layout: "flex-column", elements: [] };
    parsedSchema = { ...parsedSchema };
    selectedScreen = name;
    commitParsed();
  }

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
      let msg = e.message || "Save failed.";
      try { const p = JSON.parse(msg); if (p?.message) msg = p.message; } catch {}
      saveError = msg;
    } finally { saveBusy = false; }
  }

  function handleKeydown(e) {
    if ((e.metaKey || e.ctrlKey) && e.key === "s") { e.preventDefault(); save(); }
    if (e.key === "Escape") { if (showTemplatePicker) { showTemplatePicker = false; } else { onClose(); } }
  }

  // ── template picker ───────────────────────────────────────────────────────
  let showTemplatePicker = false;
  let templateList       = [];
  let templateLoading    = false;
  let templateError      = "";

  async function openTemplatePicker() {
    showTemplatePicker = true;
    templateLoading    = true;
    templateError      = "";
    try {
      const r = await fetch('/web-ui/screen-templates/index.json');
      if (!r.ok) throw new Error(`HTTP ${r.status}`);
      templateList = await r.json();
    } catch (e) {
      templateError = "Could not load templates: " + (e.message || e);
    } finally {
      templateLoading = false;
    }
  }

  async function importTemplate(id) {
    templateError = "";
    try {
      const r = await fetch(`/web-ui/screen-templates/${id}.json`);
      if (!r.ok) throw new Error(`HTTP ${r.status}`);
      const tmpl = await r.json();
      const base = parsedSchema ?? { version: 1, screens: {} };
      const incoming = tmpl.screens ?? {};
      let firstKey = null;
      for (const [name, screen] of Object.entries(incoming)) {
        let key = name, n = 2;
        while (base.screens[key]) key = `${name}_${n++}`;
        base.screens[key] = screen;
        if (!firstKey) firstKey = key;
      }
      parsedSchema = { ...base };
      if (firstKey) selectedScreen = firstKey;
      commitParsed();
      showTemplatePicker = false;
    } catch (e) {
      templateError = "Import failed: " + (e.message || e);
    }
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
      <button class="se-btn se-btn-icon" on:click={() => window.open('/screen-element-reference.html', '_blank')}
              title="Screen element reference">
        <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" width="16" height="16">
          <path stroke-linecap="round" stroke-linejoin="round" d="M16.712 4.33a9.027 9.027 0 0 1 1.652 1.306c.51.51.944 1.064 1.306 1.652M16.712 4.33l-3.448 4.138m3.448-4.138a9.014 9.014 0 0 0-9.424 0M19.67 7.288l-4.138 3.448m4.138-3.448a9.014 9.014 0 0 1 0 9.424m-4.138-5.976a3.736 3.736 0 0 0-.88-1.388 3.737 3.737 0 0 0-1.388-.88m2.268 2.268a3.765 3.765 0 0 1 0 2.528m-2.268-4.796a3.765 3.765 0 0 0-2.528 0m4.796 4.796c-.181.506-.475.982-.88 1.388a3.736 3.736 0 0 1-1.388.88m2.268-2.268 4.138 3.448m0 0a9.027 9.027 0 0 1-1.306 1.652c-.51.51-1.064.944-1.652 1.306m0 0-3.448-4.138m3.448 4.138a9.014 9.014 0 0 1-9.424 0m5.976-4.138a3.765 3.765 0 0 1-2.528 0m0 0a3.736 3.736 0 0 1-1.388-.88 3.737 3.737 0 0 1-.88-1.388m2.268 2.268L7.288 19.67m0 0a9.024 9.024 0 0 1-1.652-1.306 9.027 9.027 0 0 1-1.306-1.652m0 0 4.138-3.448M4.33 16.712a9.014 9.014 0 0 1 0-9.424m4.138 5.976a3.765 3.765 0 0 1 0-2.528m0 0c.181-.506.475-.982.88-1.388a3.736 3.736 0 0 1 1.388-.88m-2.268 2.268L4.33 7.288m6.406 1.18L7.288 4.33m0 0a9.024 9.024 0 0 0-1.652 1.306A9.025 9.025 0 0 0 4.33 7.288" />
        </svg>
      </button>
      <button class="se-btn se-btn-template" on:click={openTemplatePicker}
              title="Add a pre-authored screen template">From Template…</button>
      <button class="se-btn se-btn-primary" disabled={saveBusy || !dirty}
              on:click={save} title="Save (Cmd/Ctrl+S)">
        {saveBusy ? "Saving…" : "Save"}
      </button>
      <button class="se-btn" on:click={onClose} title="Close (Esc)">Close</button>
    </div>
  </div>

  <!-- Body -->
  <div class="se-body">

    <!-- Left sidebar: screens + element palette -->
    <div class="se-left">
      <div class="se-left-section">Screens</div>
      <div class="se-screens-list">
        {#each screenNames as name}
          <div class="se-screen-row" class:se-screen-active={selectedScreen === name}>
            <button class="se-screen-item" on:click={() => selectedScreen = name}>{name}</button>
            {#if screenNames.length > 1}
              <button class="se-screen-del" title="Delete screen '{name}'"
                      on:click|stopPropagation={() => deleteScreen(name)}>×</button>
            {/if}
          </div>
        {/each}
        {#if screenNames.length === 0}
          <div class="se-screens-empty">No screens yet</div>
        {/if}
      </div>
      <button class="se-new-screen-btn" on:click={addScreen}>+ New Screen</button>
      <div class="se-left-sep"></div>
      <div class="se-left-section">Palette</div>
      <div class="se-palette">
        {#each paletteGroups as group}
          <div class="se-palette-group">{group.label}</div>
          <div class="se-palette-row">
            {#each group.items as item}
              <button class="se-palette-tile" title="{item.label}"
                      on:click={() => addTopLevelElement(item.create())}>
                <span class="se-tile-icon">{item.icon}</span>
                <span class="se-tile-name">{item.label}</span>
              </button>
            {/each}
          </div>
        {/each}
      </div>
    </div>

    <!-- Center: editor / properties panel -->
    <div class="se-center">

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

        <!-- Character layer (project-level, always visible) -->
        <div class="ve-section ve-char-section">
          <div class="ve-section-title ve-char-toggle"
               role="button" tabindex="0"
               on:click={() => charExpanded = !charExpanded}
               on:keydown={e => (e.key === 'Enter' || e.key === ' ') && (charExpanded = !charExpanded)}>
            <span class="ve-char-arrow">{charExpanded ? '▼' : '▶'}</span>
            Character Layer
            {#if charEnabled && charUrl.trim()}
              <span class="ve-char-badge">active</span>
            {/if}
          </div>
          {#if charExpanded}
            <div class="ve-row" style="margin-top:0.4rem">
              <label class="ve-label">Enable</label>
              <input type="checkbox" bind:checked={charEnabled}
                     on:change={saveCharConfig}>
            </div>
            {#if charEnabled}
              <div class="ve-row ve-char-url-row">
                <label class="ve-label">URL</label>
                <input class="ve-input ve-char-url-input"
                       type="url"
                       placeholder="https://…"
                       bind:value={charUrl}
                       on:blur={saveCharConfig}>
                <button class="ve-btn-icon" disabled={charSaving}
                        on:click={saveCharConfig}
                        title="Apply URL">✓</button>
              </div>
              {#if charSaveError}
                <div class="ve-char-error">{charSaveError}</div>
              {/if}
            {/if}
          {/if}
        </div>

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
            </div>

            {#if veElements.length === 0}
              <div class="ve-empty">No elements yet — pick one from the palette.</div>
            {/if}

            {#each veElements as el, i (i)}
              <ScreenElementRow
                elements={veElements} path={[i]}
                {expandedPaths}
                onToggle={toggleExpanded}
                onChange={applyElementsTree}
                {typeLabel} {elementSummary} {alignItemsOpts} {justifyOpts}
                {textAlignOpts} {fontOpts} {variables}
                {parseColorAlpha} {buildColorAlpha} {optionsToText} {textToOptions}
                {BUTTON_ICONS} {ICON_SVG} />
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

    <!-- Template picker modal -->
    {#if showTemplatePicker}
    <!-- svelte-ignore a11y-no-noninteractive-element-interactions -->
    <div class="tp-backdrop" role="dialog" aria-modal="true"
         on:click|self={() => showTemplatePicker = false}
         on:keydown={e => e.key === "Escape" && (showTemplatePicker = false)}>
      <div class="tp-modal">
        <div class="tp-header">
          <span class="tp-title">Import Screen Template</span>
          <button class="se-btn se-btn-sm" on:click={() => showTemplatePicker = false}>✕ Close</button>
        </div>
        <p class="tp-hint">
          Select a template to add a pre-built screen to your project.
          Rename variable bindings in the Visual editor afterwards.
        </p>
        {#if templateError}
          <div class="tp-error">{templateError}</div>
        {/if}
        {#if templateLoading}
          <div class="tp-loading">Loading templates…</div>
        {:else}
          <div class="tp-grid">
            {#each templateList as t}
              <button class="tp-card" on:click={() => importTemplate(t.id)}>
                <div class="tp-card-label">{t.label}</div>
                <div class="tp-card-desc">{t.description}</div>
                {#if t.variables?.length > 0}
                  <div class="tp-card-vars">
                    {#each t.variables as v}
                      <span class="tp-var-chip" title={v.description}>{v.name}</span>
                    {/each}
                  </div>
                {/if}
                {#if t.requires?.length > 1}
                  <div class="tp-card-requires">
                    requires: {t.requires.join(", ")}
                  </div>
                {/if}
              </button>
            {/each}
          </div>
        {/if}
      </div>
    </div>
    {/if}

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
  @import url('https://fonts.googleapis.com/css2?family=DM+Mono:wght@400;500&family=DM+Sans:wght@400;500;600;700&display=swap');

  /* ── IDE Design Tokens ──────────────────────────────────────────────────── */
  .se-overlay {
    --ide-bg:         #f8f9fb;
    --ide-sidebar:    #f0f2f5;
    --ide-panel:      #ffffff;
    --ide-surface:    #f5f6f8;
    --ide-border:     #e4e7ec;
    --ide-border-md:  #d0d5dd;
    --ide-text:       #111827;
    --ide-muted:      #6b7280;
    --ide-dim:        #9ca3af;
    --ide-accent:     #2563eb;
    --ide-glow:       rgba(37,99,235,0.08);
    --ide-warm:       #d97706;
    --ide-success:    #059669;
    --ide-danger:     #dc2626;
    --ide-danger-bg:  rgba(220,38,38,0.08);
    /* bridge to child components that use parent theme vars */
    --panel:          #ffffff;
    --panel-soft:     #f5f6f8;
    --stroke:         #e4e7ec;
    --ink:            #111827;
    --accent:         #2563eb;
    --accent-soft:    rgba(37,99,235,0.08);
    --button:         #2563eb;
    --button-pressed: #1d4ed8;
    --danger:         #dc2626;
    --hover-bg:       #f3f4f6;

    position: fixed; inset: 0; z-index: 600;
    display: flex; flex-direction: column;
    background: var(--ide-bg);
    color: var(--ide-text);
    font-family: 'DM Sans', system-ui, sans-serif;
  }
  /* ── Header ─────────────────────────────────────────────────────────────── */
  .se-header {
    display: flex; align-items: center; justify-content: space-between;
    padding: 0 0.9rem; height: 44px;
    border-bottom: 1px solid var(--ide-border);
    flex-shrink: 0; gap: 0.75rem;
    background: var(--ide-sidebar);
  }
  .se-title { display: flex; align-items: baseline; gap: 0.4rem; font-size: 0.9rem; }
  .se-title-main   { font-weight: 700; letter-spacing: -0.01em; }
  .se-title-sep    { color: var(--ide-dim); }
  .se-title-plugin { color: var(--ide-accent); font-weight: 500; }
  .se-header-actions { display: flex; align-items: center; gap: 0.4rem; }

  .se-badge { font-size: 0.72rem; padding: 0.12rem 0.5rem; border-radius: 99px; font-weight: 600; }
  .se-badge-error { background: var(--ide-danger-bg); color: var(--ide-danger); }
  .se-badge-ok    { background: rgba(63,185,80,0.12); color: var(--ide-success); }
  .se-badge-warn  { background: rgba(240,136,62,0.12); color: var(--ide-warm); }

  .se-btn {
    padding: 0.24rem 0.7rem;
    border: 1px solid var(--ide-border-md); border-radius: 5px;
    background: var(--ide-surface); color: var(--ide-text);
    cursor: pointer; font-size: 0.82rem; font-family: inherit; white-space: nowrap;
    transition: background 0.12s, border-color 0.12s;
  }
  .se-btn:hover:not(:disabled) { background: var(--ide-glow); border-color: var(--ide-accent); }
  .se-btn:disabled { opacity: 0.35; cursor: default; }
  .se-btn-primary {
    background: var(--ide-accent); color: #fff;
    border-color: transparent; font-weight: 600;
  }
  .se-btn-primary:hover:not(:disabled) { background: var(--button-pressed); border-color: transparent; }
  .se-btn-sm { padding: 0.1rem 0.45rem; font-size: 0.75rem; }
  .se-btn-icon { display: flex; align-items: center; justify-content: center; padding: 0.24rem 0.4rem; }
  .se-btn-template {
    background: rgba(56,139,253,0.08); color: var(--ide-accent);
    border-color: rgba(56,139,253,0.25); font-weight: 500;
  }
  .se-btn-template:hover { background: var(--ide-glow); }

  /* ── Body grid ───────────────────────────────────────────────────────────── */
  .se-body {
    display: grid;
    grid-template-columns: 210px 1fr 1fr;
    flex: 1; min-height: 0; overflow: hidden;
    position: relative;
  }

  /* ── Left sidebar ────────────────────────────────────────────────────────── */
  .se-left {
    display: flex; flex-direction: column;
    background: var(--ide-sidebar);
    border-right: 1px solid var(--ide-border);
    overflow: hidden; min-height: 0;
  }
  .se-left-section {
    padding: 0.6rem 0.75rem 0.2rem;
    font-size: 0.62rem; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.1em;
    color: var(--ide-muted); flex-shrink: 0;
  }
  .se-screens-list {
    display: flex; flex-direction: column;
    padding: 0 0.4rem; gap: 1px; flex-shrink: 0;
    max-height: 180px; overflow-y: auto;
  }
  .se-screen-row {
    display: flex; align-items: center; border-radius: 5px;
    transition: background 0.1s;
  }
  .se-screen-row:hover { background: rgba(0,0,0,0.04); }
  .se-screen-active {
    background: var(--ide-glow) !important;
  }
  .se-screen-item {
    flex: 1; text-align: left; padding: 0.3rem 0.65rem;
    border: none; border-radius: 5px; cursor: pointer;
    font-size: 0.8rem; font-family: 'DM Mono', monospace;
    background: none; color: var(--ide-muted);
    white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
    transition: color 0.1s;
  }
  .se-screen-row:hover .se-screen-item { color: var(--ide-text); }
  .se-screen-active .se-screen-item { color: var(--ide-accent) !important; font-weight: 600; }
  .se-screen-del {
    flex-shrink: 0; padding: 0 0.45rem; height: 100%;
    border: none; background: none; cursor: pointer;
    font-size: 0.85rem; color: var(--ide-dim);
    opacity: 0; transition: opacity 0.1s, color 0.1s;
    line-height: 1;
  }
  .se-screen-row:hover .se-screen-del { opacity: 1; }
  .se-screen-del:hover { color: var(--ide-danger, #dc2626); }
  .se-screens-empty { padding: 0.4rem 0.65rem; font-size: 0.75rem; color: var(--ide-dim); font-style: italic; }
  .se-new-screen-btn {
    margin: 0.4rem 0.6rem; padding: 0.26rem 0.5rem;
    border: 1px dashed var(--ide-border-md); border-radius: 5px;
    background: none; color: var(--ide-muted);
    font-size: 0.75rem; font-family: inherit; cursor: pointer;
    text-align: center; flex-shrink: 0;
    transition: color 0.12s, border-color 0.12s;
  }
  .se-new-screen-btn:hover { color: var(--ide-accent); border-color: var(--ide-accent); }
  .se-left-sep { height: 1px; background: var(--ide-border); margin: 0.35rem 0; flex-shrink: 0; }

  /* Palette */
  .se-palette { overflow-y: auto; flex: 1; padding-bottom: 1rem; }
  .se-palette-group {
    padding: 0.45rem 0.75rem 0.2rem;
    font-size: 0.58rem; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.1em;
    color: var(--ide-dim);
  }
  .se-palette-row { display: grid; grid-template-columns: 1fr 1fr; gap: 2px; padding: 0 0.4rem; }
  .se-palette-tile {
    display: flex; flex-direction: column; align-items: center;
    padding: 0.45rem 0.2rem; gap: 0.18rem;
    border: 1px solid transparent; border-radius: 5px;
    background: none; cursor: pointer; font-family: inherit;
    transition: background 0.1s, border-color 0.1s;
  }
  .se-palette-tile:hover { background: var(--ide-glow); border-color: rgba(56,139,253,0.3); }
  .se-tile-icon {
    font-size: 0.72rem; font-weight: 700;
    color: var(--ide-accent); font-family: 'DM Mono', monospace; line-height: 1;
  }
  .se-tile-name { font-size: 0.6rem; color: var(--ide-muted); text-align: center; line-height: 1; }

  /* ── Center panel ────────────────────────────────────────────────────────── */
  .se-center {
    display: flex; flex-direction: column;
    min-width: 0; min-height: 0;
    overflow: hidden; position: relative;
    border-right: 1px solid var(--ide-border);
    background: var(--ide-bg);
  }
  .se-editor-overlay {
    position: absolute; inset: 0; z-index: 2;
    display: flex; align-items: center; justify-content: center; gap: 0.75rem;
    background: var(--ide-bg); font-size: 0.9rem;
  }
  .se-loading-text { color: var(--ide-muted); }
  .se-message-error { color: var(--ide-danger); }

  /* Tab bar */
  .se-tab-bar {
    display: flex; flex-shrink: 0; padding: 0 0.4rem;
    border-bottom: 1px solid var(--ide-border);
    background: var(--ide-sidebar);
  }
  .se-tab {
    padding: 0.42rem 0.9rem; border: none; background: none;
    font-size: 0.8rem; font-family: inherit; cursor: pointer;
    color: var(--ide-muted);
    border-bottom: 2px solid transparent; margin-bottom: -1px;
    transition: color 0.12s;
  }
  .se-tab:hover { color: var(--ide-text); }
  .se-tab-active { color: var(--ide-text) !important; font-weight: 600; border-bottom-color: var(--ide-accent); }

  .se-cm-wrap { flex: 1; min-height: 0; overflow: hidden; }
  .se-cm-hidden { display: none; }

  /* ── Visual editor ───────────────────────────────────────────────────────── */
  .ve-root {
    flex: 1; min-height: 0; overflow-y: auto;
    display: flex; flex-direction: column;
    background: var(--ide-bg);
  }
  .ve-section { background: var(--ide-panel); border-bottom: 1px solid var(--ide-border); padding: 0.6rem 0.8rem; }
  .ve-section-grow { flex: 1; padding: 0; }
  .ve-section-title {
    font-size: 0.6rem; text-transform: uppercase; letter-spacing: 0.1em;
    color: var(--ide-muted); font-weight: 700; margin-bottom: 0.45rem;
  }
  :global(.ve-row) { display: flex; align-items: center; gap: 0.5rem; margin-bottom: 0.35rem; flex-wrap: wrap; }
  :global(.ve-row:last-child) { margin-bottom: 0; }
  :global(.ve-label) { font-size: 0.78rem; color: var(--ide-muted); min-width: 72px; flex-shrink: 0; }

  :global(.ve-color) {
    width: 2.2rem; height: 1.7rem; padding: 0.1rem; border-radius: 4px;
    border: 1px solid var(--ide-border-md); cursor: pointer; background: none;
  }
  :global(.ve-opacity) {
    width: 3.4rem; padding: 0.2rem 0.3rem; border-radius: 4px;
    border: 1px solid var(--ide-border-md); background: var(--ide-surface);
    color: var(--ide-text); font-size: 0.82rem; text-align: right;
  }
  :global(.ve-opacity-unit) { font-size: 0.78rem; color: var(--ide-dim); }
  :global(.ve-media-hint) {
    font-size: 0.75rem; color: var(--ide-muted); margin-bottom: .4rem;
    padding: .3rem .5rem; background: var(--ide-surface);
    border: 1px solid var(--ide-border); border-radius: 5px; line-height: 1.5;
  }
  :global(.ve-media-hint code) { font-size: 0.72rem; color: var(--ide-accent); font-family: 'DM Mono', monospace; }
  :global(.ve-select) {
    flex: 1; padding: 0.22rem 0.4rem;
    border: 1px solid var(--ide-border-md); border-radius: 5px;
    background: var(--ide-surface); color: var(--ide-text);
    font-size: 0.82rem; font-family: inherit;
  }

  :global(.ve-align-row) { display: flex; gap: 0.2rem; flex-wrap: wrap; }
  :global(.ve-align-btn) {
    padding: 0.15rem 0.42rem; border-radius: 4px;
    border: 1px solid var(--ide-border-md);
    background: var(--ide-surface); color: var(--ide-muted);
    font-size: 0.72rem; font-family: inherit; cursor: pointer; white-space: nowrap;
    transition: background 0.1s, color 0.1s;
  }
  :global(.ve-align-btn:hover) { color: var(--ide-text); background: rgba(0,0,0,0.05); }
  :global(.ve-align-active) { background: var(--ide-accent) !important; color: #fff !important; border-color: transparent; }

  :global(.ve-elements-header) {
    display: flex; align-items: center;
    padding: 0.5rem 0.8rem;
    background: var(--ide-panel);
    border-bottom: 1px solid var(--ide-border);
    flex-shrink: 0;
  }

  /* Element cards */
  :global(.ve-card) { background: var(--ide-panel); border-bottom: 1px solid var(--ide-border); }
  /* Depth cue for nested rows (ScreenElementRow sets margin-left inline, scaled by depth) —
     without this border a deeply-indented row just looks like empty space to its left. */
  :global(.ve-card-nested) {
    border-left: 2px solid var(--ide-border);
    border-radius: 0 4px 4px 0;
  }
  :global(.ve-card-header) {
    display: flex; align-items: center; gap: 0.4rem;
    padding: 0.38rem 0.6rem; cursor: pointer; user-select: none;
    transition: background 0.1s;
  }
  :global(.ve-card-header:hover) { background: rgba(0,0,0,0.03); }
  :global(.ve-card-expanded > .ve-card-header) { background: var(--ide-glow); }

  :global(.ve-card-arrows) { display: flex; flex-direction: column; gap: 0; }
  :global(.ve-arrow) {
    padding: 1px 3px; border: none; background: none; cursor: pointer;
    font-size: 0.72rem; line-height: 1; color: var(--ide-muted); border-radius: 3px;
    transition: color 0.1s;
  }
  :global(.ve-arrow:hover:not(:disabled)) { color: var(--ide-text); background: rgba(0,0,0,0.06); }
  :global(.ve-arrow:disabled) { opacity: 0.12; cursor: default; }

  :global(.ve-type-badge) {
    width: 1.55rem; height: 1.55rem; border-radius: 4px; flex-shrink: 0;
    background: var(--ide-glow); border: 1px solid rgba(56,139,253,0.2);
    color: var(--ide-accent);
    font-size: 0.65rem; font-weight: 700; font-family: 'DM Mono', monospace;
    display: flex; align-items: center; justify-content: center;
  }
  :global(.ve-type-badge-sm) { width: 1.3rem; height: 1.3rem; font-size: 0.58rem; }

  :global(.ve-card-summary) {
    flex: 1; min-width: 0; font-size: 0.8rem;
    overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
    color: var(--ide-muted);
  }
  :global(.ve-expand-icon) { font-size: 0.72rem; color: var(--ide-dim); flex-shrink: 0; }
  :global(.ve-delete) {
    border: none; background: none; cursor: pointer;
    font-size: 0.95rem; padding: 0 0.15rem;
    color: var(--ide-danger); opacity: 0.22; flex-shrink: 0;
    transition: opacity 0.12s;
  }
  :global(.ve-delete:hover) { opacity: 1; }

  /* Properties */
  :global(.ve-props) {
    padding: 0.6rem 0.8rem 0.7rem;
    border-top: 1px solid var(--ide-border);
    background: var(--ide-surface);
    display: flex; flex-direction: column; gap: 0.32rem;
  }
  /* Where a deeply-nested row's own fields actually are, in words — indentation alone stops
     being enough to place yourself once a screen has three or four levels of panels. */
  :global(.ve-breadcrumb) {
    font-size: 0.7rem; color: var(--ide-dim);
    padding-bottom: 0.3rem; margin-bottom: 0.15rem;
    border-bottom: 1px solid var(--ide-border);
  }
  :global(.ve-prop-label) { font-size: 0.72rem; color: var(--ide-muted); margin-top: 0.1rem; }
  :global(.ve-input) {
    flex: 1;
    border: 1px solid var(--ide-border-md); border-radius: 5px;
    background: var(--ide-panel); color: var(--ide-text);
    font-size: 0.82rem; font-family: inherit; width: 100%; box-sizing: border-box;
    padding: 0.22rem 0.45rem;
  }
  :global(.ve-input-short) { width: 5rem; flex-shrink: 0; }
  :global(.ve-textarea) {
    padding: 0.3rem 0.45rem; resize: vertical;
    border: 1px solid var(--ide-border-md); border-radius: 5px;
    background: var(--ide-panel); color: var(--ide-text);
    font-size: 0.82rem; font-family: inherit; width: 100%; box-sizing: border-box;
  }
  :global(.ve-row-trio) { display: flex; gap: 0.4rem; }
  :global(.ve-row-trio > div) { flex: 1; display: flex; flex-direction: column; gap: 0.2rem; }

  /* Panel children */
  :global(.ve-children-bar) {
    display: flex; align-items: center; justify-content: space-between;
    flex-wrap: wrap; gap: 0.3rem;
    padding: 0.4rem 0; margin-top: 0.3rem;
    border-top: 1px solid var(--ide-border);
  }
  :global(.ve-add-group) { display: flex; flex-wrap: wrap; gap: 0.2rem; }
  :global(.ve-add-btn) {
    padding: 0.17rem 0.42rem;
    border: 1px solid var(--ide-border); border-radius: 4px;
    background: var(--ide-surface); color: var(--ide-muted);
    font-size: 0.7rem; font-family: inherit; cursor: pointer;
    transition: background 0.1s, color 0.1s;
  }
  :global(.ve-add-btn:hover) { background: var(--ide-glow); color: var(--ide-accent); }
  :global(.ve-add-btn-feed)    { color: var(--ide-success); border-color: rgba(63,185,80,0.3); }
  :global(.ve-add-btn-animate) { color: var(--ide-danger);  border-color: rgba(248,81,73,0.3); }
  :global(.ve-add-btn-panel)   { color: var(--ide-accent);  border-color: rgba(56,139,253,0.3); }

  :global(.ve-child-card) { border: 1px solid var(--ide-border); border-radius: 5px; margin-top: 0.3rem; overflow: hidden; }
  :global(.ve-child-header) {
    display: flex; align-items: center; gap: 0.35rem;
    padding: 0.3rem 0.5rem; cursor: pointer; user-select: none;
    background: var(--ide-panel); transition: background 0.1s;
  }
  :global(.ve-child-header:hover) { background: rgba(0,0,0,0.03); }
  :global(.ve-child-props) {
    padding: 0.5rem 0.6rem; background: var(--ide-surface);
    border-top: 1px solid var(--ide-border);
    display: flex; flex-direction: column; gap: 0.3rem;
  }

  :global(.ve-hint) { font-size: 0.68rem; color: var(--ide-dim); font-weight: 400; }
  :global(.ve-icon-preview) { display: inline-flex; align-items: center; color: var(--ide-text); opacity: 0.8; }
  :global(.ve-unknown) { font-size: 0.8rem; color: var(--ide-muted); margin: 0; }
  :global(.ve-unknown code) { background: var(--ide-panel); padding: 0 0.2rem; border-radius: 3px; font-family: 'DM Mono', monospace; }
  :global(.ve-empty) { padding: 0.8rem; font-size: 0.8rem; color: var(--ide-dim); text-align: center; }
  :global(.ve-empty-full) { flex: 1; display: flex; align-items: center; justify-content: center; }

  /* Variable bar */
  .se-var-bar {
    display: flex; flex-wrap: wrap; align-items: center; gap: 0.3rem;
    padding: 0.35rem 0.7rem; border-top: 1px solid var(--ide-border);
    font-size: 0.78rem; flex-shrink: 0; background: var(--ide-sidebar);
  }
  .se-var-label {
    color: var(--ide-dim); white-space: nowrap; margin-right: 0.1rem;
    font-size: 0.62rem; text-transform: uppercase; letter-spacing: 0.08em;
  }
  .se-var-chip {
    background: var(--ide-glow); color: var(--ide-accent);
    border: 1px solid rgba(56,139,253,0.2);
    border-radius: 3px; padding: 0.08rem 0.4rem;
    font-family: 'DM Mono', monospace; font-size: 0.7rem; cursor: default;
    transition: border-color 0.1s;
  }
  .se-var-chip:hover { border-color: var(--ide-accent); }

  /* ── Preview column ──────────────────────────────────────────────────────── */
  .se-preview-col { display: flex; flex-direction: column; min-width: 0; background: #eef0f4; }
  .se-preview-header {
    display: flex; align-items: center; justify-content: space-between;
    padding: 0 0.7rem; height: 36px; gap: 0.6rem;
    border-bottom: 1px solid var(--ide-border);
    flex-shrink: 0; background: var(--ide-sidebar);
  }
  .se-preview-label {
    font-size: 0.62rem; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.1em;
    color: var(--ide-muted); flex-shrink: 0;
  }
  .se-screen-selector { display: flex; align-items: center; gap: 0.4rem; flex: 1; min-width: 0; }
  .se-screen-label { font-size: 0.7rem; color: var(--ide-muted); white-space: nowrap; }
  .se-screen-select {
    flex: 1; min-width: 0; max-width: 200px; padding: 0.14rem 0.4rem;
    border: 1px solid var(--ide-border-md); border-radius: 4px;
    background: var(--ide-surface); color: var(--ide-text);
    font-size: 0.78rem; font-family: 'DM Mono', monospace; cursor: pointer;
  }
  .se-preview-hint { flex: 1; display: flex; align-items: center; justify-content: center; color: var(--ide-dim); }
  .se-preview-frame { flex: 1; width: 100%; border: none; background: #fff; }

  /* ── Template picker ─────────────────────────────────────────────────────── */
  .tp-backdrop {
    position: absolute; inset: 0; z-index: 10;
    background: rgba(0,0,0,0.65);
    display: flex; align-items: center; justify-content: center;
  }
  .tp-modal {
    background: var(--ide-panel); border-radius: 10px;
    border: 1px solid var(--ide-border-md);
    box-shadow: 0 20px 60px rgba(0,0,0,0.5);
    width: 680px; max-width: 94vw; max-height: 80vh;
    display: flex; flex-direction: column; overflow: hidden;
  }
  .tp-header {
    display: flex; align-items: center; justify-content: space-between;
    padding: 0.75rem 1rem; border-bottom: 1px solid var(--ide-border); flex-shrink: 0;
  }
  .tp-title { font-weight: 700; font-size: 0.95rem; }
  .tp-hint { font-size: 0.8rem; color: var(--ide-muted); padding: 0.5rem 1rem 0; margin: 0; }
  .tp-loading { padding: 2rem; text-align: center; color: var(--ide-muted); }
  .tp-error { margin: 0.5rem 1rem; padding: 0.5rem 0.75rem; background: var(--ide-danger-bg); color: var(--ide-danger); border-radius: 6px; font-size: 0.8rem; }
  .tp-grid { display: grid; grid-template-columns: repeat(2, 1fr); gap: 0.5rem; padding: 0.75rem 1rem 1rem; overflow-y: auto; }
  .tp-card {
    text-align: left; padding: 0.7rem 0.85rem;
    border: 1px solid var(--ide-border); border-radius: 7px;
    background: var(--ide-surface); cursor: pointer;
    font-family: inherit; transition: border-color .15s, background .15s;
    display: flex; flex-direction: column; gap: 0.3rem;
  }
  .tp-card:hover { border-color: var(--ide-accent); background: var(--ide-glow); }
  .tp-card-label { font-weight: 600; font-size: 0.88rem; color: var(--ide-text); }
  .tp-card-desc  { font-size: 0.75rem; color: var(--ide-muted); line-height: 1.4; }
  .tp-card-vars  { display: flex; flex-wrap: wrap; gap: 0.25rem; margin-top: 0.15rem; }
  .tp-var-chip { font-size: 0.68rem; padding: 0.1rem 0.4rem; background: var(--ide-glow); color: var(--ide-accent); border-radius: 99px; font-family: 'DM Mono', monospace; }
  .tp-card-requires { font-size: 0.68rem; color: var(--ide-warm); margin-top: 0.1rem; }

  /* ── Character layer ─────────────────────────────────────────────────────── */
  .ve-char-section { cursor: default; }
  .ve-char-toggle { display: flex; align-items: center; gap: 0.4rem; cursor: pointer; user-select: none; margin-bottom: 0; transition: opacity 0.12s; }
  .ve-char-toggle:hover { opacity: 0.75; }
  .ve-char-arrow { font-size: 0.55rem; color: var(--ide-dim); }
  .ve-char-badge { font-size: 0.62rem; padding: 0.08rem 0.4rem; border-radius: 99px; background: rgba(63,185,80,0.12); color: var(--ide-success); font-weight: 600; margin-left: 0.25rem; }
  .ve-char-url-row { align-items: stretch; }
  .ve-char-url-input {
    flex: 1; padding: 0.22rem 0.4rem;
    border: 1px solid var(--ide-border-md); border-radius: 5px;
    background: var(--ide-surface); color: var(--ide-text);
    font-size: 0.8rem; font-family: inherit; min-width: 0;
  }
  .ve-btn-icon {
    padding: 0.2rem 0.5rem; border-radius: 5px;
    border: 1px solid var(--ide-border-md);
    background: var(--ide-surface); color: var(--ide-text);
    font-size: 0.82rem; cursor: pointer; flex-shrink: 0;
    transition: background 0.1s, color 0.1s;
  }
  .ve-btn-icon:hover:not(:disabled) { background: var(--ide-glow); color: var(--ide-accent); }
  .ve-btn-icon:disabled { opacity: 0.3; cursor: default; }
  .ve-char-error { font-size: 0.75rem; color: var(--ide-danger); margin-top: 0.3rem; }
</style>
