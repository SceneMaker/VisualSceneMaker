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
  let expandedEl    = null;          // index of expanded top-level element
  let expandedChild = null;          // { pi, ci } for expanded panel child
  let modeError     = "";

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
      return el.sendsVar ? `Chat input → ${el.sendsVar}` : "Chat input (no variable)";
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
      { icon: "✉",   label: "Chat Input", create: () => ({ type:"vsm-chat-input", sendsVar:"", placeholder:"Type your message…", buttonLabel:"Send" }) },
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
                      on:click={() => addElement(item.create())}>
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
                    <div class="ve-row" style="align-items:center;gap:.5rem;margin-top:.25rem">
                      <label class="ve-prop-label" style="min-width:0">Show agent label</label>
                      <input type="checkbox" checked={el.showAgentLabel !== false}
                             on:change={e => setProp(i,"showAgentLabel", e.target.checked ? undefined : false)}>
                      <label class="ve-prop-label" style="min-width:0;margin-left:.5rem">Show user label</label>
                      <input type="checkbox" checked={el.showUserLabel !== false}
                             on:change={e => setProp(i,"showUserLabel", e.target.checked ? undefined : false)}>
                    </div>
                    <div class="ve-row" style="gap:.5rem;margin-top:.1rem">
                      <label class="ve-prop-label">Agent bg</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(el.agentColor ?? '#e8f4fd').hex}
                             on:input={e => setProp(i,"agentColor",e.target.value)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">User bg</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(el.userColor ?? '#eafbe8').hex}
                             on:input={e => setProp(i,"userColor",e.target.value)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">System bg</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(el.systemColor ?? '#f5f5f5').hex}
                             on:input={e => setProp(i,"systemColor",e.target.value)}>
                    </div>
                    <div class="ve-row" style="gap:.5rem;margin-top:.1rem">
                      <label class="ve-prop-label">Agent text</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(el.agentTextColor ?? '#000000').hex}
                             on:input={e => setProp(i,"agentTextColor",e.target.value === '#000000' ? undefined : e.target.value)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">User text</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(el.userTextColor ?? '#000000').hex}
                             on:input={e => setProp(i,"userTextColor",e.target.value === '#000000' ? undefined : e.target.value)}>
                    </div>
                    <div class="ve-row" style="gap:.5rem;margin-top:.1rem">
                      <label class="ve-prop-label">Font size</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="1rem"
                             value={el.fontSize ?? ""}
                             on:input={e => setProp(i,"fontSize",e.target.value || undefined)}>
                    </div>
                    <label class="ve-prop-label">Font</label>
                    <select class="ve-select"
                            value={fontOpts.some(f => f.v === (el.fontFamily ?? "")) ? (el.fontFamily ?? "") : "__custom__"}
                            on:change={e => setProp(i,"fontFamily", e.target.value === "__custom__" ? undefined : (e.target.value || undefined))}>
                      {#each fontOpts as f}<option value={f.v}>{f.label}</option>{/each}
                      {#if el.fontFamily && !fontOpts.some(f => f.v === el.fontFamily)}
                        <option value="__custom__">{el.fontFamily}</option>
                      {/if}
                    </select>
                    <div class="ve-row" style="align-items:center;gap:.5rem;margin-top:.25rem">
                      <label class="ve-prop-label" style="min-width:0">Show timestamps</label>
                      <input type="checkbox" checked={!!el.showTimestamps}
                             on:change={e => setProp(i,"showTimestamps",e.target.checked || undefined)}>
                    </div>
                    <div class="ve-media-hint">
                      Use <code>appendMessage(var='…', role='agent', text='…')</code> PlayAction to add messages at runtime.<br>
                      Roles: <code>agent</code> (left, tail) · <code>user</code> (right, tail) · <code>system</code> (center, italic)
                    </div>

                  <!-- ── Animate ── -->
                  {:else if el.type === "vsm-animate"}
                    {@const rateHints = { heartbeat:"BPM (e.g. 72)", breathe:"breaths/min (e.g. 15)", wave:"Hz (e.g. 4)", pulse:"Hz (e.g. 1)", spinner:"RPM (e.g. 60)" }}
                    <label class="ve-prop-label">Animation</label>
                    <select class="ve-select" value={el.animation ?? "heartbeat"}
                            on:change={e => setProp(i,"animation",e.target.value)}>
                      <option value="heartbeat">❤ Heartbeat</option>
                      <option value="breathe">○ Breathe</option>
                      <option value="pulse">◎ Pulse</option>
                      <option value="spinner">↻ Spinner</option>
                      <option value="wave">▋▋▋ Wave</option>
                    </select>
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Width</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="80px"
                             value={el.width ?? "80px"}
                             on:input={e => setProp(i,"width",e.target.value || undefined)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="80px"
                             value={el.height ?? "80px"}
                             on:input={e => setProp(i,"height",e.target.value || undefined)}>
                    </div>
                    <label class="ve-prop-label">Default color</label>
                    <input class="ve-color" type="color"
                           value={parseColorAlpha(el.color ?? '#e26d5a').hex}
                           on:input={e => setProp(i,"color",e.target.value)}>
                    <label class="ve-prop-label">Rate variable <span class="ve-hint">{rateHints[el.animation ?? "heartbeat"] ?? ""}</span></label>
                    <select class="ve-select" value={el.rateVar ?? ""}
                            on:change={e => setProp(i,"rateVar",e.target.value || undefined)}>
                      <option value="">— none (use default) —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <label class="ve-prop-label">Color variable <span class="ve-hint">(overrides default color)</span></label>
                    <select class="ve-select" value={el.colorVar ?? ""}
                            on:change={e => setProp(i,"colorVar",e.target.value || undefined)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    {#if (el.animation ?? "heartbeat") === "breathe"}
                    <label class="ve-prop-label">Amplitude variable <span class="ve-hint">(0–100, controls expansion)</span></label>
                    <select class="ve-select" value={el.amplitudeVar ?? ""}
                            on:change={e => setProp(i,"amplitudeVar",e.target.value || undefined)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    {/if}
                    <label class="ve-prop-label">Opacity variable <span class="ve-hint">(0–100)</span></label>
                    <select class="ve-select" value={el.opacityVar ?? ""}
                            on:change={e => setProp(i,"opacityVar",e.target.value || undefined)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  <!-- ── Chat Input ── -->
                  {:else if el.type === "vsm-chat-input"}
                    <label class="ve-prop-label">Sends to variable</label>
                    <select class="ve-select" value={el.sendsVar ?? ""}
                            on:change={e => setProp(i,"sendsVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <label class="ve-prop-label">Placeholder</label>
                    <input class="ve-input" type="text" placeholder="Type your message…"
                           value={el.placeholder ?? ""}
                           on:input={e => setProp(i,"placeholder",e.target.value || undefined)}>
                    <label class="ve-prop-label">Button label</label>
                    <input class="ve-input" type="text" placeholder="Send"
                           value={el.buttonLabel ?? ""}
                           on:input={e => setProp(i,"buttonLabel",e.target.value || undefined)}>
                    <label class="ve-prop-label">Disabled variable <span class="ve-hint">(Bool — disables input when true)</span></label>
                    <select class="ve-select" value={el.disabledVar ?? ""}
                            on:change={e => setProp(i,"disabledVar",e.target.value || undefined)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <div class="ve-media-hint">
                      User types a message and presses Enter or clicks the button.<br>
                      The text is sent to the selected variable and the field is cleared.
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
                        <button class="ve-add-btn ve-add-btn-animate" on:click={() => addChild(i,{type:"vsm-animate",animation:"heartbeat",color:"#e26d5a",width:"80px",height:"80px"})}>+Animate</button>
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

                          {:else if child.type === "vsm-animate"}
                            <label class="ve-prop-label">Animation</label>
                            <select class="ve-select" value={child.animation ?? "heartbeat"}
                                    on:change={e => setChildProp(i,ci,"animation",e.target.value)}>
                              <option value="heartbeat">❤ Heartbeat</option>
                              <option value="breathe">○ Breathe</option>
                              <option value="pulse">◎ Pulse</option>
                              <option value="spinner">↻ Spinner</option>
                              <option value="wave">▋▋▋ Wave</option>
                            </select>
                            <div class="ve-row" style="gap:.5rem">
                              <label class="ve-prop-label">Width</label>
                              <input class="ve-input ve-input-short" type="text" placeholder="80px"
                                     value={child.width ?? "80px"}
                                     on:input={e => setChildProp(i,ci,"width",e.target.value || undefined)}>
                              <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                              <input class="ve-input ve-input-short" type="text" placeholder="80px"
                                     value={child.height ?? "80px"}
                                     on:input={e => setChildProp(i,ci,"height",e.target.value || undefined)}>
                            </div>
                            <label class="ve-prop-label">Default color</label>
                            <input class="ve-color" type="color"
                                   value={parseColorAlpha(child.color ?? '#e26d5a').hex}
                                   on:input={e => setChildProp(i,ci,"color",e.target.value)}>
                            <label class="ve-prop-label">Rate variable</label>
                            <select class="ve-select" value={child.rateVar ?? ""}
                                    on:change={e => setChildProp(i,ci,"rateVar",e.target.value || undefined)}>
                              <option value="">— none —</option>
                              {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                            </select>
                            <label class="ve-prop-label">Color variable</label>
                            <select class="ve-select" value={child.colorVar ?? ""}
                                    on:change={e => setChildProp(i,ci,"colorVar",e.target.value || undefined)}>
                              <option value="">— none —</option>
                              {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                            </select>

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
                            <div class="ve-row" style="align-items:center;gap:.5rem;margin-top:.25rem">
                              <label class="ve-prop-label" style="min-width:0">Show agent label</label>
                              <input type="checkbox" checked={child.showAgentLabel !== false}
                                     on:change={e => setChildProp(i,ci,"showAgentLabel", e.target.checked ? undefined : false)}>
                              <label class="ve-prop-label" style="min-width:0;margin-left:.5rem">Show user label</label>
                              <input type="checkbox" checked={child.showUserLabel !== false}
                                     on:change={e => setChildProp(i,ci,"showUserLabel", e.target.checked ? undefined : false)}>
                            </div>
                            <div class="ve-row" style="gap:.5rem;margin-top:.1rem">
                              <label class="ve-prop-label">Agent bg</label>
                              <input class="ve-color" type="color"
                                     value={parseColorAlpha(child.agentColor ?? '#e8f4fd').hex}
                                     on:input={e => setChildProp(i,ci,"agentColor",e.target.value)}>
                              <label class="ve-prop-label" style="margin-left:.5rem">User bg</label>
                              <input class="ve-color" type="color"
                                     value={parseColorAlpha(child.userColor ?? '#eafbe8').hex}
                                     on:input={e => setChildProp(i,ci,"userColor",e.target.value)}>
                              <label class="ve-prop-label" style="margin-left:.5rem">System bg</label>
                              <input class="ve-color" type="color"
                                     value={parseColorAlpha(child.systemColor ?? '#f5f5f5').hex}
                                     on:input={e => setChildProp(i,ci,"systemColor",e.target.value)}>
                            </div>
                            <div class="ve-row" style="gap:.5rem;margin-top:.1rem">
                              <label class="ve-prop-label">Agent text</label>
                              <input class="ve-color" type="color"
                                     value={parseColorAlpha(child.agentTextColor ?? '#000000').hex}
                                     on:input={e => setChildProp(i,ci,"agentTextColor",e.target.value === '#000000' ? undefined : e.target.value)}>
                              <label class="ve-prop-label" style="margin-left:.5rem">User text</label>
                              <input class="ve-color" type="color"
                                     value={parseColorAlpha(child.userTextColor ?? '#000000').hex}
                                     on:input={e => setChildProp(i,ci,"userTextColor",e.target.value === '#000000' ? undefined : e.target.value)}>
                            </div>
                            <div class="ve-row" style="gap:.5rem;margin-top:.1rem">
                              <label class="ve-prop-label">Font size</label>
                              <input class="ve-input ve-input-short" type="text" placeholder="1rem"
                                     value={child.fontSize ?? ""}
                                     on:input={e => setChildProp(i,ci,"fontSize",e.target.value || undefined)}>
                            </div>
                            <label class="ve-prop-label">Font</label>
                            <select class="ve-select"
                                    value={fontOpts.some(f => f.v === (child.fontFamily ?? "")) ? (child.fontFamily ?? "") : "__custom__"}
                                    on:change={e => setChildProp(i,ci,"fontFamily", e.target.value === "__custom__" ? undefined : (e.target.value || undefined))}>
                              {#each fontOpts as f}<option value={f.v}>{f.label}</option>{/each}
                              {#if child.fontFamily && !fontOpts.some(f => f.v === child.fontFamily)}
                                <option value="__custom__">{child.fontFamily}</option>
                              {/if}
                            </select>

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
  .ve-row { display: flex; align-items: center; gap: 0.5rem; margin-bottom: 0.35rem; flex-wrap: wrap; }
  .ve-row:last-child { margin-bottom: 0; }
  .ve-label { font-size: 0.78rem; color: var(--ide-muted); min-width: 72px; flex-shrink: 0; }

  .ve-color {
    width: 2.2rem; height: 1.7rem; padding: 0.1rem; border-radius: 4px;
    border: 1px solid var(--ide-border-md); cursor: pointer; background: none;
  }
  .ve-opacity {
    width: 3.4rem; padding: 0.2rem 0.3rem; border-radius: 4px;
    border: 1px solid var(--ide-border-md); background: var(--ide-surface);
    color: var(--ide-text); font-size: 0.82rem; text-align: right;
  }
  .ve-opacity-unit { font-size: 0.78rem; color: var(--ide-dim); }
  .ve-media-hint {
    font-size: 0.75rem; color: var(--ide-muted); margin-bottom: .4rem;
    padding: .3rem .5rem; background: var(--ide-surface);
    border: 1px solid var(--ide-border); border-radius: 5px; line-height: 1.5;
  }
  .ve-media-hint code { font-size: 0.72rem; color: var(--ide-accent); font-family: 'DM Mono', monospace; }
  .ve-select {
    flex: 1; padding: 0.22rem 0.4rem;
    border: 1px solid var(--ide-border-md); border-radius: 5px;
    background: var(--ide-surface); color: var(--ide-text);
    font-size: 0.82rem; font-family: inherit;
  }

  .ve-align-row { display: flex; gap: 0.2rem; flex-wrap: wrap; }
  .ve-align-btn {
    padding: 0.15rem 0.42rem; border-radius: 4px;
    border: 1px solid var(--ide-border-md);
    background: var(--ide-surface); color: var(--ide-muted);
    font-size: 0.72rem; font-family: inherit; cursor: pointer; white-space: nowrap;
    transition: background 0.1s, color 0.1s;
  }
  .ve-align-btn:hover { color: var(--ide-text); background: rgba(0,0,0,0.05); }
  .ve-align-active { background: var(--ide-accent) !important; color: #fff !important; border-color: transparent; }

  .ve-elements-header {
    display: flex; align-items: center;
    padding: 0.5rem 0.8rem;
    background: var(--ide-panel);
    border-bottom: 1px solid var(--ide-border);
    flex-shrink: 0;
  }

  /* Element cards */
  .ve-card { background: var(--ide-panel); border-bottom: 1px solid var(--ide-border); }
  .ve-card-header {
    display: flex; align-items: center; gap: 0.4rem;
    padding: 0.38rem 0.6rem; cursor: pointer; user-select: none;
    transition: background 0.1s;
  }
  .ve-card-header:hover { background: rgba(0,0,0,0.03); }
  .ve-card-expanded > .ve-card-header { background: var(--ide-glow); }

  .ve-card-arrows { display: flex; flex-direction: column; gap: 0; }
  .ve-arrow {
    padding: 1px 3px; border: none; background: none; cursor: pointer;
    font-size: 0.72rem; line-height: 1; color: var(--ide-muted); border-radius: 3px;
    transition: color 0.1s;
  }
  .ve-arrow:hover:not(:disabled) { color: var(--ide-text); background: rgba(0,0,0,0.06); }
  .ve-arrow:disabled { opacity: 0.12; cursor: default; }

  .ve-type-badge {
    width: 1.55rem; height: 1.55rem; border-radius: 4px; flex-shrink: 0;
    background: var(--ide-glow); border: 1px solid rgba(56,139,253,0.2);
    color: var(--ide-accent);
    font-size: 0.65rem; font-weight: 700; font-family: 'DM Mono', monospace;
    display: flex; align-items: center; justify-content: center;
  }
  .ve-type-badge-sm { width: 1.3rem; height: 1.3rem; font-size: 0.58rem; }

  .ve-card-summary {
    flex: 1; min-width: 0; font-size: 0.8rem;
    overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
    color: var(--ide-muted);
  }
  .ve-expand-icon { font-size: 0.72rem; color: var(--ide-dim); flex-shrink: 0; }
  .ve-delete {
    border: none; background: none; cursor: pointer;
    font-size: 0.95rem; padding: 0 0.15rem;
    color: var(--ide-danger); opacity: 0.22; flex-shrink: 0;
    transition: opacity 0.12s;
  }
  .ve-delete:hover { opacity: 1; }

  /* Properties */
  .ve-props {
    padding: 0.6rem 0.8rem 0.7rem;
    border-top: 1px solid var(--ide-border);
    background: var(--ide-surface);
    display: flex; flex-direction: column; gap: 0.32rem;
  }
  .ve-prop-label { font-size: 0.72rem; color: var(--ide-muted); margin-top: 0.1rem; }
  .ve-input {
    flex: 1;
    border: 1px solid var(--ide-border-md); border-radius: 5px;
    background: var(--ide-panel); color: var(--ide-text);
    font-size: 0.82rem; font-family: inherit; width: 100%; box-sizing: border-box;
    padding: 0.22rem 0.45rem;
  }
  .ve-input-short { width: 5rem; flex-shrink: 0; }
  .ve-textarea {
    padding: 0.3rem 0.45rem; resize: vertical;
    border: 1px solid var(--ide-border-md); border-radius: 5px;
    background: var(--ide-panel); color: var(--ide-text);
    font-size: 0.82rem; font-family: inherit; width: 100%; box-sizing: border-box;
  }
  .ve-row-trio { display: flex; gap: 0.4rem; }
  .ve-row-trio > div { flex: 1; display: flex; flex-direction: column; gap: 0.2rem; }

  /* Panel children */
  .ve-children-bar {
    display: flex; align-items: center; justify-content: space-between;
    flex-wrap: wrap; gap: 0.3rem;
    padding: 0.4rem 0; margin-top: 0.3rem;
    border-top: 1px solid var(--ide-border);
  }
  .ve-add-group { display: flex; flex-wrap: wrap; gap: 0.2rem; }
  .ve-add-btn {
    padding: 0.17rem 0.42rem;
    border: 1px solid var(--ide-border); border-radius: 4px;
    background: var(--ide-surface); color: var(--ide-muted);
    font-size: 0.7rem; font-family: inherit; cursor: pointer;
    transition: background 0.1s, color 0.1s;
  }
  .ve-add-btn:hover { background: var(--ide-glow); color: var(--ide-accent); }
  .ve-add-btn-feed    { color: var(--ide-success); border-color: rgba(63,185,80,0.3); }
  .ve-add-btn-animate { color: var(--ide-danger);  border-color: rgba(248,81,73,0.3); }

  .ve-child-card { border: 1px solid var(--ide-border); border-radius: 5px; margin-top: 0.3rem; overflow: hidden; }
  .ve-child-header {
    display: flex; align-items: center; gap: 0.35rem;
    padding: 0.3rem 0.5rem; cursor: pointer; user-select: none;
    background: var(--ide-panel); transition: background 0.1s;
  }
  .ve-child-header:hover { background: rgba(0,0,0,0.03); }
  .ve-child-props {
    padding: 0.5rem 0.6rem; background: var(--ide-surface);
    border-top: 1px solid var(--ide-border);
    display: flex; flex-direction: column; gap: 0.3rem;
  }

  .ve-hint { font-size: 0.68rem; color: var(--ide-dim); font-weight: 400; }
  .ve-unknown { font-size: 0.8rem; color: var(--ide-muted); margin: 0; }
  .ve-unknown code { background: var(--ide-panel); padding: 0 0.2rem; border-radius: 3px; font-family: 'DM Mono', monospace; }
  .ve-empty { padding: 0.8rem; font-size: 0.8rem; color: var(--ide-dim); text-align: center; }
  .ve-empty-full { flex: 1; display: flex; align-items: center; justify-content: center; }

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
