<script>
  /**
   * Recursive row for one element in a screen's tree, used uniformly at every depth — the
   * top level and every level of nesting below it. A vsm-panel here gets full editing
   * (background, layout, alignment, add-child) plus a nested list of ScreenElementRow for its
   * own children, recursing to any depth. Every other type's fields come from
   * ScreenElementProperties, the same component regardless of depth — one implementation of
   * "how do you edit an sl-button", not one per depth.
   *
   * `elements` is always the screen's whole root array, the same reference at every level of
   * recursion — operations below address it via `path` (this row's own path) using the pure
   * tree functions from ./screenTree.js, so a change three levels down never needs its
   * ancestors to be threaded through as separate callbacks.
   *
   * `expandedPaths` is a Set of path keys (see pathKey below), not a single value: several
   * rows, at any depths, can be expanded at once, independent of each other. That
   * independence is load-bearing, not a style choice — collapsing this into one exclusive
   * "selected path" (tried first) meant expanding a child necessarily un-expanded its own
   * parent, which unmounted the child's own row (children only render while their parent is
   * expanded). The other failure mode of getting this wrong runs the opposite direction:
   * gating a panel's children on "is this row itself expanded" at every depth, with nothing
   * to turn OFF a level, made the whole tree permanently fully unrolled the moment it uses
   * this component from the root — every panel's fields and every descendant, all shown at
   * once, with no way to collapse anything. A plain per-path membership test avoids both:
   * expanding one row is independent of every other row's own state.
   */
  import {
    getAtPath, getContainerAtPath, setPropAtPath, setStylePropAtPath,
    removeAtPath, insertChildAtPath, moveAtPath, pathKey,
  } from "./screenTree.js";
  import ScreenElementProperties from "./ScreenElementProperties.svelte";

  export let elements;
  export let path;
  export let expandedPaths;      // Set<string> — see pathKey()
  export let onToggle;           // (path) => void — flips this path's membership
  export let onChange;

  // Passed down from ScreenEditor.svelte rather than reimplemented here, so the icon/summary,
  // alignment-option and property-field logic stays in exactly one place.
  export let typeLabel;
  export let elementSummary;
  export let alignItemsOpts;
  export let justifyOpts;
  export let textAlignOpts;
  export let fontOpts;
  export let variables;
  export let parseColorAlpha;
  export let buildColorAlpha;
  export let optionsToText;
  export let textToOptions;
  export let BUTTON_ICONS;
  export let ICON_SVG;

  // Mirrors the top-level "Panel children" bar's palette, plus +Panel (which that bar also
  // gained once nesting became possible — see the previous phase).
  const CHILD_TYPES = [
    { label: "+Text",       create: () => ({ type: "sl-text", content: "Text" }) },
    { label: "+Button",     create: () => ({ type: "sl-button", label: "Button", sendsVar: "", sendsValue: "" }) },
    { label: "+Slider",     create: () => ({ type: "sl-range", label: "Slider", min: 0, max: 100, step: 1 }) },
    { label: "+Input",      create: () => ({ type: "sl-input", label: "Input", bindVar: "" }) },
    { label: "+Select",     create: () => ({ type: "sl-select", label: "Select", options: ["Option 1", "Option 2"], bindVar: "" }) },
    { label: "+Check",      create: () => ({ type: "sl-checkbox", label: "Checkbox", bindVar: "" }) },
    { label: "+Filler",     create: () => ({ type: "vsm-filler", flexGrow: true }) },
    { label: "+Image",      create: () => ({ type: "vsm-image", src: "", alt: "" }) },
    { label: "+Video",      create: () => ({ type: "vsm-video", src: "", controls: true }) },
    { label: "+Audio",      create: () => ({ type: "vsm-audio", src: "", controls: true }) },
    { label: "+Embed",      create: () => ({ type: "vsm-embed", src: "", width: "100%", height: "315px" }) },
    { label: "+Bubble",     create: () => ({ type: "vsm-bubble", content: "Hello!", tail: "bottom", background: "#e8f4fd" }) },
    { label: "+Chart",      create: () => ({ type: "vsm-chart", chartType: "bar", dataVar: "", label: "", color: "#5b8edc", height: "300px" }) },
    { label: "+Feed",       create: () => ({ type: "vsm-feed", dataVar: "", height: "400px", agentColor: "#e8f4fd", userColor: "#eafbe8", systemColor: "#f5f5f5", agentLabel: "Agent", userLabel: "You" }) },
    { label: "+Animate",    create: () => ({ type: "vsm-animate", animation: "heartbeat", color: "#e26d5a", width: "80px", height: "80px" }) },
    { label: "+Chat Input", create: () => ({ type: "vsm-chat-input", sendsVar: "", placeholder: "Type your message…" }) },
    { label: "+Panel",      create: () => ({ type: "vsm-panel", children: [] }) },
  ];

  $: element = getAtPath(elements, path);
  $: children = element?.children ?? [];
  $: container = getContainerAtPath(elements, path);
  $: index = path[path.length - 1];
  $: isFirst = index === 0;
  $: isLast = !container || index === container.length - 1;
  $: isExpanded = expandedPaths.has(pathKey(path));
  $: panelIsRow = (element?.layout ?? "flex-column") === "flex-row";

  // Depth-scaled indent is the only thing that makes "how deeply nested am I" readable once
  // a screen has more than one level of panels — every card looks identical otherwise, and
  // the ambient padding a parent's own .ve-props box contributes turned out, in practice, not
  // to read as indentation at all (confirmed against a real 3-level-deep screen).
  $: depth = path.length;
  // Ancestors only, nearest first is wrong for reading order — outermost first, like a
  // filesystem path. elementSummary() is the same text each row's own header already shows,
  // so a breadcrumb built from it stays consistent with what you'd see by walking up manually.
  $: breadcrumb = depth > 1
    ? path.slice(0, -1).map((_, i) => elementSummary(getAtPath(elements, path.slice(0, i + 1))))
    : [];

  function setProp(key, value) { onChange(setPropAtPath(elements, path, key, value)); }
  function setStyleProp(key, value) { onChange(setStylePropAtPath(elements, path, key, value)); }
  function move(direction) { onChange(moveAtPath(elements, path, direction)); }
  function addChild(create) { onChange(insertChildAtPath(elements, path, create())); }

  function remove() {
    onChange(removeAtPath(elements, path));
    if (isExpanded) onToggle(path);
  }

  function toggle() { onToggle(path); }
</script>

{#if element}
  <div class="ve-card ve-card-deep" class:ve-card-expanded={isExpanded}
       class:ve-card-nested={depth > 1}
       style={depth > 1 ? `margin-left: ${(depth - 1) * 0.85}rem` : ""}>
    <div class="ve-card-header" role="button" tabindex="0"
         on:click={toggle} on:keydown={(e) => e.key === "Enter" && toggle()}>
      <div class="ve-card-arrows">
        <button class="ve-arrow" disabled={isFirst} on:click|stopPropagation={() => move(-1)}>▲</button>
        <button class="ve-arrow" disabled={isLast} on:click|stopPropagation={() => move(1)}>▼</button>
      </div>
      <span class="ve-type-badge">{typeLabel(element.type)}</span>
      <span class="ve-card-summary">{elementSummary(element)}</span>
      <span class="ve-expand-icon">{isExpanded ? "▾" : "▸"}</span>
      <button class="ve-delete" on:click|stopPropagation={remove}>×</button>
    </div>

    {#if isExpanded}
      <div class="ve-props">
        {#if breadcrumb.length > 0}
          <div class="ve-breadcrumb">{breadcrumb.join(" › ")}</div>
        {/if}
        {#if element.type === "vsm-panel"}
          <div class="ve-row">
            <label class="ve-label">Background</label>
            <input class="ve-color" type="color"
                   value={parseColorAlpha(element.background ?? "#f5f5f5").hex}
                   on:input={(e) => setProp("background",
                     buildColorAlpha(e.target.value, parseColorAlpha(element.background ?? "#f5f5f5").opacity))}>
            <input class="ve-opacity" type="number" min="0" max="100"
                   value={parseColorAlpha(element.background ?? "#f5f5f5").opacity}
                   on:input={(e) => setProp("background",
                     buildColorAlpha(parseColorAlpha(element.background ?? "#f5f5f5").hex, e.target.value))}>
            <span class="ve-opacity-unit">%</span>
            <label class="ve-label" style="margin-left:.5rem">Padding</label>
            <input class="ve-input ve-input-short" type="text" placeholder="1rem"
                   value={element.padding ?? ""}
                   on:input={(e) => setProp("padding", e.target.value)}>
          </div>
          <div class="ve-row">
            <label class="ve-label">Layout</label>
            <select class="ve-select" value={element.layout ?? "flex-column"}
                    on:change={(e) => setProp("layout", e.target.value)}>
              <option value="flex-column">Column</option>
              <option value="flex-row">Row</option>
            </select>
          </div>
          <label class="ve-prop-label">Horizontal</label>
          <div class="ve-align-row">
            {#each (panelIsRow ? justifyOpts : alignItemsOpts) as opt}
              <button class="ve-align-btn"
                      class:ve-align-active={element.alignItems === opt.v}
                      on:click={() => setProp("alignItems", element.alignItems === opt.v ? undefined : opt.v)}
                      title={opt.label}>{opt.label}</button>
            {/each}
          </div>
          <label class="ve-prop-label">Vertical</label>
          <div class="ve-align-row">
            {#each (panelIsRow ? alignItemsOpts : justifyOpts) as opt}
              <button class="ve-align-btn"
                      class:ve-align-active={element.justifyContent === opt.v}
                      on:click={() => setProp("justifyContent", element.justifyContent === opt.v ? undefined : opt.v)}
                      title={opt.label}>{opt.label}</button>
            {/each}
          </div>
          <div class="ve-row" style="align-items:center;gap:.5rem;margin-top:.25rem">
            <label class="ve-prop-label" style="min-width:0">Grow to fill</label>
            <input type="checkbox" checked={element.flexGrow ?? false}
                   on:change={(e) => setProp("flexGrow", e.target.checked || undefined)}>
            <span class="ve-hint">claims remaining space in layout</span>
          </div>

          <div class="ve-children-bar">
            <span class="ve-prop-label">Children</span>
            <div class="ve-add-group">
              {#each CHILD_TYPES as ct}
                <button class="ve-add-btn" on:click={() => addChild(ct.create)}>{ct.label}</button>
              {/each}
            </div>
          </div>

          {#if children.length === 0}
            <div class="ve-empty">No children yet.</div>
          {/if}

          {#each children as child, ci (ci)}
            <svelte:self
              {elements} path={[...path, ci]} {expandedPaths} {onToggle} {onChange}
              {typeLabel} {elementSummary} {alignItemsOpts} {justifyOpts} {textAlignOpts} {fontOpts}
              {variables} {parseColorAlpha} {buildColorAlpha} {optionsToText} {textToOptions}
              {BUTTON_ICONS} {ICON_SVG} />
          {/each}
        {:else}
          <ScreenElementProperties
            {element} onSetProp={setProp} onSetStyleProp={setStyleProp}
            {variables} {alignItemsOpts} {textAlignOpts} {fontOpts}
            {parseColorAlpha} {buildColorAlpha} {optionsToText} {textToOptions}
            {BUTTON_ICONS} {ICON_SVG} />
        {/if}
      </div>
    {/if}
  </div>
{/if}
