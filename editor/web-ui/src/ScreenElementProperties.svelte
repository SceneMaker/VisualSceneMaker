<script>
  /**
   * Type-specific property fields, shared by every place an element can be edited
   * (the top-level list, one level of panel children, and ScreenElementRow's arbitrary-depth
   * recursion) so there is exactly one implementation of "how do you edit an sl-button" instead
   * of the same fields copy-pasted at every depth. A vsm-panel is never passed in here — its
   * fields (background/layout/alignment/children) are ScreenElementRow's job, since a panel
   * also needs to recurse into its children, which this component has no path/tree access to do.
   */
  export let element;
  export let onSetProp;
  export let onSetStyleProp;
  export let variables;
  export let alignItemsOpts;
  export let textAlignOpts;
  export let fontOpts;
  export let parseColorAlpha;
  export let buildColorAlpha;
  export let optionsToText;
  export let textToOptions;
  export let BUTTON_ICONS;
  export let ICON_SVG;
</script>

<!-- Align-self (common to every non-panel element) -->
<label class="ve-prop-label">Align in layout</label>
<div class="ve-align-row">
  {#each alignItemsOpts as opt}
    <button class="ve-align-btn"
            class:ve-align-active={element.style?.["align-self"] === opt.v}
            on:click={() => onSetStyleProp("align-self",
              element.style?.["align-self"] === opt.v ? "" : opt.v)}
            title={opt.label}>{opt.label}</button>
  {/each}
</div>

                  {#if element.type === "sl-text" || element.type === "wa-text"}
                    <label class="ve-prop-label">Content</label>
                    <textarea class="ve-textarea" rows="2"
                              value={element.content ?? ""}
                              on:input={e => onSetProp("content",e.target.value)}></textarea>
                    <label class="ve-prop-label">Text color</label>
                    <input class="ve-color" type="color"
                           value={parseColorAlpha(element.style?.color ?? '#000000').hex}
                           on:input={e => onSetStyleProp("color",
                             buildColorAlpha(e.target.value, parseColorAlpha(element.style?.color ?? '#000000').opacity))}>
                    <input class="ve-opacity" type="number" min="0" max="100"
                           value={parseColorAlpha(element.style?.color ?? '#000000').opacity}
                           on:input={e => onSetStyleProp("color",
                             buildColorAlpha(parseColorAlpha(element.style?.color ?? '#000000').hex, e.target.value))}>
                    <span class="ve-opacity-unit">%</span>
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Font size</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="1rem"
                             value={element.style?.["font-size"] ?? ""}
                             on:input={e => onSetStyleProp("font-size",e.target.value || undefined)}>
                    </div>
                    <label class="ve-prop-label">Font</label>
                    <select class="ve-select"
                            value={fontOpts.some(f => f.v === (element.style?.["font-family"] ?? "")) ? (element.style?.["font-family"] ?? "") : "__custom__"}
                            on:change={e => onSetStyleProp("font-family", e.target.value === "__custom__" ? undefined : (e.target.value || undefined))}>
                      {#each fontOpts as f}<option value={f.v}>{f.label}</option>{/each}
                      {#if element.style?.["font-family"] && !fontOpts.some(f => f.v === element.style?.["font-family"])}
                        <option value="__custom__">{element.style["font-family"]}</option>
                      {/if}
                    </select>
                    <label class="ve-prop-label">Style</label>
                    <div class="ve-align-row">
                      <button class="ve-align-btn"
                              class:ve-align-active={element.style?.["font-weight"] === "bold"}
                              on:click={() => onSetStyleProp("font-weight",
                                element.style?.["font-weight"] === "bold" ? "" : "bold")}><b>B</b></button>
                      <button class="ve-align-btn"
                              class:ve-align-active={element.style?.["font-style"] === "italic"}
                              on:click={() => onSetStyleProp("font-style",
                                element.style?.["font-style"] === "italic" ? "" : "italic")}><i>I</i></button>
                      {#each textAlignOpts as opt}
                        <button class="ve-align-btn"
                                class:ve-align-active={element.style?.["text-align"] === opt.v}
                                on:click={() => onSetStyleProp("text-align",
                                  element.style?.["text-align"] === opt.v ? "" : opt.v)}
                                title={opt.label}>{opt.label}</button>
                      {/each}
                    </div>

                  {:else if element.type === "sl-button" || element.type === "wa-button"}
                    <label class="ve-prop-label">Label</label>
                    <input class="ve-input" type="text" value={element.label ?? ""}
                           on:input={e => onSetProp("label",e.target.value)}>
                    <label class="ve-prop-label">Icon <span class="ve-hint">(leave blank to use only the label)</span></label>
                    <div class="ve-row" style="gap:.5rem">
                      <select class="ve-select" value={element.icon ?? ""}
                              on:change={e => onSetProp("icon",e.target.value || undefined)}>
                        {#each BUTTON_ICONS as opt}<option value={opt.v}>{opt.label}</option>{/each}
                      </select>
                      {#if element.icon && ICON_SVG[element.icon]}
                        <span class="ve-icon-preview">{@html ICON_SVG[element.icon]}</span>
                      {/if}
                    </div>
                    <label class="ve-prop-label">Variant</label>
                    <select class="ve-select" value={element.variant ?? "default"}
                            on:change={e => onSetProp("variant",e.target.value)}>
                      <option value="default">Default</option>
                      <option value="primary">Primary</option>
                      <option value="success">Success</option>
                      <option value="warning">Warning</option>
                      <option value="danger">Danger</option>
                    </select>
                    <label class="ve-prop-label">Toggle variable <span class="ve-hint">(Bool — turns this into a two-state button: green/red follows this variable, and clicking flips it. Overrides Variant and Sends-to-variable below when set — e.g. for a mic mute toggle a flow action can also drive with an updateVar command)</span></label>
                    <select class="ve-select" value={element.toggleVar ?? ""}
                            on:change={e => onSetProp("toggleVar",e.target.value || undefined)}>
                      <option value="">— none, use Variant / Sends-to-variable below —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <label class="ve-prop-label">Sends to variable</label>
                    <select class="ve-select" value={element.sendsVar ?? ""}
                            on:change={e => onSetProp("sendsVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <label class="ve-prop-label">Value to send</label>
                    <input class="ve-input" type="text" value={element.sendsValue ?? ""}
                           on:input={e => onSetProp("sendsValue",e.target.value)}>

                  {:else if element.type === "sl-range" || element.type === "wa-range"}
                    <label class="ve-prop-label">Label</label>
                    <input class="ve-input" type="text" value={element.label ?? ""}
                           on:input={e => onSetProp("label",e.target.value)}>
                    <div class="ve-row-trio">
                      <div><label class="ve-prop-label">Min</label>
                        <input class="ve-input" type="number" value={element.min ?? 0}
                               on:input={e => onSetProp("min",+e.target.value)}></div>
                      <div><label class="ve-prop-label">Max</label>
                        <input class="ve-input" type="number" value={element.max ?? 100}
                               on:input={e => onSetProp("max",+e.target.value)}></div>
                      <div><label class="ve-prop-label">Step</label>
                        <input class="ve-input" type="number" value={element.step ?? 1}
                               on:input={e => onSetProp("step",+e.target.value)}></div>
                    </div>
                    <label class="ve-prop-label">Binds to variable</label>
                    <select class="ve-select" value={element.bindVar ?? ""}
                            on:change={e => onSetProp("bindVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  {:else if element.type === "sl-input" || element.type === "wa-input"}
                    <label class="ve-prop-label">Label</label>
                    <input class="ve-input" type="text" value={element.label ?? ""}
                           on:input={e => onSetProp("label",e.target.value)}>
                    <label class="ve-prop-label">Placeholder</label>
                    <input class="ve-input" type="text" value={element.placeholder ?? ""}
                           on:input={e => onSetProp("placeholder",e.target.value)}>
                    <label class="ve-prop-label">Binds to variable</label>
                    <select class="ve-select" value={element.bindVar ?? ""}
                            on:change={e => onSetProp("bindVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  {:else if element.type === "sl-textarea" || element.type === "wa-textarea"}
                    <label class="ve-prop-label">Label</label>
                    <input class="ve-input" type="text" value={element.label ?? ""}
                           on:input={e => onSetProp("label",e.target.value)}>
                    <label class="ve-prop-label">Placeholder</label>
                    <input class="ve-input" type="text" value={element.placeholder ?? ""}
                           on:input={e => onSetProp("placeholder",e.target.value)}>
                    <label class="ve-prop-label">Binds to variable</label>
                    <select class="ve-select" value={element.bindVar ?? ""}
                            on:change={e => onSetProp("bindVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  {:else if element.type === "sl-select" || element.type === "wa-select"}
                    <label class="ve-prop-label">Label</label>
                    <input class="ve-input" type="text" value={element.label ?? ""}
                           on:input={e => onSetProp("label",e.target.value)}>
                    <label class="ve-prop-label">Options <span class="ve-hint">(one per line, or value=Label)</span></label>
                    <textarea class="ve-textarea" rows="4"
                              value={optionsToText(element.options)}
                              on:change={e => onSetProp("options",textToOptions(e.target.value))}></textarea>
                    <label class="ve-prop-label">Binds to variable</label>
                    <select class="ve-select" value={element.bindVar ?? ""}
                            on:change={e => onSetProp("bindVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  {:else if element.type === "sl-checkbox" || element.type === "wa-checkbox"}
                    <label class="ve-prop-label">Label</label>
                    <input class="ve-input" type="text" value={element.label ?? ""}
                           on:input={e => onSetProp("label",e.target.value)}>
                    <label class="ve-prop-label">Binds to variable <span class="ve-hint">(stores true/false)</span></label>
                    <select class="ve-select" value={element.bindVar ?? ""}
                            on:change={e => onSetProp("bindVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  <!-- ── Image ── -->
                  {:else if element.type === "vsm-image"}
                    <div class="ve-media-hint">Place files in <code>screens-assets/</code> inside your project folder and use <code>/assets/filename.ext</code></div>
                    <label class="ve-prop-label">Source</label>
                    <input class="ve-input" type="text" placeholder="/assets/photo.jpg or https://…"
                           value={element.src ?? ""}
                           on:input={e => onSetProp("src",e.target.value)}>
                    <label class="ve-prop-label">Alt text</label>
                    <input class="ve-input" type="text" placeholder="Description for accessibility"
                           value={element.alt ?? ""}
                           on:input={e => onSetProp("alt",e.target.value)}>
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Width</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="100%"
                             value={element.width ?? ""}
                             on:input={e => onSetProp("width",e.target.value || undefined)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="auto"
                             value={element.height ?? ""}
                             on:input={e => onSetProp("height",e.target.value || undefined)}>
                    </div>
                    <label class="ve-prop-label">Object fit</label>
                    <select class="ve-select" value={element.objectFit ?? ""}
                            on:change={e => onSetProp("objectFit",e.target.value || undefined)}>
                      <option value="">— default —</option>
                      <option value="contain">Contain (show whole image)</option>
                      <option value="cover">Cover (fill box, crop)</option>
                      <option value="fill">Fill (stretch)</option>
                      <option value="none">None</option>
                    </select>

                  <!-- ── Video ── -->
                  {:else if element.type === "vsm-video"}
                    <div class="ve-media-hint">Place files in <code>screens-assets/</code> inside your project folder and use <code>/assets/filename.ext</code></div>
                    <label class="ve-prop-label">Source</label>
                    <input class="ve-input" type="text" placeholder="/assets/video.mp4 or https://…"
                           value={element.src ?? ""}
                           on:input={e => onSetProp("src",e.target.value)}>
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Width</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="100%"
                             value={element.width ?? ""}
                             on:input={e => onSetProp("width",e.target.value || undefined)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="auto"
                             value={element.height ?? ""}
                             on:input={e => onSetProp("height",e.target.value || undefined)}>
                    </div>
                    <div class="ve-row" style="gap:1rem;flex-wrap:wrap">
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={element.controls !== false}
                               on:change={e => onSetProp("controls",e.target.checked)}>
                        Controls</label>
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={!!element.autoplay}
                               on:change={e => onSetProp("autoplay",e.target.checked || undefined)}>
                        Autoplay</label>
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={!!element.loop}
                               on:change={e => onSetProp("loop",e.target.checked || undefined)}>
                        Loop</label>
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={!!element.muted}
                               on:change={e => onSetProp("muted",e.target.checked || undefined)}>
                        Muted <span class="ve-hint">(required for autoplay)</span></label>
                    </div>

                  <!-- ── Audio ── -->
                  {:else if element.type === "vsm-audio"}
                    <div class="ve-media-hint">Place files in <code>screens-assets/</code> inside your project folder and use <code>/assets/filename.ext</code></div>
                    <label class="ve-prop-label">Source</label>
                    <input class="ve-input" type="text" placeholder="/assets/sound.mp3 or https://…"
                           value={element.src ?? ""}
                           on:input={e => onSetProp("src",e.target.value)}>
                    <div class="ve-row" style="gap:1rem;flex-wrap:wrap">
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={element.controls !== false}
                               on:change={e => onSetProp("controls",e.target.checked)}>
                        Controls</label>
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={!!element.autoplay}
                               on:change={e => onSetProp("autoplay",e.target.checked || undefined)}>
                        Autoplay</label>
                      <label style="display:flex;align-items:center;gap:.3rem;font-size:.82rem">
                        <input type="checkbox" checked={!!element.loop}
                               on:change={e => onSetProp("loop",e.target.checked || undefined)}>
                        Loop</label>
                    </div>

                  <!-- ── Embed (YouTube / iframe) ── -->
                  {:else if element.type === "vsm-embed"}
                    <div class="ve-media-hint">For YouTube use the embed URL: <code>https://www.youtube.com/embed/VIDEO_ID</code></div>
                    <label class="ve-prop-label">Embed URL</label>
                    <input class="ve-input" type="text" placeholder="https://www.youtube.com/embed/…"
                           value={element.src ?? ""}
                           on:input={e => onSetProp("src",e.target.value)}>
                    <label class="ve-prop-label">Title <span class="ve-hint">(accessibility)</span></label>
                    <input class="ve-input" type="text" placeholder="Video title"
                           value={element.title ?? ""}
                           on:input={e => onSetProp("title",e.target.value || undefined)}>
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Width</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="100%"
                             value={element.width ?? "100%"}
                             on:input={e => onSetProp("width",e.target.value)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="315px"
                             value={element.height ?? "315px"}
                             on:input={e => onSetProp("height",e.target.value)}>
                    </div>

                  <!-- ── Filler ── -->
                  {:else if element.type === "vsm-filler"}
                    <div class="ve-row" style="align-items:center;gap:.5rem">
                      <label class="ve-prop-label" style="min-width:0">Flex grow</label>
                      <input type="checkbox" checked={element.flexGrow ?? false}
                             on:change={e => onSetProp("flexGrow", e.target.checked)}>
                      <span class="ve-hint" style="margin-left:.25rem">fills remaining space</span>
                    </div>
                    {#if !element.flexGrow}
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Width</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="e.g. 100%"
                             value={element.width ?? ""}
                             on:input={e => onSetProp("width", e.target.value || undefined)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="e.g. 2rem"
                             value={element.height ?? ""}
                             on:input={e => onSetProp("height", e.target.value || undefined)}>
                    </div>
                    {/if}

                  <!-- ── Speech Bubble ── -->
                  {:else if element.type === "vsm-bubble"}
                    <label class="ve-prop-label">Content</label>
                    <textarea class="ve-textarea" rows="2"
                              value={element.content ?? ""}
                              on:input={e => onSetProp("content",e.target.value)}></textarea>
                    <label class="ve-prop-label">Speaker name <span class="ve-hint">(optional label above bubble)</span></label>
                    <input class="ve-input" type="text" placeholder="Agent, User, …"
                           value={element.speaker ?? ""}
                           on:input={e => onSetProp("speaker",e.target.value || undefined)}>
                    <label class="ve-prop-label">Tail direction <span class="ve-hint">(left/right follows "Align in layout")</span></label>
                    <select class="ve-select" value={element.tail ?? "bottom"}
                            on:change={e => onSetProp("tail",e.target.value)}>
                      <option value="bottom">Bottom</option>
                      <option value="top">Top</option>
                      <option value="">None</option>
                    </select>
                    <label class="ve-prop-label">Background</label>
                    <input class="ve-color" type="color"
                           value={parseColorAlpha(element.background ?? '#e8f4fd').hex}
                           on:input={e => onSetProp("background",
                             buildColorAlpha(e.target.value, parseColorAlpha(element.background ?? '#e8f4fd').opacity))}>
                    <input class="ve-opacity" type="number" min="0" max="100"
                           value={parseColorAlpha(element.background ?? '#e8f4fd').opacity}
                           on:input={e => onSetProp("background",
                             buildColorAlpha(parseColorAlpha(element.background ?? '#e8f4fd').hex, e.target.value))}>
                    <span class="ve-opacity-unit">%</span>
                    <label class="ve-prop-label">Text color</label>
                    <input class="ve-color" type="color"
                           value={parseColorAlpha(element.style?.color ?? '#000000').hex}
                           on:input={e => onSetStyleProp("color",
                             buildColorAlpha(e.target.value, parseColorAlpha(element.style?.color ?? '#000000').opacity))}>
                    <input class="ve-opacity" type="number" min="0" max="100"
                           value={parseColorAlpha(element.style?.color ?? '#000000').opacity}
                           on:input={e => onSetStyleProp("color",
                             buildColorAlpha(parseColorAlpha(element.style?.color ?? '#000000').hex, e.target.value))}>
                    <span class="ve-opacity-unit">%</span>
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Font size</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="1rem"
                             value={element.style?.["font-size"] ?? ""}
                             on:input={e => onSetStyleProp("font-size",e.target.value || undefined)}>
                    </div>
                    <label class="ve-prop-label">Font</label>
                    <select class="ve-select"
                            value={fontOpts.some(f => f.v === (element.style?.["font-family"] ?? "")) ? (element.style?.["font-family"] ?? "") : "__custom__"}
                            on:change={e => onSetStyleProp("font-family", e.target.value === "__custom__" ? undefined : (e.target.value || undefined))}>
                      {#each fontOpts as f}<option value={f.v}>{f.label}</option>{/each}
                      {#if element.style?.["font-family"] && !fontOpts.some(f => f.v === element.style?.["font-family"])}
                        <option value="__custom__">{element.style["font-family"]}</option>
                      {/if}
                    </select>
                    <label class="ve-prop-label">Style</label>
                    <div class="ve-align-row">
                      <button class="ve-align-btn"
                              class:ve-align-active={element.style?.["font-weight"] === "bold"}
                              on:click={() => onSetStyleProp("font-weight",
                                element.style?.["font-weight"] === "bold" ? "" : "bold")}><b>B</b></button>
                      <button class="ve-align-btn"
                              class:ve-align-active={element.style?.["font-style"] === "italic"}
                              on:click={() => onSetStyleProp("font-style",
                                element.style?.["font-style"] === "italic" ? "" : "italic")}><i>I</i></button>
                      {#each textAlignOpts as opt}
                        <button class="ve-align-btn"
                                class:ve-align-active={element.style?.["text-align"] === opt.v}
                                on:click={() => onSetStyleProp("text-align",
                                  element.style?.["text-align"] === opt.v ? "" : opt.v)}
                                title={opt.label}>{opt.label}</button>
                      {/each}
                    </div>
                    <label class="ve-prop-label">Binds to variable <span class="ve-hint">(overrides content)</span></label>
                    <select class="ve-select" value={element.bindVar ?? ""}
                            on:change={e => onSetProp("bindVar",e.target.value || undefined)}>
                      <option value="">— static content —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  <!-- ── Chart ── -->
                  {:else if element.type === "vsm-chart"}
                    <label class="ve-prop-label">Chart type</label>
                    <select class="ve-select" value={element.chartType ?? "bar"}
                            on:change={e => onSetProp("chartType",e.target.value)}>
                      <option value="bar">Bar</option>
                      <option value="line">Line</option>
                    </select>
                    <label class="ve-prop-label">Data variable <span class="ve-hint">(holds JSON dataset)</span></label>
                    <select class="ve-select" value={element.dataVar ?? ""}
                            on:change={e => onSetProp("dataVar",e.target.value || undefined)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <label class="ve-prop-label">Dataset label</label>
                    <input class="ve-input" type="text" placeholder="My data"
                           value={element.label ?? ""}
                           on:input={e => onSetProp("label",e.target.value || undefined)}>
                    <label class="ve-prop-label">Color</label>
                    <input class="ve-color" type="color"
                           value={parseColorAlpha(element.color ?? '#5b8edc').hex}
                           on:input={e => onSetProp("color",e.target.value)}>
                    {#if (element.chartType ?? "bar") === "line"}
                    <div class="ve-row" style="align-items:center;gap:.5rem;margin-top:.25rem">
                      <label class="ve-prop-label" style="min-width:0">Fill area</label>
                      <input type="checkbox" checked={!!element.fill}
                             on:change={e => onSetProp("fill",e.target.checked || undefined)}>
                    </div>
                    {/if}
                    <div class="ve-row" style="gap:.5rem;margin-top:.25rem">
                      <label class="ve-prop-label">Width</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="100%"
                             value={element.width ?? ""}
                             on:input={e => onSetProp("width",e.target.value || undefined)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="300px"
                             value={element.height ?? "300px"}
                             on:input={e => onSetProp("height",e.target.value || undefined)}>
                    </div>
                    <div class="ve-media-hint">
                      Variable must hold JSON: <code>{"{"}"labels":["A","B"],"data":[10,25]{"}"}</code><br>
                      Multi-series: <code>{"{"}"labels":[…],"datasets":[{"{"}"label":"S1","data":[…],"color":"#f00"{"}"}]{"}"}  </code>
                    </div>

                  <!-- ── Feed ── -->
                  {:else if element.type === "vsm-feed"}
                    <label class="ve-prop-label">Data variable <span class="ve-hint">(JSON array of messages)</span></label>
                    <select class="ve-select" value={element.dataVar ?? ""}
                            on:change={e => onSetProp("dataVar",e.target.value || undefined)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <label class="ve-prop-label">Height</label>
                    <input class="ve-input" type="text" placeholder="400px"
                           value={element.height ?? "400px"}
                           on:input={e => onSetProp("height",e.target.value || undefined)}>
                    <label class="ve-prop-label">Agent label</label>
                    <input class="ve-input" type="text" placeholder="Agent"
                           value={element.agentLabel ?? "Agent"}
                           on:input={e => onSetProp("agentLabel",e.target.value || undefined)}>
                    <label class="ve-prop-label">User label</label>
                    <input class="ve-input" type="text" placeholder="You"
                           value={element.userLabel ?? "You"}
                           on:input={e => onSetProp("userLabel",e.target.value || undefined)}>
                    <div class="ve-row" style="align-items:center;gap:.5rem;margin-top:.25rem">
                      <label class="ve-prop-label" style="min-width:0">Show agent label</label>
                      <input type="checkbox" checked={element.showAgentLabel !== false}
                             on:change={e => onSetProp("showAgentLabel", e.target.checked ? undefined : false)}>
                      <label class="ve-prop-label" style="min-width:0;margin-left:.5rem">Show user label</label>
                      <input type="checkbox" checked={element.showUserLabel !== false}
                             on:change={e => onSetProp("showUserLabel", e.target.checked ? undefined : false)}>
                    </div>
                    <div class="ve-row" style="gap:.5rem;margin-top:.1rem">
                      <label class="ve-prop-label">Agent bg</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(element.agentColor ?? '#e8f4fd').hex}
                             on:input={e => onSetProp("agentColor",e.target.value)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">User bg</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(element.userColor ?? '#eafbe8').hex}
                             on:input={e => onSetProp("userColor",e.target.value)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">System bg</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(element.systemColor ?? '#f5f5f5').hex}
                             on:input={e => onSetProp("systemColor",e.target.value)}>
                    </div>
                    <div class="ve-row" style="gap:.5rem;margin-top:.1rem">
                      <label class="ve-prop-label">Agent text</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(element.agentTextColor ?? '#000000').hex}
                             on:input={e => onSetProp("agentTextColor",e.target.value === '#000000' ? undefined : e.target.value)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">User text</label>
                      <input class="ve-color" type="color"
                             value={parseColorAlpha(element.userTextColor ?? '#000000').hex}
                             on:input={e => onSetProp("userTextColor",e.target.value === '#000000' ? undefined : e.target.value)}>
                    </div>
                    <div class="ve-row" style="gap:.5rem;margin-top:.1rem">
                      <label class="ve-prop-label">Font size</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="1rem"
                             value={element.fontSize ?? ""}
                             on:input={e => onSetProp("fontSize",e.target.value || undefined)}>
                    </div>
                    <label class="ve-prop-label">Font</label>
                    <select class="ve-select"
                            value={fontOpts.some(f => f.v === (element.fontFamily ?? "")) ? (element.fontFamily ?? "") : "__custom__"}
                            on:change={e => onSetProp("fontFamily", e.target.value === "__custom__" ? undefined : (e.target.value || undefined))}>
                      {#each fontOpts as f}<option value={f.v}>{f.label}</option>{/each}
                      {#if element.fontFamily && !fontOpts.some(f => f.v === element.fontFamily)}
                        <option value="__custom__">{element.fontFamily}</option>
                      {/if}
                    </select>
                    <div class="ve-row" style="align-items:center;gap:.5rem;margin-top:.25rem">
                      <label class="ve-prop-label" style="min-width:0">Show timestamps</label>
                      <input type="checkbox" checked={!!element.showTimestamps}
                             on:change={e => onSetProp("showTimestamps",e.target.checked || undefined)}>
                    </div>
                    <div class="ve-media-hint">
                      Use <code>appendMessage(var='…', role='agent', text='…')</code> PlayAction to add messages at runtime.<br>
                      Roles: <code>agent</code> (left, tail) · <code>user</code> (right, tail) · <code>system</code> (center, italic)
                    </div>

                  <!-- ── Animate ── -->
                  {:else if element.type === "vsm-animate"}
                    {@const rateHints = { heartbeat:"BPM (e.g. 72)", breathe:"breaths/min (e.g. 15)", wave:"Hz (e.g. 4)", pulse:"Hz (e.g. 1)", spinner:"RPM (e.g. 60)" }}
                    <label class="ve-prop-label">Animation</label>
                    <select class="ve-select" value={element.animation ?? "heartbeat"}
                            on:change={e => onSetProp("animation",e.target.value)}>
                      <option value="heartbeat">❤ Heartbeat</option>
                      <option value="breathe">○ Breathe</option>
                      <option value="pulse">◎ Pulse</option>
                      <option value="spinner">↻ Spinner</option>
                      <option value="wave">▋▋▋ Wave</option>
                    </select>
                    <div class="ve-row" style="gap:.5rem">
                      <label class="ve-prop-label">Width</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="80px"
                             value={element.width ?? "80px"}
                             on:input={e => onSetProp("width",e.target.value || undefined)}>
                      <label class="ve-prop-label" style="margin-left:.5rem">Height</label>
                      <input class="ve-input ve-input-short" type="text" placeholder="80px"
                             value={element.height ?? "80px"}
                             on:input={e => onSetProp("height",e.target.value || undefined)}>
                    </div>
                    <label class="ve-prop-label">Default color</label>
                    <input class="ve-color" type="color"
                           value={parseColorAlpha(element.color ?? '#e26d5a').hex}
                           on:input={e => onSetProp("color",e.target.value)}>
                    <label class="ve-prop-label">Rate variable <span class="ve-hint">{rateHints[element.animation ?? "heartbeat"] ?? ""}</span></label>
                    <select class="ve-select" value={element.rateVar ?? ""}
                            on:change={e => onSetProp("rateVar",e.target.value || undefined)}>
                      <option value="">— none (use default) —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <label class="ve-prop-label">Color variable <span class="ve-hint">(overrides default color)</span></label>
                    <select class="ve-select" value={element.colorVar ?? ""}
                            on:change={e => onSetProp("colorVar",e.target.value || undefined)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    {#if (element.animation ?? "heartbeat") === "breathe"}
                    <label class="ve-prop-label">Amplitude variable <span class="ve-hint">(0–100, controls expansion)</span></label>
                    <select class="ve-select" value={element.amplitudeVar ?? ""}
                            on:change={e => onSetProp("amplitudeVar",e.target.value || undefined)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    {/if}
                    <label class="ve-prop-label">Opacity variable <span class="ve-hint">(0–100)</span></label>
                    <select class="ve-select" value={element.opacityVar ?? ""}
                            on:change={e => onSetProp("opacityVar",e.target.value || undefined)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>

                  <!-- ── Chat Input ── -->
                  {:else if element.type === "vsm-chat-input"}
                    <label class="ve-prop-label">Sends to variable</label>
                    <select class="ve-select" value={element.sendsVar ?? ""}
                            on:change={e => onSetProp("sendsVar",e.target.value)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <label class="ve-prop-label">Bind variable <span class="ve-hint">(String — pushing a value to it, e.g. via an updateVar flow action, sets the field's text, the same way a mic button's toggle variable drives its state)</span></label>
                    <select class="ve-select" value={element.bindVar ?? ""}
                            on:change={e => onSetProp("bindVar",e.target.value || undefined)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <label class="ve-prop-label">Placeholder</label>
                    <input class="ve-input" type="text" placeholder="Type your message…"
                           value={element.placeholder ?? ""}
                           on:input={e => onSetProp("placeholder",e.target.value || undefined)}>
                    <label class="ve-prop-label">Send button style</label>
                    <div class="ve-row" style="gap:.5rem">
                      <select class="ve-select" value={element.icon ?? "send"}
                              on:change={e => onSetProp("icon", e.target.value === "send" ? undefined : "")}>
                        <option value="send">Icon (paper plane)</option>
                        <option value="">Text label</option>
                      </select>
                      {#if (element.icon ?? "send") === "send"}
                        <span class="ve-icon-preview">{@html ICON_SVG.send}</span>
                      {/if}
                    </div>
                    <label class="ve-prop-label">Button label <span class="ve-hint">(text when style is "Text label"; title/aria-label either way)</span></label>
                    <input class="ve-input" type="text" placeholder="Send"
                           value={element.buttonLabel ?? ""}
                           on:input={e => onSetProp("buttonLabel",e.target.value || undefined)}>
                    <label class="ve-prop-label">Disabled variable <span class="ve-hint">(Bool — disables input when true)</span></label>
                    <select class="ve-select" value={element.disabledVar ?? ""}
                            on:change={e => onSetProp("disabledVar",e.target.value || undefined)}>
                      <option value="">— none —</option>
                      {#each variables as v}<option value={v.name}>{v.name}</option>{/each}
                    </select>
                    <div class="ve-media-hint">
                      User types a message and presses Enter or clicks the button.<br>
                      The text is sent to the selected variable and the field is cleared.
                    </div>


{:else}
  <p class="ve-unknown">Type <code>{element.type}</code> — edit in JSON tab.</p>
{/if}
