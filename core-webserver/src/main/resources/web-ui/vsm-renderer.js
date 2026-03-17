/**
 * VSM Schema-Driven Screen Renderer
 *
 * Loads screens.json from the plugin HTTP server, instantiates Web Awesome
 * components per the schema, and wires variable bindings to the VSM WebSocket
 * protocol via postMessage to the parent (wsclient.js).
 *
 * Inbound commands (via window.message from wsclient.js):
 *   { cmd: 'loadScreen', screen: '<screen-id>' }
 *   { cmd: 'updateVar',  var: '<name>', value: '<value>' }
 *
 * Outbound (postMessage to parent → wsclient.js → HtmlGuiWsExecutor):
 *   'varUpdate$<varName>$<value>'
 */
import { LitElement, html, css } from 'https://cdn.jsdelivr.net/gh/lit/dist@3/core/lit-core.min.js';

class VsmScreenRenderer extends LitElement {

    static properties = {
        _schema:        { state: true },
        _activeScreen:  { state: true },
        _varValues:     { state: true },
        _audioUnlocked: { state: true },
    };

    static styles = css`
        :host { display: block; width: 100%; height: 100%; }
        .vsm-screen { box-sizing: border-box; padding: 1rem; min-height: 100vh; }

        /* Audio unlock overlay — shown when character is enabled */
        .vsm-audio-overlay {
            position: fixed; inset: 0; z-index: 9999;
            display: flex; align-items: center; justify-content: center;
            background: rgba(0,0,0,0.65);
        }
        .vsm-audio-panel {
            background: #fff; color: #000;
            padding: 1.25rem 1.5rem; border-radius: 14px;
            max-width: 520px; width: calc(100% - 2rem);
            box-shadow: 0 10px 30px rgba(0,0,0,0.25);
            font-family: system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif;
        }
        .vsm-audio-panel h3 { margin: 0 0 .5rem 0; font-size: 1.15rem; }
        .vsm-audio-panel p  { margin: 0 0 1rem 0; opacity: .85; }
        .vsm-audio-btn {
            padding: .6rem 1rem; border: 0; border-radius: 10px;
            cursor: pointer; background: #5b8edc; color: #fff;
            font-size: 1rem; font-family: inherit;
        }
        .vsm-audio-btn:hover { background: #416aa6; }

        /* Speech bubble */
        .vsm-bubble-wrap { display: flex; flex-direction: column; gap: 0.35rem; }
        .vsm-bubble-speaker {
            font-size: 0.72rem; font-weight: 600; opacity: 0.65;
            padding: 0 0.5rem;
        }
        .vsm-bubble {
            position: relative;
            padding: 0.7rem 1rem;
            border-radius: 1rem;
            background: var(--bubble-bg, #e8f4fd);
            max-width: 480px;
            line-height: 1.5;
            word-wrap: break-word;
        }
        .vsm-bubble.tail-bottom-left::after {
            content: ''; position: absolute;
            bottom: -10px; left: 18px;
            width: 0; height: 0;
            border-right: 14px solid transparent;
            border-top: 11px solid var(--bubble-bg, #e8f4fd);
        }
        .vsm-bubble.tail-bottom-right::after {
            content: ''; position: absolute;
            bottom: -10px; right: 18px;
            width: 0; height: 0;
            border-left: 14px solid transparent;
            border-top: 11px solid var(--bubble-bg, #e8f4fd);
        }
        .vsm-bubble.tail-top-left::after {
            content: ''; position: absolute;
            top: -10px; left: 18px;
            width: 0; height: 0;
            border-right: 14px solid transparent;
            border-bottom: 11px solid var(--bubble-bg, #e8f4fd);
        }
        .vsm-bubble.tail-top-right::after {
            content: ''; position: absolute;
            top: -10px; right: 18px;
            width: 0; height: 0;
            border-left: 14px solid transparent;
            border-bottom: 11px solid var(--bubble-bg, #e8f4fd);
        }
    `;

    constructor() {
        super();
        this._schema              = null;
        this._activeScreen        = null;
        this._varValues           = {};
        this._audioUnlocked       = false;
        this._liveSchemaReceived  = false;  // guards against API fetch overwriting live schema

        window.addEventListener('message', (e) => {
            const data = e.data;
            if (!data || typeof data !== 'object') return;

            if (data.cmd === 'loadSchema') {
                // Live schema pushed from the editor — update without page reload.
                this._schema = data.schema;
                this._liveSchemaReceived = true;
                // Keep current screen if it still exists; otherwise fall back to first.
                const screens = data.schema?.screens ?? {};
                if (this._activeScreen && !screens[this._activeScreen]) {
                    const keys = Object.keys(screens);
                    this._activeScreen = keys.length > 0 ? keys[0] : null;
                }
            } else if (data.cmd === 'loadScreen') {
                this._activeScreen = data.screen;
            } else if (data.cmd === 'updateVar') {
                // Immutable update so Lit detects the change
                this._varValues = { ...this._varValues, [data.var]: data.value };
            }
        });
    }

    async connectedCallback() {
        super.connectedCallback();
        try {
            let schema;

            if (window.__VSM_SCHEMA_READY) {
                // Preview mode: schema supplied by the embedding page (no plugin server needed).
                schema = await window.__VSM_SCHEMA_READY;
            } else {
                // Runtime mode: fetch screens.json from the plugin HTTP server.
                const resp = await fetch('/screens.json');
                if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
                schema = await resp.json();
            }

            if (!schema) return;
            // A live loadSchema message may have arrived while we were fetching; don't overwrite it.
            if (this._liveSchemaReceived) return;
            this._schema = schema;

            // Honour ?screen= query param, otherwise fall back to the first screen.
            const requested = new URLSearchParams(window.location.search).get('screen');
            if (requested && this._schema.screens?.[requested]) {
                this._activeScreen = requested;
            } else if (!this._activeScreen && this._schema?.screens) {
                const keys = Object.keys(this._schema.screens);
                if (keys.length > 0) this._activeScreen = keys[0];
            }
        } catch (e) {
            console.error('[vsm-renderer] Failed to load schema:', e);
        }
    }

    // ---------------------------------------------------------------------------
    // VSM communication
    // ---------------------------------------------------------------------------

    _sendToVsm(varName, value) {
        parent.postMessage(`varUpdate$${varName}$${value}`, '*');
    }

    async _unlockAudio() {
        try {
            const ctx = new (window.AudioContext || window.webkitAudioContext)();
            if (ctx.state === 'suspended') await ctx.resume();
        } catch (e) {
            console.warn('[vsm-renderer] AudioContext unlock failed:', e);
        }
        this._audioUnlocked = true;
    }

    // ---------------------------------------------------------------------------
    // Rendering helpers
    // ---------------------------------------------------------------------------

    // Resolve /assets/x paths: in preview mode redirect to the project REST endpoint;
    // at runtime the plugin server serves /assets/x directly.
    _resolveAsset(src) {
        if (!src) return src;
        if (src.startsWith('/assets/') && window.__VSM_ASSETS_BASE) {
            return window.__VSM_ASSETS_BASE + '/' + src.slice('/assets/'.length);
        }
        return src;
    }

    _styleAttr(styleObj) {
        if (!styleObj) return '';
        return Object.entries(styleObj)
            .filter(([, v]) => v !== '' && v !== null && v !== undefined)
            .map(([k, v]) => `${k}:${v}`)
            .join(';');
    }

    _renderElement(el) {
        const boundValue = el.bindVar ? (this._varValues[el.bindVar] ?? '') : '';
        const style      = this._styleAttr(el.style);

        switch (el.type) {

            case 'sl-button':
            case 'wa-button':
                return html`
                    <sl-button
                        variant=${el.variant ?? 'default'}
                        style=${style}
                        @click=${() => el.sendsVar && this._sendToVsm(el.sendsVar, el.sendsValue ?? 'true')}>
                        ${el.label ?? ''}
                    </sl-button>`;

            case 'sl-input':
            case 'wa-input':
                return html`
                    <sl-input
                        label=${el.label ?? ''}
                        value=${boundValue}
                        style=${style}
                        @sl-input=${(e) => el.bindVar && this._sendToVsm(el.bindVar, e.target.value)}>
                    </sl-input>`;

            case 'sl-textarea':
            case 'wa-textarea':
                return html`
                    <sl-textarea
                        label=${el.label ?? ''}
                        value=${boundValue}
                        style=${style}
                        @sl-input=${(e) => el.bindVar && this._sendToVsm(el.bindVar, e.target.value)}>
                    </sl-textarea>`;

            case 'sl-range':
            case 'wa-range':
                return html`
                    <sl-range
                        label=${el.label ?? ''}
                        min=${el.min ?? 0}
                        max=${el.max ?? 100}
                        step=${el.step ?? 1}
                        value=${boundValue !== '' ? boundValue : (el.min ?? 0)}
                        style=${style}
                        @sl-change=${(e) => el.bindVar && this._sendToVsm(el.bindVar, String(e.target.value))}>
                    </sl-range>`;

            case 'sl-checkbox':
            case 'wa-checkbox':
                return html`
                    <sl-checkbox
                        ?checked=${boundValue === 'true'}
                        style=${style}
                        @sl-change=${(e) => el.bindVar && this._sendToVsm(el.bindVar, String(e.target.checked))}>
                        ${el.label ?? ''}
                    </sl-checkbox>`;

            case 'sl-select':
            case 'wa-select':
                return html`
                    <sl-select
                        label=${el.label ?? ''}
                        value=${boundValue}
                        style=${style}
                        @sl-change=${(e) => el.bindVar && this._sendToVsm(el.bindVar, e.target.value)}>
                        ${(el.options ?? []).map(o => {
                            const val = typeof o === 'object' ? o.value : o;
                            const lbl = typeof o === 'object' ? (o.label ?? o.value) : o;
                            return html`<sl-option value=${val}>${lbl}</sl-option>`;
                        })}
                    </sl-select>`;

            case 'sl-badge':
            case 'wa-badge':
                return html`
                    <sl-badge variant=${el.variant ?? 'neutral'} style=${style}>
                        ${el.label ?? boundValue}
                    </sl-badge>`;

            case 'sl-card':
            case 'wa-card':
                return html`
                    <sl-card style=${style}>
                        ${(el.children ?? []).map(child => this._renderElement(child))}
                    </sl-card>`;

            case 'sl-divider':
            case 'wa-divider':
                return html`<sl-divider style=${style}></sl-divider>`;

            case 'sl-text':
            case 'wa-text':
                return html`<p style=${style}>${el.content ?? boundValue}</p>`;

            case 'wa-image':
                return html`<img src=${el.src ?? ''} alt=${el.alt ?? ''} style=${style}>`;

            case 'vsm-image': {
                const imgStyle = [
                    el.width      ? `width:${el.width}`           : '',
                    el.height     ? `height:${el.height}`         : '',
                    el.objectFit  ? `object-fit:${el.objectFit}`  : '',
                    style,
                ].filter(Boolean).join(';');
                return html`<img src=${this._resolveAsset(el.src ?? '')}
                                 alt=${el.alt ?? ''}
                                 style=${imgStyle}>`;
            }

            case 'vsm-video': {
                const vidStyle = [
                    el.width  ? `width:${el.width}`   : 'max-width:100%',
                    el.height ? `height:${el.height}` : '',
                    style,
                ].filter(Boolean).join(';');
                return html`<video
                    src=${this._resolveAsset(el.src ?? '')}
                    style=${vidStyle}
                    ?controls=${el.controls !== false}
                    ?autoplay=${!!el.autoplay}
                    ?loop=${!!el.loop}
                    ?muted=${!!el.muted}
                    playsinline></video>`;
            }

            case 'vsm-audio':
                return html`<audio
                    src=${this._resolveAsset(el.src ?? '')}
                    style=${style}
                    ?controls=${el.controls !== false}
                    ?autoplay=${!!el.autoplay}
                    ?loop=${!!el.loop}></audio>`;

            case 'vsm-embed': {
                const embedStyle = [
                    `width:${el.width ?? '100%'}`,
                    `height:${el.height ?? '315px'}`,
                    'border:none',
                    style,
                ].filter(Boolean).join(';');
                return html`<iframe
                    src=${el.src ?? ''}
                    title=${el.title ?? ''}
                    style=${embedStyle}
                    allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                    allowfullscreen></iframe>`;
            }

            case 'vsm-filler': {
                const fillerStyle = el.flexGrow
                    ? 'flex:1'
                    : [
                        el.width  ? `width:${el.width}`   : '',
                        el.height ? `height:${el.height}` : '',
                        style,
                    ].filter(Boolean).join(';');
                return html`<div style=${fillerStyle}></div>`;
            }

            case 'vsm-panel': {
                const panelIsRow = el.layout === 'flex-row';
                const panelHProp = panelIsRow ? 'justify-content' : 'align-items';
                const panelVProp = panelIsRow ? 'align-items'     : 'justify-content';
                const panelStyle = [
                    'display:flex',
                    panelIsRow ? 'flex-direction:row;flex-wrap:wrap' : 'flex-direction:column',
                    `gap:${el.gap ?? '0.5rem'}`,
                    el.flexGrow       ? 'flex:1'                                  : '',
                    el.background     ? `background:${el.background}`             : '',
                    el.padding        ? `padding:${el.padding}`                   : '',
                    el.alignItems     ? `${panelHProp}:${el.alignItems}`          : '',
                    el.justifyContent ? `${panelVProp}:${el.justifyContent}`      : '',
                    style,
                ].filter(Boolean).join(';');
                return html`
                    <div style=${panelStyle}>
                        ${(el.children ?? []).map(child => this._renderElement(child))}
                    </div>`;
            }

            case 'vsm-feed': {
                const feedStyle = [
                    el.width ? `width:${el.width}` : '',
                    style,
                ].filter(Boolean).join(';');
                return html`<vsm-feed-element
                    .config=${el}
                    .datavalue=${this._varValues[el.dataVar] ?? ''}
                    style=${feedStyle}></vsm-feed-element>`;
            }

            case 'vsm-animate': {
                const animStyle = [
                    el.width  ? `width:${el.width}`   : '',
                    el.height ? `height:${el.height}` : '',
                    style,
                ].filter(Boolean).join(';');
                return html`<vsm-animate-element
                    animation=${el.animation ?? 'heartbeat'}
                    .config=${el}
                    .ratevalue=${this._varValues[el.rateVar]       ?? ''}
                    .colorvalue=${this._varValues[el.colorVar]     ?? ''}
                    .amplitudevalue=${this._varValues[el.amplitudeVar] ?? ''}
                    .opacityvalue=${this._varValues[el.opacityVar]  ?? ''}
                    style=${animStyle}></vsm-animate-element>`;
            }

            case 'vsm-bubble': {
                const bg        = el.background ?? '#e8f4fd';
                const content   = el.bindVar ? (this._varValues[el.bindVar] ?? '') : (el.content ?? '');

                // Derive left/right from align-self; el.tail controls up/down direction.
                const alignSelf = el.style?.['align-self'] ?? '';
                const tailDir   = el.tail !== undefined ? el.tail : 'bottom';
                let tailClass   = '';
                if (tailDir === 'bottom') {
                    tailClass = alignSelf === 'flex-end' ? 'tail-bottom-right'
                              : alignSelf === 'center'   ? ''
                              : 'tail-bottom-left';
                } else if (tailDir === 'top') {
                    tailClass = alignSelf === 'flex-end' ? 'tail-top-right'
                              : alignSelf === 'center'   ? ''
                              : 'tail-top-left';
                }

                // align-self on the wrap; all text/font styles + bg var on the bubble itself.
                const wrapStyle  = alignSelf ? `align-self:${alignSelf}` : '';
                const textStyles = { ...el.style };
                delete textStyles['align-self'];
                const bubbleStyle = [
                    this._styleAttr(textStyles),
                    `--bubble-bg:${bg}`,
                ].filter(Boolean).join(';');

                return html`
                    <div class="vsm-bubble-wrap" style=${wrapStyle}>
                        ${el.speaker ? html`<div class="vsm-bubble-speaker">${el.speaker}</div>` : html``}
                        <div class=${'vsm-bubble' + (tailClass ? ' ' + tailClass : '')}
                             style=${bubbleStyle}>${content}</div>
                    </div>`;
            }

            case 'vsm-chart': {
                const chartW = el.width  ?? '100%';
                const chartH = el.height ?? '300px';
                const chartStyle = [
                    `width:${chartW}`,
                    style,
                ].filter(Boolean).join(';');
                return html`<vsm-chart-element
                    charttype=${el.chartType ?? 'bar'}
                    .config=${el}
                    .datavalue=${this._varValues[el.dataVar] ?? ''}
                    style=${chartStyle}
                    height=${chartH}></vsm-chart-element>`;
            }

            default:
                console.warn('[vsm-renderer] Unknown element type:', el.type);
                return html`<span style="color:red">[unknown: ${el.type}]</span>`;
        }
    }

    render() {
        if (!this._schema || !this._activeScreen) return html``;

        const screen = this._schema.screens[this._activeScreen];
        if (!screen) {
            return html`<p style="color:red">Screen not found: ${this._activeScreen}</p>`;
        }

        // Build flex layout style with direction-aware alignment mapping.
        // For column (default): H → align-items, V → justify-content.
        // For row: H → justify-content (main axis), V → align-items (cross axis).
        // 'grid' treated as flex-column for backward compatibility.
        const isRow  = screen.layout === 'flex-row';
        const hProp  = isRow ? 'justify-content' : 'align-items';
        const vProp  = isRow ? 'align-items'     : 'justify-content';

        const screenStyle = [
            'display:flex',
            isRow ? 'flex-direction:row;flex-wrap:wrap' : 'flex-direction:column',
            'gap:1rem',
            `background:${screen.background ?? 'transparent'}`,
            screen.alignItems     ? `${hProp}:${screen.alignItems}`     : '',
            screen.justifyContent ? `${vProp}:${screen.justifyContent}` : '',
        ].filter(Boolean).join(';');

        // Optional top-level "character" key: a persistent iframe fixed behind all screens.
        // Set "enabled": false to keep the config but skip loading.
        const char        = this._schema.character;
        const charEnabled = char && char.enabled !== false && !window.__VSM_PREVIEW_MODE;
        const charStyle   = charEnabled
            ? (this._styleAttr(char.style) || 'position:fixed;left:0;top:0;width:100%;height:100%;z-index:-1;border:none')
            : '';

        return html`
            ${charEnabled ? html`<iframe
                src=${char.src ?? ''}
                allow=${char.allow ?? ''}
                style=${charStyle}
                frameborder="0"></iframe>` : html``}

            ${charEnabled && !this._audioUnlocked ? html`
            <div class="vsm-audio-overlay">
                <div class="vsm-audio-panel">
                    <h3 id="vsm-audio-title">Enable audio</h3>
                    <p>Chrome requires a user gesture before audio can start.</p>
                    <button class="vsm-audio-btn"
                            @click=${this._unlockAudio}>Enable audio</button>
                </div>
            </div>` : html``}

            <div class="vsm-screen" style=${screenStyle}>
                ${(screen.elements ?? []).map(el => this._renderElement(el))}
            </div>`;
    }
}

customElements.define('vsm-screen-renderer', VsmScreenRenderer);

// ---------------------------------------------------------------------------
// vsm-chart-element — Chart.js wrapper (Model 2: full dataset via variable)
//
// Attributes (lowercase for HTML attribute binding):
//   charttype   — 'bar' | 'line'
//   height      — CSS height string, e.g. '300px'
// Properties:
//   .config     — the schema element object (color, label, fill, …)
//   .datavalue  — JSON string pushed from a bound variable
//
// Data formats (the variable value):
//   Simple:       { "labels": ["A","B"], "data": [10, 25] }
//   Multi-series: { "labels": ["A","B"], "datasets": [{ "label":"S1", "data":[10,25], "color":"#f00" }] }
// ---------------------------------------------------------------------------

class VsmChartElement extends LitElement {

    static properties = {
        charttype: {},
        height:    {},
        config:    { type: Object },
        datavalue: {},
    };

    static styles = css`
        :host { display: block; }
        canvas { display: block; }
    `;

    constructor() {
        super();
        this.charttype = 'bar';
        this.height    = '300px';
        this.config    = {};
        this.datavalue = '';
        this._chart    = null;
    }

    firstUpdated() {
        this._initChart();
    }

    updated(changed) {
        if (!this._chart) { this._initChart(); return; }
        if (changed.has('charttype') || changed.has('config')) {
            // Full recreation needed when chart type or config changes.
            this._chart.destroy();
            this._chart = null;
            this._initChart();
        } else if (changed.has('datavalue')) {
            this._applyData();
        }
    }

    disconnectedCallback() {
        super.disconnectedCallback();
        if (this._chart) { this._chart.destroy(); this._chart = null; }
    }

    _parseData() {
        try { return this.datavalue ? JSON.parse(this.datavalue) : null; }
        catch { return null; }
    }

    _buildDatasets(d) {
        const cfg   = this.config ?? {};
        const color = cfg.color ?? '#5b8edc';
        if (d?.datasets) {
            return d.datasets.map(ds => ({
                label:           ds.label            ?? cfg.label ?? '',
                data:            ds.data             ?? [],
                backgroundColor: ds.color            ?? color,
                borderColor:     ds.color            ?? color,
                fill:            cfg.fill            ?? false,
                tension:         0.3,
            }));
        }
        return [{
            label:           cfg.label ?? '',
            data:            d?.data   ?? [],
            backgroundColor: color,
            borderColor:     color,
            fill:            cfg.fill  ?? false,
            tension:         0.3,
        }];
    }

    _initChart() {
        if (!window.Chart) {
            console.warn('[vsm-chart] Chart.js not loaded');
            return;
        }
        const canvas = this.renderRoot.querySelector('canvas');
        if (!canvas) return;
        const d = this._parseData();
        this._chart = new window.Chart(canvas, {
            type:    this.charttype || 'bar',
            data:    { labels: d?.labels ?? [], datasets: this._buildDatasets(d) },
            options: { responsive: true, maintainAspectRatio: false },
        });
    }

    _applyData() {
        const d = this._parseData();
        if (!d || !this._chart) return;
        this._chart.data.labels   = d.labels ?? [];
        this._chart.data.datasets = this._buildDatasets(d);
        this._chart.update();
    }

    render() {
        return html`<div style=${'width:100%;height:' + (this.height ?? '300px')}>
            <canvas></canvas>
        </div>`;
    }
}

customElements.define('vsm-chart-element', VsmChartElement);

// ---------------------------------------------------------------------------
// vsm-feed-element — Live dialogue diary
//
// Renders a scrollable, role-aware list of speech bubbles driven by a single
// VSM variable that holds a JSON array of message objects.
//
// Message object format:
//   { "role": "agent"|"user"|"system", "text": "...", "speaker": "...", "timestamp": "..." }
//
// Config (from the schema element):
//   dataVar, agentColor, userColor, systemColor, agentLabel, userLabel,
//   showTimestamps, height
// ---------------------------------------------------------------------------

class VsmFeedElement extends LitElement {

    static properties = {
        config:    { type: Object },
        datavalue: {},
    };

    static styles = css`
        :host { display: block; }

        .vsm-feed {
            display: flex; flex-direction: column; gap: 0.55rem;
            overflow-y: auto; padding: 0.6rem 0.8rem;
            box-sizing: border-box; scroll-behavior: smooth;
        }

        /* Bubble wrapper — controls alignment */
        .vsm-feed-row             { display: flex; flex-direction: column; max-width: 78%; }
        .vsm-feed-row.role-agent  { align-self: flex-start; }
        .vsm-feed-row.role-user   { align-self: flex-end; }
        .vsm-feed-row.role-system { align-self: center; max-width: 90%; }

        .vsm-feed-speaker {
            font-size: 0.7rem; font-weight: 600; opacity: 0.6;
            padding: 0 0.5rem; margin-bottom: 0.15rem;
        }
        .vsm-feed-row.role-user .vsm-feed-speaker { text-align: right; }

        .vsm-feed-bubble {
            position: relative;
            padding: 0.6rem 0.9rem;
            border-radius: 1rem;
            background: var(--bubble-bg, #e8f4fd);
            line-height: 1.5; word-wrap: break-word;
        }

        /* agent tail — bottom-left */
        .vsm-feed-row.role-agent .vsm-feed-bubble::after {
            content: ''; position: absolute;
            bottom: -9px; left: 16px;
            border-right: 12px solid transparent;
            border-top: 10px solid var(--bubble-bg, #e8f4fd);
        }

        /* user tail — bottom-right */
        .vsm-feed-row.role-user .vsm-feed-bubble::after {
            content: ''; position: absolute;
            bottom: -9px; right: 16px;
            border-left: 12px solid transparent;
            border-top: 10px solid var(--bubble-bg, #e8f4fd);
        }

        /* system — no tail, centered, subdued */
        .vsm-feed-row.role-system .vsm-feed-bubble {
            font-style: italic; font-size: 0.85rem;
            text-align: center; border-radius: 0.6rem; opacity: 0.75;
        }

        .vsm-feed-timestamp {
            font-size: 0.68rem; opacity: 0.45;
            padding: 0.1rem 0.5rem; margin-top: 0.1rem;
        }
        .vsm-feed-row.role-user .vsm-feed-timestamp { text-align: right; }
    `;

    constructor() {
        super();
        this.config    = {};
        this.datavalue = '';
    }

    updated(changed) {
        if (changed.has('datavalue')) {
            this.updateComplete.then(() => {
                const feed = this.renderRoot.querySelector('.vsm-feed');
                if (feed) feed.scrollTop = feed.scrollHeight;
            });
        }
    }

    _parseMessages() {
        try { return this.datavalue ? JSON.parse(this.datavalue) : []; }
        catch { return []; }
    }

    _renderMessage(msg, cfg) {
        const role = msg.role ?? 'agent';
        const bg   = role === 'user'   ? (cfg.userColor   ?? '#eafbe8')
                   : role === 'system' ? (cfg.systemColor ?? '#f5f5f5')
                   :                     (cfg.agentColor  ?? '#e8f4fd');

        // Speaker label: per-message override > config label > omit for system
        const defaultLabel = role === 'user' ? (cfg.userLabel ?? 'You')
                           : role === 'agent' ? (cfg.agentLabel ?? 'Agent')
                           : null;
        const speaker = msg.speaker !== undefined ? msg.speaker : defaultLabel;

        return html`
            <div class=${'vsm-feed-row role-' + role}>
                ${speaker ? html`<div class="vsm-feed-speaker">${speaker}</div>` : html``}
                <div class="vsm-feed-bubble" style=${'--bubble-bg:' + bg + ';background:' + bg}>
                    ${msg.text ?? ''}
                </div>
                ${msg.timestamp && cfg.showTimestamps
                    ? html`<div class="vsm-feed-timestamp">${msg.timestamp}</div>`
                    : html``}
            </div>`;
    }

    render() {
        const cfg  = this.config ?? {};
        const msgs = this._parseMessages();
        return html`
            <div class="vsm-feed" style=${'height:' + (cfg.height ?? '400px')}>
                ${msgs.map(m => this._renderMessage(m, cfg))}
            </div>`;
    }
}

customElements.define('vsm-feed-element', VsmFeedElement);

// ---------------------------------------------------------------------------
// vsm-animate-element — Variable-driven animated overlays
//
// Displays a pre-built looping animation whose parameters are controlled live
// by VSM variables mapped to CSS custom properties.
//
// Supported animations: heartbeat | breathe | pulse | spinner | wave
//
// Schema element fields:
//   animation      — which animation (default: 'heartbeat')
//   color          — static fallback color (hex or CSS)
//   width/height   — element size (default: 80px / 80px)
//   rateVar        — BPM / breaths-per-min / Hz / RPM → --vsm-duration
//   colorVar       — CSS color string                 → --vsm-color
//   amplitudeVar   — 0–100 scale                      → --vsm-amplitude
//   opacityVar     — 0–100                            → --vsm-opacity
// ---------------------------------------------------------------------------

class VsmAnimateElement extends LitElement {

    static properties = {
        animation:      {},
        config:         { type: Object },
        ratevalue:      {},
        colorvalue:     {},
        amplitudevalue: {},
        opacityvalue:   {},
    };

    static styles = css`
        :host {
            display: inline-block;
            --vsm-duration:  833ms;
            --vsm-color:     #e26d5a;
            --vsm-amplitude: 0.25;
            --vsm-opacity:   1;
        }

        /* ── Heartbeat ─────────────────────────────────────────────── */
        @keyframes vsm-heartbeat {
            0%   { transform: scale(1);    }
            14%  { transform: scale(1.22); }
            28%  { transform: scale(1);    }
            42%  { transform: scale(1.12); }
            70%  { transform: scale(1);    }
            100% { transform: scale(1);    }
        }
        .heart-path {
            transform-box: fill-box; transform-origin: center;
            fill: var(--vsm-color);
            animation: vsm-heartbeat var(--vsm-duration) ease-in-out infinite;
        }

        /* ── Breathe ───────────────────────────────────────────────── */
        @keyframes vsm-breathe {
            0%, 100% { transform: scale(1); opacity: 0.65; }
            50%      { transform: scale(calc(1 + var(--vsm-amplitude))); opacity: 1; }
        }
        .breathe-circle {
            transform-box: fill-box; transform-origin: center;
            fill: var(--vsm-color); opacity: 0.65;
            animation: vsm-breathe var(--vsm-duration) ease-in-out infinite;
        }

        /* ── Pulse ─────────────────────────────────────────────────── */
        @keyframes vsm-pulse-ring {
            0%   { transform: scale(1);   opacity: 0.7; }
            100% { transform: scale(3.5); opacity: 0;   }
        }
        .pulse-wrap {
            position: relative; width: 100%; height: 100%;
            display: flex; align-items: center; justify-content: center;
        }
        .pulse-ring {
            position: absolute;
            width: 30%; height: 30%; border-radius: 50%;
            background: var(--vsm-color);
            animation: vsm-pulse-ring var(--vsm-duration) ease-out infinite;
        }
        .pulse-ring-2 { animation-delay: calc(var(--vsm-duration) * 0.5); }
        .pulse-dot {
            width: 30%; height: 30%; border-radius: 50%;
            background: var(--vsm-color); opacity: var(--vsm-opacity);
            position: relative; z-index: 1;
        }

        /* ── Spinner ───────────────────────────────────────────────── */
        @keyframes vsm-spin {
            from { transform: rotate(0deg);   }
            to   { transform: rotate(360deg); }
        }
        .spinner-g {
            transform-box: fill-box; transform-origin: center;
            animation: vsm-spin var(--vsm-duration) linear infinite;
        }
        .spinner-track { stroke: var(--vsm-color); opacity: 0.15; }
        .spinner-arc   {
            stroke: var(--vsm-color); stroke-linecap: round;
            stroke-dasharray: 251; stroke-dashoffset: 188;
        }

        /* ── Wave ──────────────────────────────────────────────────── */
        @keyframes vsm-wave-bar {
            0%, 100% { transform: scaleY(0.2); opacity: 0.45; }
            50%      { transform: scaleY(1);   opacity: 1;    }
        }
        .wave-wrap {
            display: flex; align-items: flex-end; justify-content: center;
            gap: 5%; height: 100%; width: 100%;
            padding: 0 4%; box-sizing: border-box;
        }
        .wave-bar {
            flex: 1; height: 75%; border-radius: 4px;
            background: var(--vsm-color); transform-origin: bottom;
            animation: vsm-wave-bar var(--vsm-duration) ease-in-out infinite;
            opacity: var(--vsm-opacity);
        }
        .wave-bar:nth-child(1) { animation-delay: 0ms; }
        .wave-bar:nth-child(2) { animation-delay: calc(var(--vsm-duration) * 0.15); }
        .wave-bar:nth-child(3) { animation-delay: calc(var(--vsm-duration) * 0.30); }
        .wave-bar:nth-child(4) { animation-delay: calc(var(--vsm-duration) * 0.45); }
        .wave-bar:nth-child(5) { animation-delay: calc(var(--vsm-duration) * 0.60); }
    `;

    constructor() {
        super();
        this.animation      = 'heartbeat';
        this.config         = {};
        this.ratevalue      = '';
        this.colorvalue     = '';
        this.amplitudevalue = '';
        this.opacityvalue   = '';
    }

    updated() { this._updateCssVars(); }

    _updateCssVars() {
        const anim = this.animation ?? 'heartbeat';
        const cfg  = this.config    ?? {};

        // Rate → animation duration
        const rate = parseFloat(this.ratevalue);
        if (!isNaN(rate) && rate > 0) {
            const perMinute = anim === 'heartbeat' || anim === 'breathe';
            const ms = perMinute ? Math.round(60000 / rate) : Math.round(1000 / rate);
            this.style.setProperty('--vsm-duration', ms + 'ms');
        }

        // Color: variable value overrides static config color
        const color = (this.colorvalue !== '') ? this.colorvalue : (cfg.color ?? null);
        if (color) this.style.setProperty('--vsm-color', color);

        // Amplitude 0–100 → 0.0–1.0
        const amp = parseFloat(this.amplitudevalue);
        if (!isNaN(amp)) this.style.setProperty('--vsm-amplitude', (amp / 100).toFixed(3));

        // Opacity 0–100 → 0.0–1.0
        const op = parseFloat(this.opacityvalue);
        if (!isNaN(op)) this.style.setProperty('--vsm-opacity', (op / 100).toFixed(3));
    }

    _renderHeartbeat() {
        return html`
            <svg viewBox="0 0 100 100" style="width:100%;height:100%;overflow:visible">
                <path class="heart-path"
                      d="M50 35 C50 35 30 18 18 24 C5 30 5 47 17 58 L50 85 L83 58 C95 47 95 30 82 24 C70 18 50 35 50 35Z"/>
            </svg>`;
    }

    _renderBreathe() {
        return html`
            <svg viewBox="0 0 100 100" style="width:100%;height:100%">
                <circle class="breathe-circle" cx="50" cy="50" r="35"/>
            </svg>`;
    }

    _renderPulse() {
        return html`
            <div class="pulse-wrap">
                <div class="pulse-ring"></div>
                <div class="pulse-ring pulse-ring-2"></div>
                <div class="pulse-dot"></div>
            </div>`;
    }

    _renderSpinner() {
        return html`
            <svg viewBox="0 0 100 100" style="width:100%;height:100%" fill="none">
                <g class="spinner-g">
                    <circle class="spinner-track" cx="50" cy="50" r="40" stroke-width="10"/>
                    <circle class="spinner-arc"   cx="50" cy="50" r="40" stroke-width="10"/>
                </g>
            </svg>`;
    }

    _renderWave() {
        return html`
            <div class="wave-wrap">
                <div class="wave-bar"></div>
                <div class="wave-bar"></div>
                <div class="wave-bar"></div>
                <div class="wave-bar"></div>
                <div class="wave-bar"></div>
            </div>`;
    }

    render() {
        const anim = this.animation ?? 'heartbeat';
        const cfg  = this.config    ?? {};
        return html`
            <div style=${'width:' + (cfg.width ?? '80px') + ';height:' + (cfg.height ?? '80px') + ';position:relative;display:inline-block'}>
                ${anim === 'heartbeat' ? this._renderHeartbeat()
                : anim === 'breathe'   ? this._renderBreathe()
                : anim === 'pulse'     ? this._renderPulse()
                : anim === 'spinner'   ? this._renderSpinner()
                : anim === 'wave'      ? this._renderWave()
                : html`<div style="color:orange;font-size:.75rem;padding:.25rem">[unknown: ${anim}]</div>`}
            </div>`;
    }
}

customElements.define('vsm-animate-element', VsmAnimateElement);
