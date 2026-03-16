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
