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
        :host { display: block; width: 100%; min-height: 100vh; }
        .vsm-screen { box-sizing: border-box; padding: 1rem; }
        .vsm-layout-column { display: flex; flex-direction: column; gap: 1rem; }
        .vsm-layout-row    { display: flex; flex-direction: row;    gap: 1rem; flex-wrap: wrap; }
        .vsm-layout-grid   { display: grid; gap: 1rem; }

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
    `;

    constructor() {
        super();
        this._schema        = null;
        this._activeScreen  = null;
        this._varValues     = {};
        this._audioUnlocked = false;

        window.addEventListener('message', (e) => {
            const data = e.data;
            if (!data || typeof data !== 'object') return;

            if (data.cmd === 'loadScreen') {
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

    _styleAttr(styleObj) {
        if (!styleObj) return '';
        return Object.entries(styleObj).map(([k, v]) => `${k}:${v}`).join(';');
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

        const layoutClass = screen.layout === 'flex-row' ? 'vsm-layout-row'
                          : screen.layout === 'grid'     ? 'vsm-layout-grid'
                          :                                'vsm-layout-column';

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

            <div class="vsm-screen ${layoutClass}"
                 style="background:${screen.background ?? 'transparent'}">
                ${(screen.elements ?? []).map(el => this._renderElement(el))}
            </div>`;
    }
}

customElements.define('vsm-screen-renderer', VsmScreenRenderer);
