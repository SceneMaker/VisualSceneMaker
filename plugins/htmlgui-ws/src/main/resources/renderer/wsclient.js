/**
 * INFRASTRUCTURE FILE — DO NOT EDIT FOR CONTENT CUSTOMIZATION.
 *
 * This file is served directly from the plugin JAR at runtime. Any edits made
 * here will have no effect because the JAR version always takes precedence.
 *
 * To customize what users see:
 *   - Define screens in your project's screens.json  (schema-driven, recommended)
 *   - Add custom HTML pages to your project's gui/   (legacy, full flexibility)
 *
 * Only change this file if you are modifying the plugin itself.
 */

// constants
const WS_PROTOCOL = "ws";
const WS_HOSTNAME = "localhost";
const WS_PORT = 4041;      // number is nicer than string
const WS_ENDPOINT = "/ws";
// Jetty closes a WS with no traffic for 10 minutes (see HtmlGuiWsExecutor.launch()'s
// setIdleTimeout) — a GUI page left open while the user reads/watches without sending
// anything would otherwise idle out. Same fix as charamel-embed's vm-adapter.js
// (HEARTBEAT_INTERVAL_MS there): well under 10 minutes so a single missed beat still
// leaves margin.
const HEARTBEAT_INTERVAL_MS = 120000;

// variables
var webSocket = null;
var heartbeatTimer = null;
// Derive the connection target from the page origin so this GUI works when it is
// opened from a *remote* machine (co-editing), not only from the host that runs
// the runtime. The WebSocket port differs from the HTTP port this page is served
// on, so the editor appends it as ?wsPort=<ws_port> when it opens this page; we
// fall back to the built-in constants when the params/location are unavailable
// (e.g. opened directly on the host with no query string, or via file://).
var _wsParams = (typeof window !== "undefined" && window.location)
    ? new URLSearchParams(window.location.search) : null;
var ws_protocol = (typeof window !== "undefined" && window.location
    && window.location.protocol === "https:") ? "wss" : WS_PROTOCOL;
var ws_hostname = (typeof window !== "undefined" && window.location
    && window.location.hostname) ? window.location.hostname : WS_HOSTNAME;
var ws_port = (_wsParams && _wsParams.get("wsPort")) ? _wsParams.get("wsPort") : WS_PORT;
var ws_endpoint = WS_ENDPOINT;

// Nginx-routed deployments (VSM's inner-nginx dynamic plugin routing, doc/vsm-workspace-
// platform-plan.md Phase 5): the browser may not be able to reach ws_port directly at all, so
// instead of ws_hostname:ws_port, connect through the SAME path prefix this page was itself
// loaded under, with html_port swapped for ws_port. window.VSM_GUI_CONFIG.pathPrefix is
// server-injected (see vsm-gui-config.js, HtmlGuiWsExecutor.launch()) only when
// VSM_PLUGIN_PATH_PREFIX_ENABLED is on — absent (empty string) in every other deployment mode,
// in which case this falls through to the pre-existing ws_hostname:ws_port behavior untouched.
var _guiConfig = (typeof window !== "undefined" && window.VSM_GUI_CONFIG) || {};
var ws_path_prefix = _guiConfig.pathPrefix || "";
var ws_url = ws_path_prefix
    ? (ws_protocol + "://" + window.location.host + ws_path_prefix + "ws_port/ws")
    : (ws_protocol + "://" + ws_hostname + ":" + ws_port + ws_endpoint);

var eventMethod = window.addEventListener
    ? "addEventListener"
    : "attachEvent";
var eventer = window[eventMethod];

var messageEvent = eventMethod === "attachEvent"
    ? "onmessage"
    : "message";

let audioCtx;

eventer(messageEvent, function (e) {
    sendToWSServer(e.data);
    console.log(e);
})

/**
 * Init functions
 */
function init() {
    connect();
    setupAudioOverlay();
}

async function unlockAudioOnce() {
    audioCtx = audioCtx || new (window.AudioContext || window.webkitAudioContext)();
    if (audioCtx.state === "suspended") await audioCtx.resume();
    console.log("Audio unlocked");
}

function setupAudioOverlay() {
    const overlay = document.getElementById("audioOverlay");
    const btn = document.getElementById("enableAudioBtn");

    // Fallback: if overlay isn't present for some reason, keep the old behavior.
    if (!overlay || !btn) {
        document.addEventListener("pointerdown", unlockAudioOnce, {once: true});
        return;
    }

    btn.addEventListener("click", async () => {
        try {
            await unlockAudioOnce();      // user gesture happens here (button click)
            overlay.style.display = "none";
        } catch (e) {
            console.error("Failed to unlock audio:", e);
        }
    }, {once: true});
}

/**
 * Tell the screens iframe to switch to the given screen id.
 */
function loadScreen(screenId) {
    console.log("Load VSM screen: " + screenId);
    document.getElementById('screens').contentWindow.postMessage(
        { cmd: 'loadScreen', screen: screenId }, '*'
    );
}

/**
 * Push a variable update to the screens iframe so bound components reflect
 * the new value.
 */
function forwardUpdateVar(wsMsg) {
    // Format: updateVar$<varName>$<value>  (value may itself contain $)
    const idx1 = wsMsg.indexOf('$');
    const idx2 = wsMsg.indexOf('$', idx1 + 1);
    if (idx1 < 0 || idx2 < 0) return;
    const varName = wsMsg.substring(idx1 + 1, idx2);
    const value   = wsMsg.substring(idx2 + 1);
    document.getElementById('screens').contentWindow.postMessage(
        { cmd: 'updateVar', var: varName, value: value }, '*'
    );
}

/**
 * Event handler for clicking on button "Connect"
 */
function connect() {
    openWSConnection(ws_url);
}

/**
 * Event handler for clicking on button "Disconnect"
 */
function disconnect() {
    webSocket.close();
}

/**
 * Open a new WebSocket connection to the given URL
 */
function openWSConnection(webSocketURL) {
    console.log("openWSConnection to: " + webSocketURL);
    try {
        webSocket = new WebSocket(webSocketURL);
        webSocket.onopen = function (openEvent) {
            console.log("WebSocket OPEN: " + JSON.stringify(openEvent, null, 4));
            startHeartbeat();
        };
        webSocket.onclose = function (closeEvent) {
            console.log("WebSocket CLOSE: " + JSON.stringify(closeEvent, null, 4));
            stopHeartbeat();
        };
        webSocket.onerror = function (errorEvent) {
            console.log("WebSocket ERROR: " + JSON.stringify(errorEvent, null, 4));
        };
        webSocket.onmessage = function (messageEvent) {
            var wsMsg = messageEvent.data;
            console.log("WebSocket MESSAGE: " + wsMsg);

            if (wsMsg.indexOf("error") > 0) {
                //document.getElementById("incomingMsgOutput").value += "error: " + wsMsg.error + "\r\n";
            } else {
                //document.getElementById("incomingMsgOutput").value += "message: " + wsMsg + "\r\n";

                if (wsMsg.startsWith("loadScreen$")) {
                    loadScreen(wsMsg.substring("loadScreen$".length));
                } else if (wsMsg.startsWith("updateVar$")) {
                    forwardUpdateVar(wsMsg);
                }
            }
        };
    } catch (exception) {
        console.error(exception);
    }
}

/**
 * Keeps the WebSocket connection alive against Jetty's idle timeout — see
 * HEARTBEAT_INTERVAL_MS's declaration for why.
 */
function startHeartbeat() {
    stopHeartbeat(); // guard against a stray second onopen re-arming a duplicate timer
    heartbeatTimer = setInterval(function () {
        if (webSocket && webSocket.readyState === WebSocket.OPEN) {
            webSocket.send("heartbeat");
        }
    }, HEARTBEAT_INTERVAL_MS);
}

function stopHeartbeat() {
    if (heartbeatTimer !== null) {
        clearInterval(heartbeatTimer);
        heartbeatTimer = null;
    }
}

/**
 * Send a message to the WebSocket server
 */
function sendToWSServer(ws_message) {
    if (typeof webSocket == 'undefined') {
        return;
    }
    if (webSocket.readyState != WebSocket.OPEN) {
        console.error("webSocket is not open: " + webSocket.readyState);
        return;
    }
    webSocket.send(ws_message);
}

/**
 * Init the whole thing (connect to our server and enable audio
 */
init();