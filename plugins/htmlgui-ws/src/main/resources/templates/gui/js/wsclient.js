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

// variables
var webSocket = null;
var ws_protocol = WS_PROTOCOL;
var ws_hostname = WS_HOSTNAME;
var ws_port = WS_PORT;
var ws_endpoint = WS_ENDPOINT;

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
    openWSConnection(ws_protocol, ws_hostname, ws_port, ws_endpoint);
}

/**
 * Event handler for clicking on button "Disconnect"
 */
function disconnect() {
    webSocket.close();
}

/**
 * Open a new WebSocket connection using the given parameters
 */
function openWSConnection(protocol, hostname, port, endpoint) {
    var webSocketURL = null;
    webSocketURL = protocol + "://" + hostname + ":" + port + endpoint;
    console.log("openWSConnection to: " + webSocketURL);
    try {
        webSocket = new WebSocket(webSocketURL);
        webSocket.onopen = function (openEvent) {
            console.log("WebSocket OPEN: " + JSON.stringify(openEvent, null, 4));
        };
        webSocket.onclose = function (closeEvent) {
            console.log("WebSocket CLOSE: " + JSON.stringify(closeEvent, null, 4));
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