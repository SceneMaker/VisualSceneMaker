var webSocket   = null;
var ws_protocol = null;
var ws_hostname = null;
var ws_port     = null;
var ws_endpoint = null;

var eventMethod = window.addEventListener
			? "addEventListener"
			: "attachEvent";
var eventer = window[eventMethod];

var messageEvent = eventMethod === "attachEvent"
		? "onmessage"
		: "message";

var aliveTimer = null;

eventer(messageEvent, function (e) {
	sendToWSServer(e.data);
    console.log(e);
	})

/**
 * Load Web page into gui iframe
 */
function loadPage(page) {
    console.log("Load page " + page + " into iframe gui")
    document.getElementById('gui').src = page;
}

function guiToFront() {
		document.getElementById('gui').style.zIndex = 1;
  	document.getElementById('emma').style.zIndex = -1;
}

function vcToFront() {
		document.getElementById('gui').style.zIndex = -1;
  	document.getElementById('emma').style.zIndex = 1;
}

/**
 * Set values of gui elements in gui iframe
 */
function setGuiElementValue(msg) {
    console.log("Assign value to element " + msg)
    document.getElementById('gui').contentWindow.postMessage(msg, '*');
};


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
 * Message from client (this and webpage loading this) that client is alive
 */
function clientAliveMessage() {
	console.log("Send client alive message to server");
  sendToWSServer("gui_alive");
  aliveTimer = setTimeout(clientAliveMessage, 1000);
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
        webSocket.onopen = function(openEvent) {
            console.log("WebSocket OPEN: " + JSON.stringify(openEvent, null, 4));
					  clientAliveMessage();
        };
        webSocket.onclose = function (closeEvent) {
            console.log("WebSocket CLOSE: " + JSON.stringify(closeEvent, null, 4));
						clearTimeout(aliveTimer);
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

                // if message contains a ":", assume it is a key value pair to set a value of an element
                if (wsMsg.includes("$")) {
                    setGuiElementValue(wsMsg);
                } else if (wsMsg.includes("guiToFront")) {
									guiToFront();
								} else if (wsMsg.includes("vcToFront")) {
									vcToFront();
								}	else { // assume it is a web page and load it into the gui iframe
                    loadPage(wsMsg);
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
