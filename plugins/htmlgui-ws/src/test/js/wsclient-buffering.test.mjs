/**
 * The shell must hold messages for the screens iframe until the renderer can hear them.
 *
 * Run from the repository root:
 *   node --test plugins/htmlgui-ws/src/test/js/wsclient-buffering.test.mjs
 *
 * There is no JS test runner wired into this build, so this uses node's own and is not part of
 * `gradle test`. It reads the real wsclient.js rather than a copy, so it cannot drift from what the
 * plugin serves.
 *
 * What it guards: the renderer attaches its message listener while its module loads and its custom
 * element upgrades, which is well after this shell's WebSocket is open. postMessage has no queue, so
 * anything sent before that arrived at a window with nobody listening and was simply gone, with
 * nothing to resend it. A flow whose first step speaks as soon as the browser connects, which is
 * exactly what waiting for the interface to be ready produces, therefore lost its first line every
 * time: the page was up, the plugin had pushed the text, and the feed stayed empty.
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';

const SOURCE = 'plugins/htmlgui-ws/src/main/resources/renderer/wsclient.js';

/** Loads the buffering helpers out of the real file, without its WebSocket and audio setup. */
function loadShell() {
    const src = readFileSync(SOURCE, 'utf8');
    const from = src.indexOf('let screensReady = false;');
    const to = src.indexOf('/**\n * Send a message to the WebSocket server');
    assert.ok(from >= 0 && to > from, `${SOURCE} no longer contains the buffering helpers`);

    const delivered = [];
    globalThis.document = {
        getElementById: () => ({ contentWindow: { postMessage: (m) => delivered.push(m) } })
    };
    const api = new Function(src.slice(from, to) + `
        return { flushToScreens, loadScreen, forwardUpdateVar,
                 rendererReady: () => { screensReady = true; flushToScreens(); } };
    `)();
    return { ...api, delivered };
}

test('nothing is delivered before the renderer announces itself', () => {
    const shell = loadShell();
    shell.forwardUpdateVar('updateVar$conversation_log$[{"role":"agent","text":"Hallo"}]');
    shell.loadScreen('chat');
    assert.equal(shell.delivered.length, 0);
});

test('everything held back arrives once the renderer is listening, in order', () => {
    const shell = loadShell();
    shell.forwardUpdateVar('updateVar$conversation_log$[{"role":"agent","text":"Hallo"}]');
    shell.loadScreen('chat');
    shell.rendererReady();

    assert.equal(shell.delivered.length, 2);
    assert.deepEqual(shell.delivered[0],
        { cmd: 'updateVar', var: 'conversation_log', value: '[{"role":"agent","text":"Hallo"}]' });
    assert.deepEqual(shell.delivered[1], { cmd: 'loadScreen', screen: 'chat' });
});

test('later messages go straight through', () => {
    const shell = loadShell();
    shell.rendererReady();
    shell.forwardUpdateVar('updateVar$user_input$Patrick');

    assert.equal(shell.delivered.length, 1);
    assert.deepEqual(shell.delivered[0],
        { cmd: 'updateVar', var: 'user_input', value: 'Patrick' });
});

test('a value containing a dollar sign survives being split', () => {
    const shell = loadShell();
    shell.rendererReady();
    shell.forwardUpdateVar('updateVar$conversation_log$[{"text":"costs $5$10"}]');

    assert.equal(shell.delivered[0].value, '[{"text":"costs $5$10"}]');
});
