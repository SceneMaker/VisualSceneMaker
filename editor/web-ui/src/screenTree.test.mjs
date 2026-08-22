/**
 * Run from the repository root:
 *   node --test editor/web-ui/src/screenTree.test.mjs
 *
 * There is no JS test runner wired into the web-ui build (Vite has no test step), so this uses
 * node's own — the same approach as plugins/htmlgui-ws/src/test/js/wsclient-buffering.test.mjs —
 * and is not part of `npm run build` or `gradle test`.
 */
import { test } from 'node:test';
import assert from 'node:assert/strict';
import {
    getAtPath, getContainerAtPath, setPropAtPath, setStylePropAtPath,
    removeAtPath, insertChildAtPath, moveAtPath, pathKey,
} from './screenTree.js';

/** A representative three-level tree: outer panel > header panel > name-block panel > text. */
function sampleTree() {
    return [
        {
            type: 'vsm-panel',
            children: [
                {
                    type: 'vsm-panel',
                    children: [
                        { type: 'vsm-image', src: '/assets/alex.png' },
                        {
                            type: 'vsm-panel',
                            children: [
                                { type: 'sl-text', content: 'Alex' },
                                { type: 'sl-text', content: 'Digital Intake Assistant' },
                            ],
                        },
                    ],
                },
                { type: 'vsm-feed', dataVar: 'conversation_log' },
                {
                    type: 'vsm-panel',
                    children: [
                        { type: 'vsm-chat-input', sendsVar: 'user_input' },
                    ],
                },
            ],
        },
    ];
}

function deepFreeze(value) {
    if (value && typeof value === 'object' && !Object.isFrozen(value)) {
        Object.freeze(value);
        for (const v of Object.values(value)) deepFreeze(v);
    }
    return value;
}

// ── getAtPath ────────────────────────────────────────────────────────────────

test('getAtPath resolves at every depth', () => {
    const tree = sampleTree();
    assert.equal(getAtPath(tree, [0]).type, 'vsm-panel');
    assert.equal(getAtPath(tree, [0, 0]).type, 'vsm-panel');
    assert.equal(getAtPath(tree, [0, 0, 0]).type, 'vsm-image');
    assert.equal(getAtPath(tree, [0, 0, 1, 0]).content, 'Alex');
    assert.equal(getAtPath(tree, [0, 0, 1, 1]).content, 'Digital Intake Assistant');
    assert.equal(getAtPath(tree, [0, 2, 0]).type, 'vsm-chat-input');
});

test('getAtPath returns undefined for the empty path or an out-of-range index', () => {
    const tree = sampleTree();
    assert.equal(getAtPath(tree, []), undefined);
    assert.equal(getAtPath(tree, [99]), undefined);
    assert.equal(getAtPath(tree, [0, 99]), undefined);
    assert.equal(getAtPath(tree, [0, 0, 0, 0]), undefined); // vsm-image has no children
});

// ── getContainerAtPath ───────────────────────────────────────────────────────

test('getContainerAtPath finds the root array for a top-level path', () => {
    const tree = sampleTree();
    assert.equal(getContainerAtPath(tree, [0]), tree);
});

test('getContainerAtPath finds a nested children array', () => {
    const tree = sampleTree();
    const container = getContainerAtPath(tree, [0, 0, 1, 0]);
    assert.equal(container, getAtPath(tree, [0, 0, 1]).children);
    assert.equal(container.length, 2);
});

// ── setPropAtPath ────────────────────────────────────────────────────────────

test('setPropAtPath sets a prop at any depth without touching the input', () => {
    const tree = deepFreeze(sampleTree());
    const next = setPropAtPath(tree, [0, 0, 0], 'src', '/assets/new.png');
    assert.equal(getAtPath(next, [0, 0, 0]).src, '/assets/new.png');
    assert.equal(getAtPath(tree, [0, 0, 0]).src, '/assets/alex.png'); // original untouched
});

test('setPropAtPath deletes the key when value is undefined', () => {
    const tree = sampleTree();
    const next = setPropAtPath(tree, [0, 0, 1, 0], 'content', undefined);
    assert.ok(!('content' in getAtPath(next, [0, 0, 1, 0])));
});

test('setPropAtPath leaves sibling branches structurally equal', () => {
    const tree = sampleTree();
    const next = setPropAtPath(tree, [0, 0, 0], 'src', '/x.png');
    assert.deepEqual(getAtPath(next, [0, 1]), getAtPath(tree, [0, 1]));
    assert.deepEqual(getAtPath(next, [0, 2]), getAtPath(tree, [0, 2]));
    assert.deepEqual(getAtPath(next, [0, 0, 1]), getAtPath(tree, [0, 0, 1]));
});

// ── setStylePropAtPath ───────────────────────────────────────────────────────

test('setStylePropAtPath adds, overwrites and clears style entries', () => {
    const tree = sampleTree();
    const withStyle = setStylePropAtPath(tree, [0, 2], 'padding', '1rem');
    assert.equal(getAtPath(withStyle, [0, 2]).style.padding, '1rem');

    const overwritten = setStylePropAtPath(withStyle, [0, 2], 'padding', '2rem');
    assert.equal(getAtPath(overwritten, [0, 2]).style.padding, '2rem');

    const cleared = setStylePropAtPath(overwritten, [0, 2], 'padding', undefined);
    assert.ok(!('style' in getAtPath(cleared, [0, 2])), 'style object dropped once empty');
});

// ── removeAtPath ─────────────────────────────────────────────────────────────

test('removeAtPath removes a top-level element', () => {
    // sampleTree()'s single top-level entry is the outer wrapper; give it a genuine sibling
    // so this actually exercises path.length === 1, not just nested removal.
    const tree = [...sampleTree(), { type: 'vsm-feed', dataVar: 'other_log' }];
    const next = removeAtPath(tree, [1]);
    assert.equal(next.length, 1);
    assert.equal(next[0].type, 'vsm-panel');
});

test('removeAtPath removes a deeply nested element and preserves its sibling', () => {
    const tree = sampleTree();
    const next = removeAtPath(tree, [0, 0, 1, 0]); // "Alex" text
    const nameBlock = getAtPath(next, [0, 0, 1]);
    assert.equal(nameBlock.children.length, 1);
    assert.equal(nameBlock.children[0].content, 'Digital Intake Assistant');
});

test('removeAtPath is a no-op on the empty path', () => {
    const tree = sampleTree();
    assert.equal(removeAtPath(tree, []), tree);
});

// ── insertChildAtPath ────────────────────────────────────────────────────────

test('insertChildAtPath appends a new top-level element when containerPath is empty', () => {
    const tree = sampleTree();
    const next = insertChildAtPath(tree, [], { type: 'sl-text', content: 'Footer note' });
    assert.equal(next.length, 2);
    assert.equal(next[1].content, 'Footer note');
});

test('insertChildAtPath appends into an existing nested panel', () => {
    const tree = sampleTree();
    const next = insertChildAtPath(tree, [0, 0], { type: 'sl-button', label: 'Mute' });
    const header = getAtPath(next, [0, 0]);
    assert.equal(header.children.length, 3);
    assert.equal(header.children[2].label, 'Mute');
});

test('insertChildAtPath works on a container with no children yet', () => {
    const tree = [{ type: 'vsm-panel' }]; // no `children` key at all
    const next = insertChildAtPath(tree, [0], { type: 'sl-text', content: 'First' });
    assert.deepEqual(next[0].children, [{ type: 'sl-text', content: 'First' }]);
});

// ── moveAtPath ────────────────────────────────────────────────────────────────

test('moveAtPath swaps two top-level elements', () => {
    const tree = [...sampleTree(), { type: 'vsm-feed', dataVar: 'other_log' }];
    const next = moveAtPath(tree, [0], +1);
    assert.equal(next[0].type, 'vsm-feed');
    assert.equal(next[1].type, 'vsm-panel');
});

test('moveAtPath swaps two nested siblings', () => {
    const tree = sampleTree();
    const next = moveAtPath(tree, [0, 0, 1, 0], +1); // "Alex" <-> "Digital Intake Assistant"
    const nameBlock = getAtPath(next, [0, 0, 1]);
    assert.equal(nameBlock.children[0].content, 'Digital Intake Assistant');
    assert.equal(nameBlock.children[1].content, 'Alex');
});

test('moveAtPath is a no-op past either boundary', () => {
    const tree = sampleTree();
    assert.equal(moveAtPath(tree, [0], -1), tree); // only top-level element, already first
    assert.equal(moveAtPath(tree, [0, 2], +1), tree); // footer is the last of header/feed/footer
});

// ── immutability under a frozen input ────────────────────────────────────────

test('every operation tolerates (and never mutates) a deep-frozen tree', () => {
    const tree = deepFreeze(sampleTree());
    assert.doesNotThrow(() => {
        setPropAtPath(tree, [0, 0, 0], 'src', '/x.png');
        setStylePropAtPath(tree, [0, 2], 'padding', '1rem');
        removeAtPath(tree, [0, 0, 1, 0]);
        insertChildAtPath(tree, [0, 0], { type: 'sl-text', content: 'x' });
        insertChildAtPath(tree, [], { type: 'sl-text', content: 'x' });
        moveAtPath(tree, [0, 2], -1);
    });
});

// ── pathKey ──────────────────────────────────────────────────────────────────

test('pathKey gives distinct paths distinct keys, and the same path the same key', () => {
    assert.equal(pathKey([0, 2, 1]), pathKey([0, 2, 1]));
    assert.notEqual(pathKey([0, 2, 1]), pathKey([0, 21]));
    assert.notEqual(pathKey([1, 2]), pathKey([1, 2, 0]));
});

test('pathKey works as a Set membership key for tracking expanded rows', () => {
    const expanded = new Set([pathKey([0]), pathKey([0, 1])]);
    assert.ok(expanded.has(pathKey([0, 1])));
    assert.ok(!expanded.has(pathKey([0, 2])));
});
