<script>
  import { onMount, onDestroy } from "svelte";
  import BackgroundColorEditor from "./BackgroundColorEditor.svelte";
  import ParameterEnvelopeEditor from "./ParameterEnvelopeEditor.svelte";
  import { EMOTION_TYPES } from "./emotionTypes.js";

  export let projectId = null;
  export let apiGet;
  export let apiPost;
  export let agents = [];        // [{agentName, instanceName}] — previewCapableAgents
  export let loaded = {};        // {[instanceName]: boolean}
  export let loadProgress = {};  // {[instanceName]: 0-100} — from vm.progress postMessage
  export let anySpeaking = false;
  export let suspended = false;  // a real SceneFlow run is active — see CharacterPreviewPanel's
                                  // original comment on why the iframe is torn down rather than
                                  // just muted (two live engine sessions under one license break
                                  // both, confirmed 2026-07-18)
  export let height = 420;
  export let measuredMinHeight = 300; // bindable — reported up so the resize drag can clamp against it
  export let onLoad = null;       // (instanceName) => void
  export let onUnload = null;     // (instanceName) => void
  export let onProgress = null;   // (instanceName, value) => void
  export let onSpeaking = null;   // (instanceName, speaking) => void
  export let onResizeStart = null;    // (event) => void — drags the panel's own height
  export let onInsertAtCursor = null; // (instanceName, agentName, commandBody) => void

  // Avatar width follows a fixed 2:3 (width:height) ratio, computed from the panel height rather
  // than CSS aspect-ratio — plain arithmetic, one less moving CSS mechanism to reason about.
  const CARD_CHROME_HEIGHT = 50; // card header + body padding, approximate
  $: avatarHeight = Math.max(0, height - CARD_CHROME_HEIGHT);
  $: avatarWidth = Math.round(avatarHeight * (2 / 3));

  // .sia-columns wraps Background/Emotion/Animation into new columns (rather than scrolling) once
  // they don't fit the avatar's height — CSS flex-wrap handles that placement, but a shrink-to-fit
  // ancestor (.sia-card, sized by flex-basis:auto) doesn't reliably grow to match the wrapped
  // result across browsers (reported 2026-07-21 — the second/third column was clipped by
  // .sia-card's own overflow:hidden instead of growing the card). Computing the same greedy
  // column-packing in JS and applying it as an explicit width sidesteps that ambiguity entirely.
  // Measuring each column's own height is safe here (unlike the earlier sticky-header case) since
  // nothing feeds this measurement back into changing that column's own size.
  const SIA_COLUMN_WIDTH = 220;
  const SIA_COLUMN_GAP = 10; // ~0.6rem
  let backgroundSectionHeight = 0;
  let emotionSectionHeight = 0;
  let animationSectionHeight = 0;
  $: columnsNeeded = computeColumnsNeeded(
    [backgroundSectionHeight, emotionSectionHeight, animationSectionHeight],
    avatarHeight
  );
  $: columnsWidth = columnsNeeded * SIA_COLUMN_WIDTH + Math.max(0, columnsNeeded - 1) * SIA_COLUMN_GAP;
  $: measuredMinHeight = Math.max(
    CARD_CHROME_HEIGHT,
    backgroundSectionHeight,
    emotionSectionHeight,
    animationSectionHeight
  );

  function computeColumnsNeeded(sectionHeights, available) {
    if (!(available > 0)) return sectionHeights.length || 1;
    let columns = 1;
    let used = 0;
    for (const h of sectionHeights) {
      const needed = h + SIA_COLUMN_GAP;
      if (used > 0 && used + needed > available) {
        columns += 1;
        used = needed;
      } else {
        used += needed;
      }
    }
    return columns;
  }

  // Per-agent avatar state, mirroring what CharacterPreviewPanel used to track per floating
  // window (M9) minus any position/size/z-order — undefined means "never attempted", so unload
  // (which resets an entry back to undefined) makes the next Load redo the full sequence rather
  // than instantly resuming.
  let previewUrls = {};
  let loadErrors = {};
  let loadingFlags = {};
  let iframeEls = {};
  let wasSuspended = false;
  let backgroundCommands = {};
  let emotionCommands = {};
  let mutedFlags = {}; // {[instanceName]: boolean} — client-side only, see vm-adapter.js's mute section

  // The server broadcasts every speak/action command identically to every connected preview
  // session (JettyTransport.send()), so two browsers previewing the same character at once each
  // hear the other's tests. Muting here only silences audio in THIS tab via a postMessage into
  // the iframe (vm-adapter.js) — it doesn't touch dispatch, so the visuals/other viewers are
  // unaffected. Confirmed 2026-07-21.
  function toggleMute(instanceName) {
    const next = !mutedFlags[instanceName];
    mutedFlags = { ...mutedFlags, [instanceName]: next };
    iframeEls[instanceName]?.contentWindow?.postMessage({ vsmMute: next }, "*");
  }

  // Root cause of a severe hang (reported 2026-07-20/21): ParameterEnvelopeEditor/
  // BackgroundColorEditor's own `$: if (currentCommand) onChange?.(currentCommand);` treats the
  // onChange PROP REFERENCE as a dependency, not just currentCommand's value — so an inline
  // onChange that unconditionally reassigns *Commands recreates a new closure every call, which
  // re-triggers that reactive statement, which reassigns *Commands again, forever, synchronously,
  // never yielding back to the browser. Guarding the reassignment so it's a no-op once the value
  // has actually settled breaks the cycle after one pass instead of looping.
  function setBackgroundCommand(instanceName, cmd) {
    if (backgroundCommands[instanceName] === cmd) return;
    backgroundCommands = { ...backgroundCommands, [instanceName]: cmd };
  }

  function setEmotionCommand(instanceName, cmd) {
    if (emotionCommands[instanceName] === cmd) return;
    emotionCommands = { ...emotionCommands, [instanceName]: cmd };
  }

  // Play buttons live in the column header (beside "Background"/"Emotion") rather than at the
  // bottom of each sub-editor, so hide those components' own built-in test row (below, scoped to
  // .sia-column so ActionCommandModal's own use of the same components is unaffected) and drive
  // the same testActionFor(...) call from here instead, using the command already captured via
  // setBackgroundCommand/setEmotionCommand.
  let testingBackground = {};
  let testingEmotion = {};
  let backgroundTestErrors = {};
  let emotionTestErrors = {};

  async function playBackground(instanceName) {
    const cmd = backgroundCommands[instanceName];
    if (!cmd || testingBackground[instanceName] || isTestDisabled(instanceName)) return;
    testingBackground = { ...testingBackground, [instanceName]: true };
    backgroundTestErrors = { ...backgroundTestErrors, [instanceName]: "" };
    try {
      await testActionFor(instanceName, cmd);
    } catch (err) {
      backgroundTestErrors = { ...backgroundTestErrors, [instanceName]: err?.message || "Failed to test" };
    } finally {
      testingBackground = { ...testingBackground, [instanceName]: false };
    }
  }

  async function playEmotion(instanceName) {
    const cmd = emotionCommands[instanceName];
    if (!cmd || testingEmotion[instanceName] || isTestDisabled(instanceName)) return;
    testingEmotion = { ...testingEmotion, [instanceName]: true };
    emotionTestErrors = { ...emotionTestErrors, [instanceName]: "" };
    try {
      await testActionFor(instanceName, cmd);
    } catch (err) {
      emotionTestErrors = { ...emotionTestErrors, [instanceName]: err?.message || "Failed to test" };
    } finally {
      testingEmotion = { ...testingEmotion, [instanceName]: false };
    }
  }

  function registerIframe(node, instanceName) {
    iframeEls[instanceName] = node;
    return {
      destroy() {
        if (iframeEls[instanceName] === node) delete iframeEls[instanceName];
      }
    };
  }

  function handlePreviewMessage(event) {
    for (const instanceName of Object.keys(iframeEls)) {
      const el = iframeEls[instanceName];
      if (!el || event.source !== el.contentWindow) continue;
      const progress = event.data?.vsmPreviewProgress;
      if (typeof progress === "number") onProgress?.(instanceName, progress);
      const speaking = event.data?.vsmSpeaking;
      if (typeof speaking === "boolean") onSpeaking?.(instanceName, speaking);
      // Re-assert on every message from this frame (progress ticks while loading, then speaking
      // changes) rather than only once on load — postMessage sent right after the iframe mounts
      // can arrive before vm-adapter.js's own listener is registered and be silently lost, but by
      // the time IT sends anything back to us, that listener is guaranteed to already be live.
      if (mutedFlags[instanceName]) {
        el.contentWindow?.postMessage({ vsmMute: true }, "*");
      }
      return;
    }
  }

  onMount(() => {
    window.addEventListener("message", handlePreviewMessage);
  });
  onDestroy(() => {
    window.removeEventListener("message", handlePreviewMessage);
  });

  async function loadPreviewInfo(instanceName) {
    loadingFlags = { ...loadingFlags, [instanceName]: true };
    loadErrors = { ...loadErrors, [instanceName]: "" };
    previewUrls = { ...previewUrls, [instanceName]: null };
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/plugins/${instanceName}/preview`);
      // Build against the host this browser actually used to reach the server (not
      // "localhost", which would only work for whoever is running VSM itself) — same
      // reasoning as CharacterPreviewPanel's original loadPreviewInfo.
      if (data?.previewPort) {
        const host = (typeof window !== "undefined" && window.location.hostname) || "localhost";
        const scheme = data.previewSecure ? "https" : "http";
        previewUrls = {
          ...previewUrls,
          [instanceName]: `${scheme}://${host}:${data.previewPort}${data.previewPath || "/character.html"}`
        };
      } else {
        previewUrls = { ...previewUrls, [instanceName]: null };
      }
    } catch (err) {
      loadErrors = { ...loadErrors, [instanceName]: err?.message || "Failed to load preview" };
    } finally {
      loadingFlags = { ...loadingFlags, [instanceName]: false };
    }
  }

  // Fetch for any agent that's marked loaded but hasn't been attempted yet (a fresh Load click,
  // or a resume after suspension reset it back to undefined below).
  $: for (const agent of agents) {
    if (loaded[agent.instanceName] && !suspended && previewUrls[agent.instanceName] === undefined) {
      loadPreviewInfo(agent.instanceName);
    }
  }

  // Tear every loaded iframe down for the run's duration; the reactive block above re-fetches
  // whatever's still marked loaded once suspended goes back to false.
  $: if (suspended !== wasSuspended) {
    wasSuspended = suspended;
    if (suspended) {
      const cleared = {};
      for (const instanceName of Object.keys(previewUrls)) cleared[instanceName] = undefined;
      previewUrls = { ...previewUrls, ...cleared };
      loadingFlags = {};
      loadErrors = {};
    }
  }

  function testActionFor(instanceName, command) {
    return apiPost(`/api/v1/projects/${projectId}/plugins/${instanceName}/preview/action`, { command });
  }

  function isTestDisabled(instanceName) {
    return suspended || (loadProgress[instanceName] ?? 0) < 100 || anySpeaking;
  }
</script>

<div class="sia-panel-wrap" style:height="{height}px">
  <div class="sia-panel-scroll">
    {#each agents as agent (agent.instanceName)}
      {@const isLoaded = !!loaded[agent.instanceName]}
      <div class="sia-card" class:sia-card-unloaded={!isLoaded}>
        <div class="sia-card-header">
          <span class="sia-card-name">{agent.agentName}</span>
          {#if isLoaded}
            <button
              type="button"
              class="sia-card-mute"
              class:sia-card-mute-active={mutedFlags[agent.instanceName]}
              on:click={() => toggleMute(agent.instanceName)}
              title={mutedFlags[agent.instanceName] ? "Unmute audio in this preview" : "Mute audio in this preview"}
              aria-label={mutedFlags[agent.instanceName] ? "Unmute audio in this preview" : "Mute audio in this preview"}
            >
              {mutedFlags[agent.instanceName] ? "🔇" : "🔊"}
            </button>
            <button
              type="button"
              class="sia-card-unload"
              on:click={() => onUnload?.(agent.instanceName)}
              title="Unload {agent.agentName}"
              aria-label="Unload {agent.agentName}"
            >
              &times;
            </button>
          {/if}
        </div>
        <div class="sia-card-body">
          <div class="sia-avatar" style:width="{avatarWidth}px">
            {#if !isLoaded}
              <div class="sia-avatar-placeholder"></div>
            {:else if suspended}
              <div class="sia-avatar-message">A SceneFlow run is active — preview paused.</div>
            {:else if loadErrors[agent.instanceName]}
              <div class="sia-avatar-message sia-avatar-error">{loadErrors[agent.instanceName]}</div>
            {:else if loadingFlags[agent.instanceName] || previewUrls[agent.instanceName] === undefined}
              <div class="sia-avatar-message">Loading… {loadProgress[agent.instanceName] ?? 0}%</div>
            {:else if previewUrls[agent.instanceName]}
              <iframe
                class="sia-avatar-frame"
                src={previewUrls[agent.instanceName]}
                title="{agent.agentName} preview"
                allow="autoplay"
                use:registerIframe={agent.instanceName}
              ></iframe>
            {:else}
              <div class="sia-avatar-message">No preview page available.</div>
            {/if}
          </div>
          <div class="sia-columns" style:width="{columnsWidth}px">
            <div class="sia-column" bind:clientHeight={backgroundSectionHeight}>
              <div class="sia-column-header">
                <span>Background</span>
                <button
                  type="button"
                  class="sia-column-play"
                  disabled={!backgroundCommands[agent.instanceName] || isTestDisabled(agent.instanceName) || testingBackground[agent.instanceName]}
                  title={isTestDisabled(agent.instanceName) ? "Character isn't loaded yet" : "Play on preview"}
                  aria-label="Play on preview"
                  on:click={() => playBackground(agent.instanceName)}
                >
                  <svg viewBox="0 0 24 24" width="12" height="12" fill="currentColor" aria-hidden="true"><path d="M8 6l10 6-10 6V6z" /></svg>
                </button>
              </div>
              <BackgroundColorEditor
                disabled={isTestDisabled(agent.instanceName)}
                onTest={(cmd) => testActionFor(agent.instanceName, cmd)}
                onChange={(cmd) => setBackgroundCommand(agent.instanceName, cmd)}
              />
              {#if backgroundTestErrors[agent.instanceName]}
                <span class="sia-column-error">{backgroundTestErrors[agent.instanceName]}</span>
              {/if}
              <button
                type="button"
                class="sia-insert-btn"
                disabled={!backgroundCommands[agent.instanceName]}
                on:click={() => onInsertAtCursor?.(agent.instanceName, agent.agentName, backgroundCommands[agent.instanceName])}
              >
                Insert at cursor
              </button>
            </div>
            <div class="sia-column" bind:clientHeight={emotionSectionHeight}>
              <div class="sia-column-header">
                <span>Emotion</span>
                <button
                  type="button"
                  class="sia-column-play"
                  disabled={!emotionCommands[agent.instanceName] || isTestDisabled(agent.instanceName) || testingEmotion[agent.instanceName]}
                  title={isTestDisabled(agent.instanceName) ? "Character isn't loaded yet" : "Play on preview"}
                  aria-label="Play on preview"
                  on:click={() => playEmotion(agent.instanceName)}
                >
                  <svg viewBox="0 0 24 24" width="12" height="12" fill="currentColor" aria-hidden="true"><path d="M8 6l10 6-10 6V6z" /></svg>
                </button>
              </div>
              <ParameterEnvelopeEditor
                actionName="emotion"
                typeOptions={EMOTION_TYPES}
                disabled={isTestDisabled(agent.instanceName)}
                onTest={(cmd) => testActionFor(agent.instanceName, cmd)}
                onChange={(cmd) => setEmotionCommand(agent.instanceName, cmd)}
              />
              {#if emotionTestErrors[agent.instanceName]}
                <span class="sia-column-error">{emotionTestErrors[agent.instanceName]}</span>
              {/if}
              <button
                type="button"
                class="sia-insert-btn"
                disabled={!emotionCommands[agent.instanceName]}
                on:click={() => onInsertAtCursor?.(agent.instanceName, agent.agentName, emotionCommands[agent.instanceName])}
              >
                Insert at cursor
              </button>
            </div>
            <div class="sia-column sia-column-disabled" bind:clientHeight={animationSectionHeight}>
              <div class="sia-column-header">
                <span>Animation</span>
                <button type="button" class="sia-column-play" disabled title="Coming soon" aria-label="Coming soon">
                  <svg viewBox="0 0 24 24" width="12" height="12" fill="currentColor" aria-hidden="true"><path d="M8 6l10 6-10 6V6z" /></svg>
                </button>
              </div>
              <div class="sia-column-todo">Coming soon</div>
            </div>
          </div>
          {#if !isLoaded}
            <div class="sia-load-overlay">
              <button type="button" class="sia-load-btn" on:click={() => onLoad?.(agent.instanceName)}>
                Load
              </button>
            </div>
          {/if}
        </div>
      </div>
    {/each}
  </div>
  <div
    class="sia-resize-handle"
    aria-hidden="true"
    on:pointerdown|stopPropagation={(e) => onResizeStart?.(e)}
    on:mousedown|stopPropagation={(e) => onResizeStart?.(e)}
  ></div>
</div>

<style>
  .sia-panel-wrap {
    position: relative;
    box-sizing: border-box;
    border: 1px solid var(--stroke);
    border-radius: 12px;
    background: #ffffff;
    box-shadow: 0 4px 14px rgba(0, 0, 0, 0.08);
    overflow: hidden;
  }

  .sia-panel-scroll {
    display: flex;
    align-items: stretch;
    gap: 0.6rem;
    height: 100%;
    padding: 0.6rem;
    overflow-x: auto;
    /* Setting overflow-x alone makes the browser compute overflow-y as auto too (same quirk
       fixed for the parameter sliders earlier this session) — explicitly keep this axis hidden
       so a few stray sub-pixels of vertical overflow don't spawn an unwanted scrollbar
       (reported 2026-07-21). */
    overflow-y: hidden;
    box-sizing: border-box;
  }

  .sia-card {
    flex: 0 0 auto;
    height: 100%;
    display: flex;
    flex-direction: column;
    border: 1px solid var(--stroke);
    border-radius: 10px;
    overflow: hidden;
    background: var(--panel-soft);
  }

  .sia-card-header {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    padding: 0.35rem 0.5rem;
    font-size: 0.82rem;
    font-weight: 600;
    color: var(--ink);
    background: #f8f6f2;
    border-bottom: 1px solid var(--stroke);
    flex-shrink: 0;
  }

  .sia-card-name {
    flex: 1;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .sia-card-mute,
  .sia-card-unload {
    width: 18px;
    height: 18px;
    flex-shrink: 0;
    background: transparent;
    border: 1px solid var(--stroke);
    border-radius: 4px;
    cursor: pointer;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 0;
    font-size: 0.68rem;
    line-height: 1;
    color: var(--muted);
  }

  .sia-card-mute:hover,
  .sia-card-unload:hover {
    background: var(--panel-soft);
  }

  .sia-card-mute-active {
    background: var(--accent-soft);
    border-color: var(--accent);
  }

  .sia-card-body {
    position: relative;
    flex: 1;
    min-height: 0;
    display: flex;
    align-items: stretch;
    gap: 0.6rem;
    padding: 0.6rem;
    box-sizing: border-box;
  }

  .sia-card-unloaded .sia-card-body > .sia-avatar,
  .sia-card-unloaded .sia-card-body > .sia-columns {
    opacity: 0.4;
    pointer-events: none;
  }

  .sia-avatar {
    height: 100%;
    flex-shrink: 0;
    background: #000;
    border-radius: 6px;
    overflow: hidden;
    position: relative;
  }

  .sia-avatar-placeholder {
    width: 100%;
    height: 100%;
    background: #d8d2c8;
  }

  .sia-avatar-message {
    position: absolute;
    inset: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 0.5rem;
    font-size: 0.75rem;
    color: #ccc;
    text-align: center;
  }

  .sia-avatar-error {
    color: #ff8a80;
  }

  .sia-avatar-frame {
    width: 100%;
    height: 100%;
    border: none;
    display: block;
  }

  /* Stacked vertically (not side by side) so a card's horizontal footprint stays narrow —
     comparing two characters side by side otherwise needed too much width per card (reported
     2026-07-21). Scrolls internally if the stack is taller than the panel's current height,
     rather than growing the whole card past it. */
  /* flex-wrap in the column direction: sections stack top-to-bottom and fill the avatar's height,
     then wrap into a new column to the right (growing the card horizontally) once a section
     doesn't fit — rather than scrolling — so nothing gets clipped/hidden below the fold. */
  .sia-columns {
    display: flex;
    flex-direction: column;
    flex-wrap: wrap;
    align-content: flex-start;
    gap: 0.6rem;
  }

  .sia-column {
    width: 220px;
    flex-shrink: 0;
    display: flex;
    flex-direction: column;
    gap: 0.4rem;
  }

  .sia-column-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 0.4rem;
    font-size: 0.78rem;
    font-weight: 600;
    color: var(--muted);
    text-transform: uppercase;
    letter-spacing: 0.03em;
  }

  /* Play button lives beside the column heading rather than at the bottom of the sub-editor —
     hides that component's own built-in test row (below) and drives testActionFor from here
     instead, scoped to .sia-column so ActionCommandModal's own use of these components (where the
     bottom placement still makes sense) is unaffected. */
  .sia-column-play {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 20px;
    height: 20px;
    padding: 0;
    flex-shrink: 0;
    border-radius: var(--radius-sm);
    border: 1px solid var(--stroke);
    background: var(--panel);
    color: var(--ink);
    cursor: pointer;
    text-transform: none;
  }

  .sia-column-play:hover:not(:disabled) {
    background: var(--accent-soft);
  }

  .sia-column-play:disabled {
    color: var(--muted);
    cursor: default;
  }

  .sia-column :global(.pev-test-row),
  .sia-column :global(.bce-test-row) {
    display: none;
  }

  .sia-column-error {
    font-size: 0.72rem;
    color: var(--danger);
  }

  .sia-column-disabled {
    opacity: 0.5;
  }

  .sia-column-todo {
    font-size: 0.78rem;
    color: var(--muted);
    font-style: italic;
  }

  .sia-insert-btn {
    align-self: flex-end;
    font-size: 0.75rem;
    padding: 0.3rem 0.6rem;
    border-radius: var(--radius-sm);
    border: 1px solid var(--stroke);
    background: var(--panel);
    color: var(--ink);
    cursor: pointer;
  }

  .sia-insert-btn:hover:not(:disabled) {
    background: var(--accent-soft);
  }

  .sia-insert-btn:disabled {
    color: var(--muted);
    cursor: default;
  }

  .sia-load-overlay {
    position: absolute;
    inset: 0;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .sia-load-btn {
    font-size: 0.9rem;
    font-weight: 600;
    padding: 0.5rem 1.2rem;
    border-radius: var(--radius-sm);
    border: 1px solid var(--accent);
    background: var(--accent);
    color: #fff;
    cursor: pointer;
  }

  .sia-load-btn:hover {
    background: var(--button-pressed);
  }

  .sia-resize-handle {
    position: absolute;
    bottom: 2px;
    right: 2px;
    width: 14px;
    height: 14px;
    cursor: ns-resize;
    touch-action: none;
    background:
      linear-gradient(135deg, transparent 45%, #c0b8ae 45%, #c0b8ae 55%, transparent 55%),
      linear-gradient(135deg, transparent 65%, #c0b8ae 65%, #c0b8ae 75%, transparent 75%),
      linear-gradient(135deg, transparent 82%, #c0b8ae 82%);
    border-radius: 0 0 8px 0;
  }
</style>
