<script>
  import { onMount, onDestroy } from "svelte";
  import ActionForm from "./ActionForm.svelte";

  // Display title for a command's declared uiCategory (plugin-properties.json).
  const CATEGORY_LABELS = { background: "Background", emotion: "Emotion", gesture: "Animation", camera: "Camera" };
  // The SIA preview is a curated character-preview surface, not a general command palette — only
  // these categories make sense here (bare actions like "stop"/"clearemotion" with no uiCategory
  // are authored via the PlayAction command helper or Ctrl+I's InsertActionDialog instead, both of
  // which already show every declared command). Animation/Camera are reserved slots: no plugin
  // declares uiCategory "gesture" or "camera" yet, so those columns simply won't appear until one
  // does — no separate "coming soon" placeholder needed. Fixed order (not first-seen) keeps the
  // layout deterministic regardless of how a plugin lists its commands.
  const SIA_VISIBLE_CATEGORIES = ["background", "emotion", "gesture", "camera"];
  function categoryLabel(category) {
    return CATEGORY_LABELS[category] || (category.charAt(0).toUpperCase() + category.slice(1));
  }

  // Groups a plugin's declared commands by uiCategory, restricted to SIA_VISIBLE_CATEGORIES and
  // ordered to match it — this is what replaced the old hardcoded Background/Emotion/Animation
  // columns: any previewCapable plugin's own commands[] now drives the panel directly, no
  // per-action-type Svelte component needed.
  function groupCommandsByCategory(commands) {
    const groups = {};
    for (const cmd of commands || []) {
      const category = cmd?.uiCategory;
      if (!category || !SIA_VISIBLE_CATEGORIES.includes(category)) continue;
      if (!groups[category]) groups[category] = [];
      groups[category].push(cmd);
    }
    return SIA_VISIBLE_CATEGORIES
      .filter((category) => groups[category]?.length)
      .map((category) => ({ category, commands: groups[category] }));
  }

  export let projectId = null;
  export let apiGet;
  export let apiPost;
  export let agents = [];        // [{agentName, instanceName, commands}] — previewCapableAgents
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
  // than CSS aspect-ratio — plain arithmetic, one less moving CSS mechanism to reason about. This
  // is only an ESTIMATE (CARD_CHROME_HEIGHT is approximate) used solely to size the avatar's own
  // width attractively — it must NOT be used as the "available height" for the column-wrap
  // decision below, since any drift between this estimate and the real rendered height caused the
  // JS column-count prediction to diverge from what CSS actually wraps to (reported 2026-07-23:
  // .sia-columns reserved width for 2 columns while CSS rendered only 1, leaving a blank gap).
  const CARD_CHROME_HEIGHT = 64; // card header (now with 32px icon buttons) + body padding, approximate
  $: avatarHeight = Math.max(0, height - CARD_CHROME_HEIGHT);
  $: avatarWidth = Math.round(avatarHeight * (2 / 3));

  // .sia-columns wraps a plugin's category columns into new columns (rather than scrolling) once
  // they don't fit the avatar's height — CSS flex-wrap handles that placement, but a shrink-to-fit
  // ancestor (.sia-card, sized by flex-basis:auto) doesn't reliably grow to match the wrapped
  // result across browsers (reported 2026-07-21 — the second/third column was clipped by
  // .sia-card's own overflow:hidden instead of growing the card). Computing the same greedy
  // column-packing in JS and applying it as an explicit width sidesteps that ambiguity entirely —
  // but only if it packs against the SAME available height CSS itself wraps against, hence
  // measuredAvatarHeight (real clientHeight, bound below) rather than the CARD_CHROME_HEIGHT
  // estimate above. Falls back to the estimate for the one frame before the binding settles.
  // Measuring each column's own height is safe here (unlike the earlier sticky-header case) since
  // nothing feeds this measurement back into changing that column's own size.
  const SIA_COLUMN_WIDTH = 220;
  const SIA_COLUMN_GAP = 10; // ~0.6rem
  let measuredAvatarHeight = 0;
  // {[`${instanceName}:${category}`]: heightPx} — one entry per rendered column, replacing the
  // old three fixed background/emotion/animation variables now that the column list is dynamic
  // (driven by whatever categories an agent's own commands declare).
  let columnHeights = {};
  $: measuredMinHeight = Math.max(CARD_CHROME_HEIGHT, ...Object.values(columnHeights), 0);

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

  // Each agent's own columns pack independently — columnHeights is one flat map shared across
  // every card (keyed "instanceName:category"), so packing against ALL of Object.values(...) at
  // once summed every OTHER agent's category heights into this agent's own wrap decision, wildly
  // over-reserving width for cards that only have 1-2 real categories (reported 2026-07-23: a
  // small panel showed columnsWidth sized for 4 columns when each card only ever has at most 2).
  // Filtering to this instance's own keys first is what actually scopes the packing per-card.
  //
  // This MUST be a `$:` block computing a plain object, not a plain function called from the
  // template (`{columnsWidthFor(agent.instanceName)}`) — Svelte's reactivity is based on static
  // analysis of which reactive variables a `$:` statement's OWN source text references; a
  // template expression that only textually mentions `agent.instanceName` never re-runs when
  // columnHeights/measuredAvatarHeight change deep inside a separately-defined function's body,
  // so the width silently froze at whatever it computed on the very first render (before any
  // column had actually been measured yet) and never updated again (reported 2026-07-23, same
  // session as the per-agent scoping fix above — a second, independent cause of the same symptom).
  $: columnsWidthByInstance = Object.fromEntries(agents.map((a) => {
    const prefix = `${a.instanceName}:`;
    const heights = Object.entries(columnHeights)
      .filter(([key]) => key.startsWith(prefix))
      .map(([, h]) => h);
    const n = computeColumnsNeeded(heights, measuredAvatarHeight || avatarHeight);
    return [a.instanceName, n * SIA_COLUMN_WIDTH + Math.max(0, n - 1) * SIA_COLUMN_GAP];
  }));

  // Per-agent avatar state, mirroring what CharacterPreviewPanel used to track per floating
  // window (M9) minus any position/size/z-order — undefined means "never attempted", so unload
  // (which resets an entry back to undefined) makes the next Load redo the full sequence rather
  // than instantly resuming.
  let previewUrls = {};
  let loadErrors = {};
  let loadingFlags = {};
  let iframeEls = {};
  let wasSuspended = false;
  // {[`${instanceName}:${category}`]: commandText} — the currently built command for whichever
  // command is selected in that category's column (selectedCommand below picks which one, for
  // categories with more than one command, e.g. a future gesture list).
  let commandTexts = {};
  // {[`${instanceName}:${category}`]: commandName} — which of a category's commands is active;
  // undefined means "not chosen yet", defaulted to the category's first command at render time.
  let selectedCommand = {};
  let testingCategory = {}; // {[`${instanceName}:${category}`]: boolean}
  let categoryTestErrors = {}; // {[`${instanceName}:${category}`]: string}
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

  function columnKey(instanceName, category) {
    return `${instanceName}:${category}`;
  }

  // Root cause of a severe hang (reported 2026-07-20/21): ParameterEnvelopeEditor/
  // BackgroundColorEditor's own `$: if (currentCommand) onChange?.(currentCommand);` (now also
  // in ActionForm) treats the onChange PROP REFERENCE as a dependency, not just currentCommand's
  // value — so an inline onChange that unconditionally reassigns state recreates a new closure
  // every call, which re-triggers that reactive statement, which reassigns state again, forever,
  // synchronously, never yielding back to the browser. Guarding the reassignment so it's a no-op
  // once the value has actually settled breaks the cycle after one pass instead of looping.
  function setCommandText(instanceName, category, cmd) {
    const key = columnKey(instanceName, category);
    if (commandTexts[key] === cmd) return;
    commandTexts = { ...commandTexts, [key]: cmd };
  }

  function activeCommandName(instanceName, category, commands) {
    const key = columnKey(instanceName, category);
    const selected = selectedCommand[key];
    return (selected && commands.some((c) => c.name === selected)) ? selected : commands[0]?.name;
  }

  function selectCommand(instanceName, category, name) {
    selectedCommand = { ...selectedCommand, [columnKey(instanceName, category)]: name };
  }

  // Play button lives in the column header (beside the category title) rather than at the bottom
  // of ActionForm's own sub-editor — hides that component's own built-in test row (below, scoped
  // to .sia-column so InsertActionDialog's own use of ActionForm/ParameterEnvelopeEditor is
  // unaffected) and drives the same testActionFor(...) call from here instead, using the command
  // already captured via setCommandText.
  async function playCategory(instanceName, category) {
    const key = columnKey(instanceName, category);
    const cmd = commandTexts[key];
    if (!cmd || testingCategory[key] || isTestDisabled(instanceName)) return;
    testingCategory = { ...testingCategory, [key]: true };
    categoryTestErrors = { ...categoryTestErrors, [key]: "" };
    try {
      await testActionFor(instanceName, cmd);
    } catch (err) {
      categoryTestErrors = { ...categoryTestErrors, [key]: err?.message || "Failed to test" };
    } finally {
      testingCategory = { ...testingCategory, [key]: false };
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
        const path = data.previewPath || "/character.html";
        if (data.previewPathPrefix) {
          // Nginx-routed deployment (PortPoolManager's _pathPrefix, Option C — doc/vsm-
          // workspace-platform-plan.md Phase 5 follow-up): route through inner-nginx, on this
          // same origin, instead of a raw port the browser may not be able to reach directly.
          const rest = path.replace(/^\/+/, "");
          const scheme = window.location.protocol === "https:" ? "https" : "http";
          previewUrls = {
            ...previewUrls,
            [instanceName]: `${scheme}://${host}${data.previewPathPrefix}port/${rest}`
          };
        } else {
          const scheme = data.previewSecure ? "https" : "http";
          previewUrls = {
            ...previewUrls,
            [instanceName]: `${scheme}://${host}:${data.previewPort}${path}`
          };
        }
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
              {#if mutedFlags[agent.instanceName]}
                <svg viewBox="0 0 24 24" width="16" height="16" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                  <path stroke-linecap="round" stroke-linejoin="round" d="M17.25 9.75 19.5 12m0 0 2.25 2.25M19.5 12l2.25-2.25M19.5 12l-2.25 2.25m-10.5-6 4.72-4.72a.75.75 0 0 1 1.28.53v15.88a.75.75 0 0 1-1.28.53l-4.72-4.72H4.51c-.88 0-1.704-.507-1.938-1.354A9.009 9.009 0 0 1 2.25 12c0-.83.112-1.633.322-2.396C2.806 8.756 3.63 8.25 4.51 8.25H6.75Z" />
                </svg>
              {:else}
                <svg viewBox="0 0 24 24" width="16" height="16" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                  <path stroke-linecap="round" stroke-linejoin="round" d="M19.114 5.636a9 9 0 0 1 0 12.728M16.463 8.288a5.25 5.25 0 0 1 0 7.424M6.75 8.25l4.72-4.72a.75.75 0 0 1 1.28.53v15.88a.75.75 0 0 1-1.28.53l-4.72-4.72H4.51c-.88 0-1.704-.507-1.938-1.354A9.009 9.009 0 0 1 2.25 12c0-.83.112-1.633.322-2.396C2.806 8.756 3.63 8.25 4.51 8.25H6.75Z" />
                </svg>
              {/if}
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
          <div class="sia-avatar" style:width="{avatarWidth}px" bind:clientHeight={measuredAvatarHeight}>
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
          <div class="sia-columns" style:width="{columnsWidthByInstance[agent.instanceName]}px">
            {#each groupCommandsByCategory(agent.commands) as group (group.category)}
              {@const key = columnKey(agent.instanceName, group.category)}
              {@const activeName = activeCommandName(agent.instanceName, group.category, group.commands)}
              {@const activeSchema = group.commands.find((c) => c.name === activeName) || group.commands[0]}
              <div class="sia-column" bind:clientHeight={columnHeights[key]}>
                <div class="sia-column-header">
                  <span>{categoryLabel(group.category)}</span>
                  <button
                    type="button"
                    class="sia-column-play"
                    disabled={!commandTexts[key] || isTestDisabled(agent.instanceName) || testingCategory[key]}
                    title={isTestDisabled(agent.instanceName) ? "Character isn't loaded yet" : "Play on preview"}
                    aria-label="Play on preview"
                    on:click={() => playCategory(agent.instanceName, group.category)}
                  >
                    <svg viewBox="0 0 24 24" width="16" height="16" fill="currentColor" aria-hidden="true"><path d="M8 6l10 6-10 6V6z" /></svg>
                  </button>
                </div>
                {#if group.commands.length > 1}
                  <select
                    class="sia-column-command-select"
                    value={activeName}
                    on:change={(e) => selectCommand(agent.instanceName, group.category, e.target.value)}
                  >
                    {#each group.commands as cmd}
                      <option value={cmd.name}>{cmd.summary || cmd.name}</option>
                    {/each}
                  </select>
                {/if}
                {#if activeSchema}
                  <ActionForm
                    schema={activeSchema}
                    disabled={isTestDisabled(agent.instanceName)}
                    onTest={(cmd) => testActionFor(agent.instanceName, cmd)}
                    onChange={(cmd) => setCommandText(agent.instanceName, group.category, cmd)}
                  />
                {/if}
                {#if categoryTestErrors[key]}
                  <span class="sia-column-error">{categoryTestErrors[key]}</span>
                {/if}
                <button
                  type="button"
                  class="sia-insert-btn"
                  disabled={!commandTexts[key]}
                  on:click={() => onInsertAtCursor?.(agent.instanceName, agent.agentName, commandTexts[key])}
                >
                  Insert at cursor
                </button>
              </div>
            {/each}
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
    /* .script-sticky-header (App.svelte) is a CSS grid, whose default justify-items:stretch
       makes this wrapper fill the whole grid track regardless of how much width its cards
       actually need — a tall panel (wide 2:3 avatars) then leaves a large empty band to the
       right of the last card (reported 2026-07-22). Shrink-to-fit the content instead; cap at
       100% so it still never overflows the row when enough cards ARE present to need the full
       width — .sia-panel-scroll's own overflow-x:auto takes over beyond that. */
    width: fit-content;
    max-width: 100%;
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
    font-size: 0.85rem;
    font-weight: 700;
    color: var(--muted);
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }

  .sia-card-mute,
  .sia-card-unload {
    width: 32px;
    height: 32px;
    flex-shrink: 0;
    background: transparent;
    border: 1px solid var(--stroke);
    border-radius: 4px;
    cursor: pointer;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 0;
    font-size: 1.1rem;
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
    font-size: 0.85rem;
    font-weight: 700;
    color: var(--muted);
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }

  /* Play button lives beside the column heading rather than at the bottom of the sub-editor —
     hides that component's own built-in test row (below) and drives testActionFor from here
     instead, scoped to .sia-column so InsertActionDialog's own use of these components (where the
     bottom placement still makes sense) is unaffected. */
  .sia-column-play {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 32px;
    height: 32px;
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
  .sia-column :global(.bce-test-row),
  .sia-column :global(.af-test-row) {
    display: none;
  }

  .sia-column-command-select {
    font-size: 0.8rem;
  }

  .sia-column-error {
    font-size: 0.72rem;
    color: var(--danger);
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
