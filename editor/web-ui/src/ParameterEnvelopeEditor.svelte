<script>
  // Shared by the emotion editor now and the gesture editor later (M5) — only the
  // action name and type vocabulary differ; the envelope (type/intensity/attack/hold/decay)
  // and dispatch mechanism are identical.
  export let actionName = "emotion";
  export let typeOptions = [];
  export let onTest = null;     // async (command: string) => void — throws/rejects on failure
  export let disabled = false;  // e.g. the target character isn't loaded yet
  export let onChange = null;   // (command: string) => void — fires on every parameter change,
                                 // so a host (e.g. the M11 insert modal) can always read the
                                 // current command without duplicating the construction logic
  export let initialValues = null; // {type, intensity, attack, hold, decay, blocking} — M13d edit
                                    // mode; read once at creation (this component is recreated
                                    // fresh per modal open, so no reactive re-sync is needed)

  const DEFAULTS = { intensity: 1, attack: 200, hold: 20, decay: 300 };

  let type = initialValues?.type ?? typeOptions[0] ?? "";

  // Keeps the dropdown showing every known type, but never silently drops an existing script's
  // value even if it's a legacy/custom one no longer in typeOptions (a plain <select> would
  // otherwise fall back to its first option, quietly changing the saved command on next save).
  $: selectableTypes = type && !typeOptions.includes(type) ? [type, ...typeOptions] : typeOptions;
  let intensity = initialValues?.intensity !== undefined ? Number(initialValues.intensity) : DEFAULTS.intensity;
  let attack = initialValues?.attack !== undefined ? Number(initialValues.attack) : DEFAULTS.attack;
  let hold = initialValues?.hold !== undefined ? Number(initialValues.hold) : DEFAULTS.hold;
  let decay = initialValues?.decay !== undefined ? Number(initialValues.decay) : DEFAULTS.decay;
  // M13e: pauses the utterance's speech around this action at runtime instead of firing as a
  // fire-and-forget inline marker — see ReactivePlayer.isBlockingAction / CharamelEmbedExecutor.
  // sleepForBlockingEnvelope for the full mechanism (a runtime doc note: this checkbox has no
  // effect on background/clearEmotion, only emotion — VuppetMaster gives no completion signal
  // for a transition, so "done" there is *estimated* as attack+hold+decay+50ms, not measured).
  let blocking = initialValues?.blocking === true || initialValues?.blocking === "true";

  let testing = false;
  let testError = "";

  // Referencing type/intensity/attack/hold/decay directly (not hidden inside a called function)
  // so Svelte's static dependency analysis actually reruns this on every slider/type change —
  // a helper function call alone wouldn't have tracked its internal reads.
  // M13d: keep the written command minimal — only 'type' plus whatever differs from default —
  // so the file stays as short as possible; the compact/full toggle (M13c) is what expands it
  // back out for reading, not the stored text itself. "blocking" follows the same rule: omitted
  // entirely unless checked, so existing scripts with no blocking key keep today's non-blocking
  // (fire-and-forget) behavior with zero change.
  $: currentCommand = type.trim()
    ? [
        `${actionName} type='${type.trim()}'`,
        Number(intensity) !== DEFAULTS.intensity ? `intensity='${intensity}'` : null,
        Number(attack) !== DEFAULTS.attack ? `attack='${attack}'` : null,
        Number(hold) !== DEFAULTS.hold ? `hold='${hold}'` : null,
        Number(decay) !== DEFAULTS.decay ? `decay='${decay}'` : null,
        blocking ? `blocking='true'` : null
      ].filter(Boolean).join(" ")
    : "";
  $: if (currentCommand) onChange?.(currentCommand);

  const CURVE_W = 280;
  const CURVE_H = 70;
  const CURVE_PAD = 4;
  const CURVE_Y_MAX = 1; // fixed scale, matches the intensity slider's max (1.0 is the engine's ceiling)

  $: totalMs = Math.max(1, Number(attack) + Number(hold) + Number(decay));
  $: curvePoints = buildCurvePoints(Number(attack), Number(hold), Number(decay), Number(intensity), totalMs);

  function buildCurvePoints(a, h, d, peak, total) {
    const xScale = CURVE_W / total;
    const yScale = CURVE_H / CURVE_Y_MAX;
    const baseline = CURVE_PAD + CURVE_H;
    const point = (t, v) => `${(CURVE_PAD + t * xScale).toFixed(1)},${(baseline - Math.min(v, CURVE_Y_MAX) * yScale).toFixed(1)}`;
    return [point(0, 0), point(a, peak), point(a + h, peak), point(a + h + d, 0)].join(" ");
  }

  async function testNow() {
    if (!onTest || testing || disabled || !currentCommand) return;
    testing = true;
    testError = "";
    try {
      await onTest(currentCommand);
    } catch (err) {
      testError = err?.message || "Failed to test";
    } finally {
      testing = false;
    }
  }
</script>

<div class="pev">
  <div class="pev-row">
    <label class="pev-type-label" for="pev-type-{actionName}">Type</label>
    <select
      id="pev-type-{actionName}"
      class="pev-type-select"
      bind:value={type}
      on:pointerdown|stopPropagation
      on:mousedown|stopPropagation
    >
      {#each selectableTypes as t}
        <option value={t}>{t}</option>
      {/each}
    </select>
  </div>

  <svg class="pev-curve" viewBox="0 0 {CURVE_W + CURVE_PAD * 2} {CURVE_H + CURVE_PAD * 2}" preserveAspectRatio="none">
    <polyline points={curvePoints} fill="none" stroke="#8a6d3b" stroke-width="2" />
  </svg>

  <div class="pev-sliders">
    <label class="pev-slider-row">
      <span class="pev-slider-label">Intensity</span>
      <input type="range" min="0" max="1" step="0.05" bind:value={intensity}
        on:pointerdown|stopPropagation on:mousedown|stopPropagation />
      <span class="pev-value">{Number(intensity).toFixed(2)}</span>
    </label>
    <label class="pev-slider-row">
      <span class="pev-slider-label">Attack</span>
      <input type="range" min="0" max="2000" step="10" bind:value={attack}
        on:pointerdown|stopPropagation on:mousedown|stopPropagation />
      <span class="pev-value">{attack} ms</span>
    </label>
    <label class="pev-slider-row">
      <span class="pev-slider-label">Hold</span>
      <input type="range" min="0" max="2000" step="10" bind:value={hold}
        on:pointerdown|stopPropagation on:mousedown|stopPropagation />
      <span class="pev-value">{hold} ms</span>
    </label>
    <label class="pev-slider-row">
      <span class="pev-slider-label">Decay</span>
      <input type="range" min="0" max="2000" step="10" bind:value={decay}
        on:pointerdown|stopPropagation on:mousedown|stopPropagation />
      <span class="pev-value">{decay} ms</span>
    </label>
  </div>

  <label class="pev-blocking-row" title="Pauses the utterance's speech at this point, runs this emotion, then continues speaking — placed mid-sentence, this effectively splits it into two. VuppetMaster has no way to report when the transition actually finishes, so completion is estimated from attack+hold+decay+50ms (a fixed buffer for dispatch/transport delay), not measured.">
    <input type="checkbox" bind:checked={blocking}
      on:pointerdown|stopPropagation on:mousedown|stopPropagation />
    <span class="pev-blocking-text">Blocking (pauses speech until this finishes)</span>
  </label>

  <div class="pev-test-row">
    {#if testError}<span class="pev-error">{testError}</span>{/if}
    <button
      type="button"
      class="pev-play-btn"
      disabled={disabled || testing || !currentCommand}
      title={disabled ? "Character isn't loaded yet" : testing ? "Testing…" : "Play on preview"}
      aria-label={disabled ? "Character isn't loaded yet" : testing ? "Testing…" : "Play on preview"}
      on:click={testNow}
    >
      <svg viewBox="0 0 24 24" width="12" height="12" fill="currentColor" aria-hidden="true"><path d="M8 6l10 6-10 6V6z" /></svg>
    </button>
  </div>
</div>

<style>
  .pev {
    display: flex;
    flex-direction: column;
    gap: 0.35rem;
  }

  .pev-row {
    display: flex;
    align-items: center;
    gap: 0.4rem;
  }

  .pev-type-label {
    font-size: 0.85rem;
    color: var(--muted);
    width: 3.8rem;
    flex-shrink: 0;
  }

  .pev-type-select {
    flex: 1;
  }

  .pev-curve {
    width: 100%;
    height: 60px;
    background: var(--panel-soft);
    border: 1px solid var(--stroke);
    border-radius: var(--radius-sm);
  }

  .pev-sliders {
    display: flex;
    flex-direction: column;
    gap: 0.3rem;
  }

  .pev-slider-row {
    display: flex;
    align-items: center;
    gap: 0.4rem;
  }

  .pev-slider-label {
    font-size: 0.85rem;
    color: var(--muted);
    width: 3.8rem;
    flex-shrink: 0;
  }

  /* The global `input { padding/border/box-sizing }` reset (app.css) applies to every <input>
     including type="range", padding the native track inside a bordered box — so the visible box
     no longer matches the browser's actual 0%-100% track, and the thumb ends up looking
     offset from the value it represents (reported 2026-07-20). Reset it back to a bare track and
     draw our own, same recipe as .edge-timeout-slider (app.css). */
  .pev-slider-row input[type="range"] {
    flex: 1;
    min-width: 0;
    appearance: none;
    -webkit-appearance: none;
    height: 22px;
    padding: 0;
    border: none;
    background: transparent;
    cursor: pointer;
  }

  .pev-slider-row input[type="range"]::-webkit-slider-runnable-track {
    height: 6px;
    border-radius: 999px;
    background: color-mix(in srgb, var(--accent) 30%, #ffffff 70%);
    border: 1px solid color-mix(in srgb, var(--accent) 28%, var(--stroke) 72%);
  }

  .pev-slider-row input[type="range"]::-webkit-slider-thumb {
    -webkit-appearance: none;
    appearance: none;
    width: 16px;
    height: 16px;
    margin-top: -6px;
    border-radius: 50%;
    border: 2px solid color-mix(in srgb, var(--accent) 72%, var(--ink) 28%);
    background: #ffffff;
    box-shadow: 0 1px 2px rgba(17, 24, 39, 0.25);
  }

  .pev-slider-row input[type="range"]::-moz-range-track {
    height: 6px;
    border-radius: 999px;
    background: color-mix(in srgb, var(--accent) 30%, #ffffff 70%);
    border: 1px solid color-mix(in srgb, var(--accent) 28%, var(--stroke) 72%);
  }

  .pev-slider-row input[type="range"]::-moz-range-thumb {
    width: 16px;
    height: 16px;
    border-radius: 50%;
    border: 2px solid color-mix(in srgb, var(--accent) 72%, var(--ink) 28%);
    background: #ffffff;
    box-shadow: 0 1px 2px rgba(17, 24, 39, 0.25);
  }

  .pev-value {
    font-size: 0.85rem;
    color: var(--ink);
    width: 3.8rem;
    flex-shrink: 0;
    text-align: right;
    font-variant-numeric: tabular-nums;
  }

  .pev-blocking-row {
    display: flex;
    align-items: flex-start;
    gap: 0.35rem;
    font-size: 0.85rem;
    color: var(--muted);
    cursor: pointer;
    margin-top: 0.15rem;
  }

  .pev-blocking-row input[type="checkbox"] {
    /* Overrides the global `input { width: 100% }` reset (app.css), which otherwise stretches
       the checkbox itself across the whole row — pushing the label text past the modal's right
       edge instead of sitting beside the checkbox (reported 2026-07-15). */
    flex: 0 0 auto;
    width: auto;
    margin-top: 0.15rem;
  }

  /* Without this, the label text has no width constraint of its own and can overflow past the
     modal's right edge instead of wrapping onto a second line (reported 2026-07-15). */
  .pev-blocking-text {
    flex: 1;
    min-width: 0;
  }

  .pev-test-row {
    display: flex;
    align-items: center;
    justify-content: flex-end;
    gap: 0.5rem;
    margin-top: 0.15rem;
  }

  .pev-error {
    font-size: 0.8rem;
    color: var(--danger);
    flex: 1;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .pev-play-btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 26px;
    height: 26px;
    padding: 0;
    border-radius: var(--radius-sm);
    border: 1px solid var(--stroke);
    background: var(--panel-soft);
    cursor: pointer;
    color: var(--ink);
  }

  .pev-play-btn:hover:not(:disabled) {
    background: var(--accent-soft);
  }

  .pev-play-btn:disabled {
    color: var(--muted);
    cursor: default;
  }
</style>
