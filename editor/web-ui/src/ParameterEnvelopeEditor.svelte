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

  let type = typeOptions[0] || "";
  let intensity = 1;
  let attack = 200;
  let hold = 20;
  let decay = 300;

  let testing = false;
  let testError = "";

  // Referencing type/intensity/attack/hold/decay directly (not hidden inside a called function)
  // so Svelte's static dependency analysis actually reruns this on every slider/type change —
  // a helper function call alone wouldn't have tracked its internal reads.
  $: currentCommand = type.trim()
    ? `${actionName} type='${type.trim()}' intensity='${intensity}' attack='${attack}' hold='${hold}' decay='${decay}'`
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
    <input
      id="pev-type-{actionName}"
      class="pev-type-input"
      list="pev-type-list-{actionName}"
      bind:value={type}
      on:pointerdown|stopPropagation
      on:mousedown|stopPropagation
    />
    <datalist id="pev-type-list-{actionName}">
      {#each typeOptions as t}
        <option value={t}></option>
      {/each}
    </datalist>
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
    font-size: 0.72rem;
    color: #7a7a7a;
    width: 3.5rem;
    flex-shrink: 0;
  }

  .pev-type-input {
    flex: 1;
    font-size: 0.78rem;
    font-family: inherit;
    padding: 0.2rem 0.4rem;
    border: 1px solid #d8d2c8;
    border-radius: 6px;
    background: #fff;
    color: #3d3d3d;
  }

  .pev-curve {
    width: 100%;
    height: 60px;
    background: #fbfaf8;
    border: 1px solid #e2ddd4;
    border-radius: 6px;
  }

  .pev-sliders {
    display: flex;
    flex-direction: column;
    gap: 0.15rem;
  }

  .pev-slider-row {
    display: flex;
    align-items: center;
    gap: 0.4rem;
  }

  .pev-slider-label {
    font-size: 0.72rem;
    color: #7a7a7a;
    width: 3.5rem;
    flex-shrink: 0;
  }

  .pev-slider-row input[type="range"] {
    flex: 1;
    min-width: 0;
  }

  .pev-value {
    font-size: 0.72rem;
    color: #3d3d3d;
    width: 3.8rem;
    flex-shrink: 0;
    text-align: right;
    font-variant-numeric: tabular-nums;
  }

  .pev-test-row {
    display: flex;
    align-items: center;
    justify-content: flex-end;
    gap: 0.5rem;
    margin-top: 0.15rem;
  }

  .pev-error {
    font-size: 0.72rem;
    color: #c0392b;
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
    border-radius: 6px;
    border: 1px solid #c0b8ae;
    background: #f8f6f2;
    cursor: pointer;
    color: #3d3d3d;
  }

  .pev-play-btn:hover:not(:disabled) {
    background: #efe9e0;
  }

  .pev-play-btn:disabled {
    color: #9ca3af;
    cursor: default;
  }
</style>
