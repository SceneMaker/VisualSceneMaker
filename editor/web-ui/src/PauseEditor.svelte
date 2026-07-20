<script>
  // Pure timing primitive (see core's ActionBlockingUtil / ReactivePlayer.playScene()) — no actor,
  // no visual effect, so unlike ParameterEnvelopeEditor/BackgroundColorEditor/ClearEmotionEditor
  // there's nothing meaningful to preview-test here; just the one parameter.
  export let onChange = null;      // (command: string) => void
  export let initialValues = null; // {duration} — edit mode; read once at creation

  let duration = initialValues?.duration !== undefined ? Number(initialValues.duration) : 500;

  $: currentCommand = `pause duration='${duration}'`;
  $: if (currentCommand) onChange?.(currentCommand);
</script>

<div class="pse">
  <label class="pse-row">
    <span class="pse-label">Duration</span>
    <input type="range" min="0" max="3000" step="50" bind:value={duration}
      on:pointerdown|stopPropagation on:mousedown|stopPropagation />
    <span class="pse-value">{duration} ms</span>
  </label>
  <p class="pse-hint">Pauses the turn's speech here for the given duration, then continues — no visual effect on the character.</p>
</div>

<style>
  .pse {
    display: flex;
    flex-direction: column;
    gap: 0.4rem;
  }

  .pse-row {
    display: flex;
    align-items: center;
    gap: 0.4rem;
  }

  .pse-label {
    font-size: 0.85rem;
    color: var(--muted);
    width: 3.8rem;
    flex-shrink: 0;
  }

  /* Same fix as ParameterEnvelopeEditor's sliders — the global `input { padding/border }` reset
     (app.css) otherwise pads the native track inside a bordered box, so the thumb ends up
     looking offset from the value it represents (reported 2026-07-20). */
  .pse-row input[type="range"] {
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

  .pse-row input[type="range"]::-webkit-slider-runnable-track {
    height: 6px;
    border-radius: 999px;
    background: color-mix(in srgb, var(--accent) 30%, #ffffff 70%);
    border: 1px solid color-mix(in srgb, var(--accent) 28%, var(--stroke) 72%);
  }

  .pse-row input[type="range"]::-webkit-slider-thumb {
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

  .pse-row input[type="range"]::-moz-range-track {
    height: 6px;
    border-radius: 999px;
    background: color-mix(in srgb, var(--accent) 30%, #ffffff 70%);
    border: 1px solid color-mix(in srgb, var(--accent) 28%, var(--stroke) 72%);
  }

  .pse-row input[type="range"]::-moz-range-thumb {
    width: 16px;
    height: 16px;
    border-radius: 50%;
    border: 2px solid color-mix(in srgb, var(--accent) 72%, var(--ink) 28%);
    background: #ffffff;
    box-shadow: 0 1px 2px rgba(17, 24, 39, 0.25);
  }

  .pse-value {
    font-size: 0.85rem;
    color: var(--ink);
    width: 3.8rem;
    flex-shrink: 0;
    text-align: right;
    font-variant-numeric: tabular-nums;
  }

  .pse-hint {
    margin: 0;
    font-size: 0.8rem;
    color: var(--muted);
  }
</style>
