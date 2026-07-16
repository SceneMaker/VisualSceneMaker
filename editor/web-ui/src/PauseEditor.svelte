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
    font-size: 0.72rem;
    color: #7a7a7a;
    width: 3.5rem;
    flex-shrink: 0;
  }

  .pse-row input[type="range"] {
    flex: 1;
    min-width: 0;
  }

  .pse-value {
    font-size: 0.72rem;
    color: #3d3d3d;
    width: 3.8rem;
    flex-shrink: 0;
    text-align: right;
    font-variant-numeric: tabular-nums;
  }

  .pse-hint {
    margin: 0;
    font-size: 0.72rem;
    color: #5a5a5a;
  }
</style>
