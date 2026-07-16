<script>
  // Small counterpart to ParameterEnvelopeEditor for the "background" action, whose parameter
  // shape (just a color) doesn't fit the emotion envelope (type/intensity/attack/hold/decay) —
  // a dedicated per-type editor rather than forcing background through the emotion shape or
  // building a generic schema-driven action editor for only 2 real action types (M13 scope note).
  export let onTest = null;     // async (command: string) => void — throws/rejects on failure
  export let disabled = false;  // e.g. the target character isn't loaded yet
  export let onChange = null;   // (command: string) => void — fires on every change
  export let initialValues = null; // {color} — M13d edit mode; read once at creation

  let color = initialValues?.color ?? "";

  let testing = false;
  let testError = "";

  $: currentCommand = color.trim() ? `background color='${color.trim()}'` : "";
  $: if (currentCommand) onChange?.(currentCommand);

  // <input type="color"> requires a 6-digit hex value or it silently ignores the assignment —
  // fall back to a neutral swatch for named CSS colors (e.g. "midnightblue") rather than error.
  $: swatchValue = /^#[0-9a-fA-F]{6}$/.test(color) ? color : "#000000";

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

<div class="bce">
  <div class="bce-row">
    <label class="bce-color-label" for="bce-color-text">Color</label>
    <input
      id="bce-color-text"
      class="bce-color-input"
      list="bce-color-list"
      bind:value={color}
      placeholder="e.g. midnightblue or #191970"
      on:pointerdown|stopPropagation
      on:mousedown|stopPropagation
    />
    <input
      type="color"
      class="bce-color-swatch"
      value={swatchValue}
      on:input={(e) => (color = e.target.value)}
      on:pointerdown|stopPropagation
      on:mousedown|stopPropagation
    />
    <datalist id="bce-color-list">
      <option value="midnightblue"></option>
      <option value="black"></option>
      <option value="white"></option>
      <option value="crimson"></option>
      <option value="forestgreen"></option>
      <option value="goldenrod"></option>
    </datalist>
  </div>

  <div class="bce-test-row">
    {#if testError}<span class="bce-error">{testError}</span>{/if}
    <button
      type="button"
      class="bce-play-btn"
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
  .bce {
    display: flex;
    flex-direction: column;
    gap: 0.35rem;
  }

  .bce-row {
    display: flex;
    align-items: center;
    gap: 0.4rem;
  }

  .bce-color-label {
    font-size: 0.72rem;
    color: #7a7a7a;
    width: 3.5rem;
    flex-shrink: 0;
  }

  .bce-color-input {
    flex: 1;
    font-size: 0.78rem;
    font-family: inherit;
    padding: 0.2rem 0.4rem;
    border: 1px solid #d8d2c8;
    border-radius: 6px;
    background: #fff;
    color: #3d3d3d;
  }

  .bce-color-swatch {
    width: 30px;
    height: 26px;
    padding: 2px;
    flex-shrink: 0;
    border: 1px solid #d8d2c8;
    border-radius: 6px;
    background: #fff;
    cursor: pointer;
  }

  .bce-test-row {
    display: flex;
    align-items: center;
    justify-content: flex-end;
    gap: 0.5rem;
    margin-top: 0.15rem;
  }

  .bce-error {
    font-size: 0.72rem;
    color: #c0392b;
    flex: 1;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .bce-play-btn {
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

  .bce-play-btn:hover:not(:disabled) {
    background: #efe9e0;
  }

  .bce-play-btn:disabled {
    color: #9ca3af;
    cursor: default;
  }
</style>
