<script>
  // Smallest of the per-type editors — clearEmotion takes no parameters at all, so there's
  // nothing to configure; just a description and a Test/Play button, matching the same
  // onTest/disabled/onChange contract as ParameterEnvelopeEditor/BackgroundColorEditor.
  export let onTest = null;
  export let disabled = false;
  export let onChange = null;

  const currentCommand = "clearEmotion";
  $: if (currentCommand) onChange?.(currentCommand);

  let testing = false;
  let testError = "";

  async function testNow() {
    if (!onTest || testing || disabled) return;
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

<div class="cee">
  <p class="cee-message">Clears the character's current emotion, resetting to neutral.</p>
  <div class="cee-test-row">
    {#if testError}<span class="cee-error">{testError}</span>{/if}
    <button
      type="button"
      class="cee-play-btn"
      disabled={disabled || testing}
      title={disabled ? "Character isn't loaded yet" : testing ? "Testing…" : "Play on preview"}
      aria-label={disabled ? "Character isn't loaded yet" : testing ? "Testing…" : "Play on preview"}
      on:click={testNow}
    >
      <svg viewBox="0 0 24 24" width="12" height="12" fill="currentColor" aria-hidden="true"><path d="M8 6l10 6-10 6V6z" /></svg>
    </button>
  </div>
</div>

<style>
  .cee {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }

  .cee-message {
    margin: 0;
    font-size: 0.78rem;
    color: #5a5a5a;
  }

  .cee-test-row {
    display: flex;
    align-items: center;
    justify-content: flex-end;
    gap: 0.5rem;
  }

  .cee-error {
    font-size: 0.72rem;
    color: #c0392b;
    flex: 1;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .cee-play-btn {
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

  .cee-play-btn:hover:not(:disabled) {
    background: #efe9e0;
  }

  .cee-play-btn:disabled {
    color: #9ca3af;
    cursor: default;
  }
</style>
