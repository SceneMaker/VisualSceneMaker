<script>
  // Schema-driven action editor: one control per declared param.type, generated from a
  // PluginCommand-shaped schema ({name, params: [{name, type, required, default, description,
  // enum}]}) instead of a bespoke Svelte component per action name. Emits the same "command
  // text" contract (onChange(commandText), onTest(commandText)) that ParameterEnvelopeEditor
  // already established, so it's a drop-in replacement at existing call sites (InsertActionDialog,
  // SiaPanel).
  import ParameterEnvelopeEditor from "./ParameterEnvelopeEditor.svelte";
  import ActionParamField from "./ActionParamField.svelte";
  import { EMOTION_TYPES } from "./emotionTypes.js";
  import { buildCommandText } from "./actionCommandCodec.js";

  export let schema; // {name, params: [{name, type, required, default, description, enum}], widget?}
  export let initialValues = null; // {[paramName]: value} — read once at creation, e.g. from featuresToMap()
  export let onTest = null;    // async (command: string) => void — throws/rejects on failure
  export let disabled = false; // e.g. the target character isn't loaded yet
  export let onChange = null;  // (command: string) => void — fires on every parameter change

  $: params = Array.isArray(schema?.params) ? schema.params : [];

  // Escape hatch: the emotion envelope (type/intensity/attack/hold/decay/blocking) is a genuine
  // composite widget (curve visualization coupling five params), not a per-param rendering gap —
  // keep using the existing hand-built editor for it rather than forcing it through generic
  // one-control-per-param rendering. Selected by name today; schema.widget lets a future command
  // opt in without being literally named "emotion".
  $: useEnvelopeWidget = schema?.widget === "envelope" || schema?.name === "emotion";

  // Reactive to schema changes (e.g. a caller lets the author switch commands in the same
  // dialog instance, like InsertActionDialog's command picker) — reset values to the new
  // schema's defaults only when the command itself actually changes, not on every params
  // re-render, so mid-edit input isn't clobbered by unrelated reactivity elsewhere.
  let values = {};
  let lastSchemaName;
  $: if (schema?.name !== lastSchemaName) {
    lastSchemaName = schema?.name;
    const next = {};
    for (const param of params) {
      next[param.name] = initialValues?.[param.name] ?? param.default ?? (param.type === "boolean" ? "false" : "");
    }
    values = next;
  }

  $: currentCommand = schema?.name ? buildCommandText(schema.name, values, schema) : "";
  // Always report the current value, including "" (e.g. a required param still empty) — a
  // caller (InsertActionDialog) needs the "" to correctly re-disable Insert/Play once a
  // previously-valid command becomes invalid again (switching commands, clearing a field). Only
  // guarding on truthiness here would silently keep the caller on its last-known-valid value
  // forever, since it'd never be told the command turned invalid (reported 2026-07-22, once a
  // required color param could legitimately make this "").
  $: onChange?.(currentCommand);

  function setValue(paramName, value) {
    values = { ...values, [paramName]: value };
  }

  let testing = false;
  let testError = "";

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

{#if useEnvelopeWidget}
  <ParameterEnvelopeEditor
    actionName={schema.name}
    typeOptions={EMOTION_TYPES}
    {initialValues}
    {disabled}
    {onTest}
    {onChange}
  />
{:else}
  <div class="af">
    {#each params as param (param.name)}
      <div class="af-row">
        <label class="af-label" for="af-{schema?.name}-{param.name}" title={param.description || ""}>
          {param.name}{param.required ? " *" : ""}
        </label>
        <ActionParamField
          {param}
          value={values[param.name]}
          onChange={(v) => setValue(param.name, v)}
          inputId="af-{schema?.name}-{param.name}"
        />
      </div>
      {#if param.description}
        <div class="af-hint">{param.description}</div>
      {/if}
    {/each}

    {#if onTest}
      <div class="af-test-row">
        {#if testError}<span class="af-error">{testError}</span>{/if}
        <button
          type="button"
          class="af-play-btn"
          disabled={disabled || testing || !currentCommand}
          title={disabled ? "Character isn't loaded yet" : testing ? "Testing…" : "Play on preview"}
          aria-label={disabled ? "Character isn't loaded yet" : testing ? "Testing…" : "Play on preview"}
          on:click={testNow}
        >
          <svg viewBox="0 0 24 24" width="12" height="12" fill="currentColor" aria-hidden="true"><path d="M8 6l10 6-10 6V6z" /></svg>
        </button>
      </div>
    {/if}
  </div>
{/if}

<style>
  .af {
    display: flex;
    flex-direction: column;
    gap: 0.35rem;
  }

  .af-row {
    display: flex;
    align-items: center;
    gap: 0.4rem;
  }

  .af-label {
    font-size: 0.85rem;
    color: var(--muted);
    flex: 0 0 auto;
    min-width: 3.8rem;
    text-transform: none;
  }

  .af-hint {
    font-size: 0.72rem;
    color: var(--muted);
    margin-top: -0.2rem;
  }

  .af-test-row {
    display: flex;
    align-items: center;
    justify-content: flex-end;
    gap: 0.5rem;
    margin-top: 0.15rem;
  }

  .af-error {
    font-size: 0.8rem;
    color: var(--danger);
    flex: 1;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .af-play-btn {
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

  .af-play-btn:hover:not(:disabled) {
    background: var(--accent-soft);
  }

  .af-play-btn:disabled {
    color: var(--muted);
    cursor: default;
  }
</style>
