<script>
  // Renders one type-aware control for a single command param — the atomic unit ActionForm
  // assembles many of into a whole-command form, and that App.svelte's PlayAction command helper
  // reuses per free-form argument row (which, unlike ActionForm, lets the author add/remove rows
  // with arbitrary keys not in the schema — so it needs a single-value widget, not a whole form).
  export let param;        // {name?, type, enum?, description?} — name is optional here (App.svelte's
                            // cmd-helper rows resolve it from a free-typed key, may be unknown)
  export let value = "";
  export let onChange;     // (value: string) => void
  export let disabled = false;
  export let inputId = undefined; // optional id, e.g. for an associated <label for=...>
  export let placeholder = undefined;

  $: type = param?.type || "string";
  $: enumValues = Array.isArray(param?.enum) ? param.enum : [];
</script>

{#if type === "boolean"}
  <input
    id={inputId}
    class="apf-checkbox"
    type="checkbox"
    checked={value === "true" || value === true}
    disabled={disabled}
    on:change={(e) => onChange?.(e.target.checked ? "true" : "false")}
    on:pointerdown|stopPropagation on:mousedown|stopPropagation
  />
{:else if type === "int" || type === "number"}
  <input
    id={inputId}
    class="apf-flex"
    type="number"
    {value}
    {disabled}
    {placeholder}
    on:input={(e) => onChange?.(e.target.value)}
    on:pointerdown|stopPropagation on:mousedown|stopPropagation
  />
{:else if type === "color"}
  <div class="apf-color">
    <input
      class="apf-color-text"
      type="text"
      {value}
      {disabled}
      placeholder={placeholder || "e.g. midnightblue or #191970"}
      on:input={(e) => onChange?.(e.target.value)}
      on:pointerdown|stopPropagation on:mousedown|stopPropagation
    />
    <input
      type="color"
      class="apf-color-swatch"
      value={/^#[0-9a-fA-F]{6}$/.test(value) ? value : "#000000"}
      {disabled}
      on:input={(e) => onChange?.(e.target.value)}
      on:pointerdown|stopPropagation on:mousedown|stopPropagation
    />
  </div>
{:else if enumValues.length}
  <select
    id={inputId}
    class="apf-flex"
    {value}
    {disabled}
    on:change={(e) => onChange?.(e.target.value)}
    on:pointerdown|stopPropagation on:mousedown|stopPropagation
  >
    {#each enumValues as option}
      <option value={option}>{option}</option>
    {/each}
  </select>
{:else}
  <input
    id={inputId}
    class="apf-flex"
    type="text"
    {value}
    {disabled}
    {placeholder}
    on:input={(e) => onChange?.(e.target.value)}
    on:pointerdown|stopPropagation on:mousedown|stopPropagation
  />
{/if}

<style>
  /* Components (unlike plain elements) have no selectable "host" tag from the outside — a
     parent's scoped `.some-row input[type="text"]` rule can't reach into this component's own
     template (reported building ActionForm: Svelte flagged those very selectors as unused once
     the inputs moved here), so sizing for callers that lay this out in a flex row (ActionForm's
     .af-row) has to live in this component's own styles instead. */
  .apf-flex {
    flex: 1;
    min-width: 0;
  }

  .apf-checkbox {
    flex: 0 0 auto;
    width: auto;
  }

  .apf-color {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    flex: 1;
  }

  .apf-color-text {
    flex: 1;
  }

  .apf-color-swatch {
    width: 30px;
    height: 26px;
    padding: 2px;
    flex-shrink: 0;
    border: 1px solid var(--stroke);
    border-radius: 6px;
    background: #fff;
    cursor: pointer;
  }
</style>
