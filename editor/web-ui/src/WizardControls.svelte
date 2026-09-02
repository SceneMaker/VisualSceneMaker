<script>
  import { onMount } from "svelte";

  export let projectId = null;
  export let plugin = null;
  export let apiGet;
  export let apiPut;
  export let onClose = () => {};

  let loading = true;
  let loadError = "";
  let variables = []; // [{name, type}] from the project's declared SceneFlow variables
  let rows = [];       // [{var, label, kind, optionsText, min, max}]
  let addVarChoice = "";
  let saveBusy = false;
  let saveError = "";
  let saveOk = false;
  let dirty = false;

  $: instanceName = plugin?.instanceName ?? "";
  $: usedVars = new Set(rows.map((r) => r.var));
  $: availableVariables = variables.filter((v) => !usedVars.has(v.name));

  function kindForType(type) {
    const t = (type || "").toLowerCase();
    if (t === "bool" || t === "boolean") return "boolean";
    if (t === "int" || t === "float" || t === "number") return "number";
    return "string";
  }

  function varTypeOf(name) {
    const v = variables.find((v) => v.name === name);
    return v ? v.type : "";
  }

  async function loadData() {
    loading = true;
    loadError = "";
    try {
      const [varsRes, controlsRes] = await Promise.all([
        apiGet(`/api/v1/projects/${projectId}/variables`),
        apiGet(`/api/v1/projects/${projectId}/plugins/${encodeURIComponent(instanceName)}/wizard-controls`),
      ]);
      variables = varsRes?.variables ?? [];
      const loaded = controlsRes?.controls ?? [];
      rows = loaded.map((c) => ({
        var: c.var,
        label: c.label ?? c.var,
        kind: c.kind === "choice" ? "string" : (c.kind ?? kindForType(varTypeOf(c.var))),
        optionsText: (c.options ?? []).join(", "),
        min: c.min ?? "",
        max: c.max ?? "",
      }));
    } catch (e) {
      loadError = e.message || "Failed to load wizard controls.";
    } finally {
      loading = false;
      dirty = false;
    }
  }

  function addControl() {
    if (!addVarChoice) return;
    const v = variables.find((v) => v.name === addVarChoice);
    rows = [
      ...rows,
      {
        var: addVarChoice,
        label: addVarChoice,
        kind: kindForType(v?.type),
        optionsText: "",
        min: "",
        max: "",
      },
    ];
    addVarChoice = "";
    dirty = true;
  }

  function removeControl(varName) {
    rows = rows.filter((r) => r.var !== varName);
    dirty = true;
  }

  function markDirty() {
    dirty = true;
    saveOk = false;
  }

  function toSaveObject(row) {
    const obj = { var: row.var, label: row.label || row.var };
    if (row.kind === "number") {
      obj.kind = "number";
      if (row.min !== "" && row.min !== null && row.min !== undefined) obj.min = Number(row.min);
      if (row.max !== "" && row.max !== null && row.max !== undefined) obj.max = Number(row.max);
    } else if (row.kind === "boolean") {
      obj.kind = "boolean";
    } else {
      const opts = (row.optionsText || "").split(",").map((s) => s.trim()).filter(Boolean);
      if (opts.length > 0) {
        obj.kind = "choice";
        obj.options = opts;
      } else {
        obj.kind = "string";
      }
    }
    return obj;
  }

  async function save() {
    if (saveBusy) return;
    saveError = "";
    saveOk = false;
    saveBusy = true;
    try {
      const controls = rows.map(toSaveObject);
      await apiPut(
        `/api/v1/projects/${projectId}/plugins/${encodeURIComponent(instanceName)}/wizard-controls`,
        { controls }
      );
      dirty = false;
      saveOk = true;
    } catch (e) {
      let msg = e.message || "Save failed.";
      try {
        const p = JSON.parse(msg);
        if (p?.message) msg = p.message;
      } catch {}
      saveError = msg;
    } finally {
      saveBusy = false;
    }
  }

  function handleKeydown(e) {
    if (e.key === "Escape") onClose();
  }

  onMount(loadData);
</script>

<!-- svelte-ignore a11y-click-events-have-key-events -->
<!-- svelte-ignore a11y-no-static-element-interactions -->
<div class="modal-backdrop" role="presentation" on:keydown={handleKeydown}
     on:click={(e) => { if (e.target === e.currentTarget) onClose(); }}>
  <div class="modal wc-modal" role="dialog" aria-modal="true" aria-labelledby="wc-title">
    <div class="wc-header">
      <div>
        <h4 id="wc-title" class="wc-title">Wizard controls</h4>
        <div class="wc-subtitle">{instanceName} — which SceneFlow variables the wizard can see and set</div>
      </div>
      <div class="wc-header-status">
        {#if saveError}
          <span class="wc-badge wc-badge-error" title={saveError}>Save failed</span>
        {:else if saveOk}
          <span class="wc-badge wc-badge-ok">Saved</span>
        {:else if dirty}
          <span class="wc-badge wc-badge-warn">Unsaved changes</span>
        {/if}
      </div>
    </div>

    {#if loading}
      <div class="wc-empty">Loading…</div>
    {:else if loadError}
      <div class="wc-empty wc-error-text">{loadError}</div>
    {:else}
      <div class="wc-rows">
        {#if rows.length === 0}
          <div class="wc-empty">No wizard-controllable variables yet. Add one below.</div>
        {/if}
        {#each rows as row (row.var)}
          <div class="wc-row">
            <div class="wc-row-main">
              <div class="wc-row-var">
                <code>{row.var}</code>
                <span class="wc-type-badge">{row.kind}</span>
              </div>
              <input
                class="wc-input"
                type="text"
                placeholder="Label shown to the wizard"
                bind:value={row.label}
                on:input={markDirty}
              />
              <button type="button" class="wc-remove" title="Remove" on:click={() => removeControl(row.var)}>×</button>
            </div>
            {#if row.kind === "number"}
              <div class="wc-row-extra">
                <label>Min <input class="wc-input wc-input-sm" type="number" bind:value={row.min} on:input={markDirty} /></label>
                <label>Max <input class="wc-input wc-input-sm" type="number" bind:value={row.max} on:input={markDirty} /></label>
              </div>
            {:else if row.kind === "string"}
              <div class="wc-row-extra">
                <label class="wc-options-label">
                  Options (comma-separated — leave empty for free text, fill in to show as a dropdown)
                  <input
                    class="wc-input"
                    type="text"
                    placeholder="e.g. health, travel, hobbies"
                    bind:value={row.optionsText}
                    on:input={markDirty}
                  />
                </label>
              </div>
            {/if}
          </div>
        {/each}
      </div>

      <div class="wc-add-row">
        <select class="wc-select" bind:value={addVarChoice}>
          <option value="">— pick a SceneFlow variable to add —</option>
          {#each availableVariables as v}
            <option value={v.name}>{v.name} ({v.type})</option>
          {/each}
        </select>
        <button type="button" class="ghost" disabled={!addVarChoice} on:click={addControl}>Add</button>
      </div>
      {#if variables.length === 0}
        <div class="wc-hint">This project has no declared SceneFlow variables yet.</div>
      {/if}
    {/if}

    <div class="wc-actions">
      <button type="button" class="primary" disabled={saveBusy || !dirty} on:click={save}>
        {saveBusy ? "Saving…" : "Save"}
      </button>
      <button type="button" class="ghost" on:click={onClose}>Close</button>
    </div>
  </div>
</div>

<style>
  .wc-modal {
    width: min(640px, 94vw);
    max-height: 82vh;
    overflow-y: auto;
    padding: 1.1rem 1.3rem;
  }
  .wc-header {
    display: flex;
    align-items: flex-start;
    justify-content: space-between;
    gap: 0.75rem;
    border-bottom: 1px solid var(--stroke);
    padding-bottom: 0.6rem;
    margin-bottom: 0.6rem;
  }
  .wc-title { margin: 0; font-size: 1rem; }
  .wc-subtitle { color: var(--muted); font-size: 0.8rem; margin-top: 2px; }

  .wc-badge {
    font-size: 0.72rem;
    font-weight: 600;
    padding: 3px 9px;
    border-radius: 999px;
    white-space: nowrap;
  }
  .wc-badge-ok { background: var(--accent-soft); color: var(--button-pressed); }
  .wc-badge-warn { background: #fff4d6; color: #8a6100; }
  .wc-badge-error { background: #fbe9e6; color: var(--danger); }

  .wc-empty { color: var(--muted); font-size: 0.85rem; padding: 0.75rem 0; }
  .wc-error-text { color: var(--danger); }

  .wc-rows { display: flex; flex-direction: column; gap: 10px; margin-bottom: 12px; }
  .wc-row {
    border: 1px solid var(--stroke);
    border-radius: var(--radius-md, 10px);
    padding: 8px 10px;
    background: var(--panel-soft);
  }
  .wc-row-main { display: flex; align-items: center; gap: 10px; }
  .wc-row-var {
    flex: none;
    display: flex;
    align-items: center;
    gap: 6px;
    min-width: 160px;
  }
  .wc-row-var code {
    background: #fff;
    border: 1px solid var(--stroke);
    border-radius: 6px;
    padding: 2px 6px;
    font-size: 0.8rem;
  }
  .wc-type-badge {
    font-size: 0.68rem;
    color: var(--muted);
    border: 1px solid var(--stroke);
    border-radius: 999px;
    padding: 1px 7px;
    background: #fff;
  }
  .wc-input { flex: 1; padding: 6px 9px; border-radius: 8px; border: 1px solid var(--stroke); font: inherit; }
  .wc-input-sm { width: 90px; flex: none; }
  .wc-remove {
    flex: none;
    background: transparent;
    border: none;
    color: var(--muted);
    font-size: 1.1rem;
    line-height: 1;
    cursor: pointer;
    padding: 2px 6px;
  }
  .wc-remove:hover { color: var(--danger); }

  .wc-row-extra { display: flex; gap: 16px; margin-top: 8px; padding-left: 2px; }
  .wc-row-extra label { display: flex; align-items: center; gap: 6px; font-size: 0.78rem; color: var(--muted); }
  .wc-options-label { flex: 1; display: flex; flex-direction: column; align-items: flex-start; gap: 4px; }
  .wc-options-label .wc-input { width: 100%; }

  .wc-add-row { display: flex; gap: 8px; align-items: center; margin-top: 4px; }
  .wc-select { flex: 1; padding: 7px 10px; border-radius: 8px; border: 1px solid var(--stroke); font: inherit; }
  .wc-hint { color: var(--muted); font-size: 0.78rem; margin-top: 6px; }

  .wc-actions {
    display: flex;
    justify-content: flex-end;
    gap: 8px;
    margin-top: 16px;
    border-top: 1px solid var(--stroke);
    padding-top: 12px;
  }

  .wc-actions button,
  .wc-add-row button {
    padding: 7px 14px;
    border-radius: 8px;
    border: 1px solid var(--stroke);
    font: inherit;
    font-weight: 600;
    cursor: pointer;
  }
</style>
