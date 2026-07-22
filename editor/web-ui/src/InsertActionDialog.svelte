<script>
  // Generalized insert/edit dialog for inline scene-script actions — replaces ActionCommandModal's
  // four hardcoded per-action-type editors with a schema-driven agent+command picker feeding
  // ActionForm, so ANY agent's ANY declared command is insertable/editable, not just
  // emotion/background/clearEmotion/pause on SIA agents. Triggered by Ctrl+I (insert) or
  // double-click on an existing span (edit) — see App.svelte's handleInsertShortcut/
  // handleScriptCommandMenu.
  import ActionForm from "./ActionForm.svelte";

  export let mode = "insert";          // "insert" | "edit"
  export let agents = [];              // [{agentName}], ordered — first entry is the default target
                                        // (turn's own speaker for insert, the span's actor for edit)
  export let initialTarget = "";       // agentName to preselect
  export let initialCommandName = null; // command name to preselect (edit mode) — null for insert
  export let initialValues = null;     // {[paramName]: value} — only meaningful while the selected
                                        // command still equals initialCommandName
  export let useRawFallback = false;   // true when editing a span whose action name matched no
                                        // known command (nor a convenience emotion alias) — the
                                        // dialog degrades to a single raw-text field instead of ActionForm
  export let initialRawBody = "";      // full command body text, used only when useRawFallback
  export let previewCapableAgents = []; // [{agentName, instanceName, loaded}] — for the optional
                                         // live "Play" test button and "not loaded" banner
  export let pluginCommandsForAgent;    // (agentName) => PluginCommand[] — App.svelte's existing
                                         // agent-agnostic command resolver, passed down rather than
                                         // duplicated here
  export let x = null; // null = centered (CSS flex); number = dragged to an absolute viewport position
  export let y = null;
  export let projectId = null;
  export let apiPost;
  export let onSave = null;   // (commandBody: string, targetActor: string, commandName: string) => void
  export let onClose = null;  // () => void
  export let onDragStart = null; // (event, rect) => void

  const PAUSE_PSEUDO_COMMAND = {
    name: "pause",
    type: "action",
    summary: "Pause",
    description: "Pauses the utterance's speech for a fixed duration — a core timing primitive, not tied to any agent.",
    params: [{ name: "duration", type: "int", required: true, default: "500", description: "Milliseconds to pause" }]
  };

  let modalEl;
  let selectedAgent = initialTarget || agents[0]?.agentName || "";
  let selectedCommandName = initialCommandName;
  let currentCommand = "";
  let rawCommandText = initialRawBody;

  $: agentCommands = [PAUSE_PSEUDO_COMMAND, ...(pluginCommandsForAgent?.(selectedAgent) || [])];
  $: if (!useRawFallback && !agentCommands.some((c) => c.name === selectedCommandName)) {
    selectedCommandName = agentCommands[0]?.name || null;
  }
  $: selectedSchema = agentCommands.find((c) => c.name === selectedCommandName) || null;
  $: activeInitialValues = selectedCommandName === initialCommandName ? initialValues : null;
  $: selectedPreviewAgent = previewCapableAgents.find((a) => a.agentName === selectedAgent) || null;
  $: testFn = selectedPreviewAgent?.loaded
    ? (cmd) => apiPost(`/api/v1/projects/${projectId}/plugins/${selectedPreviewAgent.instanceName}/preview/action`, { command: cmd })
    : null;

  function handleHeaderPointerDown(e) {
    onDragStart?.(e, modalEl?.getBoundingClientRect());
  }

  // A plugin's declared `summary` is documentation prose ("Set the page backdrop shown behind
  // the transparent avatar"), not a scannable label — a user looking for "the background action"
  // in a dropdown full of full sentences won't recognize it as such (reported 2026-07-23). The
  // short command NAME is what the user actually thinks in terms of; summary moves to a hover
  // tooltip (option title=) instead. A couple of known multi-word names get a nicer split;
  // anything else just gets capitalized.
  const COMMAND_NAME_LABELS = { clearemotion: "Clear Emotion" };
  function commandLabel(cmd) {
    const known = COMMAND_NAME_LABELS[cmd.name];
    if (known) return known;
    return cmd.name.charAt(0).toUpperCase() + cmd.name.slice(1);
  }

  // Root cause of a severe hang (see SiaPanel.svelte's own note on the same pitfall):
  // ActionForm/ParameterEnvelopeEditor's `$: if (currentCommand) onChange?.(currentCommand);`
  // treats the onChange PROP REFERENCE as a dependency, not just currentCommand's value — an
  // inline `onChange={(cmd) => (currentCommand = cmd)}` recreates a new closure every render,
  // which re-triggers that reactive statement, which reassigns currentCommand again, forever,
  // synchronously. A stable function reference that no-ops once the value has actually settled
  // breaks the cycle after one pass.
  function setCurrentCommand(cmd) {
    if (currentCommand === cmd) return;
    currentCommand = cmd;
  }

  function saveNow() {
    const body = useRawFallback ? rawCommandText.trim() : currentCommand;
    if (!body) return;
    onSave?.(body, selectedAgent, useRawFallback ? initialCommandName : selectedCommandName);
  }

  function handleBackdropClick(e) {
    if (e.target === e.currentTarget) onClose?.();
  }

  $: title = `${mode === "edit" ? "Edit" : "Insert"} action — ${selectedAgent}`;
  $: saveLabel = mode === "edit" ? "Save" : "Insert";
  $: canSave = useRawFallback ? !!rawCommandText.trim() : !!currentCommand;
</script>

<div class="modal-backdrop eim-backdrop" role="presentation" on:click={handleBackdropClick}>
  <div
    class="modal eim-modal"
    bind:this={modalEl}
    role="dialog"
    aria-label={title}
    style:position={x !== null ? "fixed" : null}
    style:left={x !== null ? `${x}px` : null}
    style:top={y !== null ? `${y}px` : null}
    style:margin={x !== null ? "0" : null}
  >
    <div
      class="eim-header"
      on:pointerdown|stopPropagation={handleHeaderPointerDown}
      on:mousedown|stopPropagation={handleHeaderPointerDown}
    >
      <h3 class="eim-title">{title}</h3>
      <button
        type="button"
        class="ghost icon-button eim-close"
        on:pointerdown|stopPropagation
        on:mousedown|stopPropagation
        on:click|stopPropagation={() => onClose?.()}
        title="Cancel"
        aria-label="Cancel"
      >
        &times;
      </button>
    </div>
    <div class="eim-body">
      {#if agents.length > 1}
        <div class="eim-row">
          <label class="eim-target-label" for="eim-target-select">Target</label>
          <select id="eim-target-select" class="eim-target-select" bind:value={selectedAgent}
            on:pointerdown|stopPropagation on:mousedown|stopPropagation
          >
            {#each agents as opt}
              <option value={opt.agentName}>{opt.agentName}</option>
            {/each}
          </select>
        </div>
      {/if}

      {#if useRawFallback}
        <div class="eim-row">
          <label class="eim-target-label" for="eim-raw-input">Command</label>
          <input
            id="eim-raw-input"
            class="eim-target-select"
            type="text"
            bind:value={rawCommandText}
            placeholder="name key='value' ..."
            on:pointerdown|stopPropagation on:mousedown|stopPropagation
          />
        </div>
        <div class="eim-hint">
          No known command schema matched "{initialCommandName}" for {selectedAgent} — editing as raw text.
        </div>
      {:else}
        <div class="eim-row">
          <label class="eim-target-label" for="eim-command-select">Command</label>
          <select id="eim-command-select" class="eim-target-select" bind:value={selectedCommandName}
            on:pointerdown|stopPropagation on:mousedown|stopPropagation
          >
            {#each agentCommands as cmd}
              <option value={cmd.name} title={cmd.summary || ""}>{commandLabel(cmd)}</option>
            {/each}
          </select>
        </div>
        {#if selectedSchema}
          <ActionForm
            schema={selectedSchema}
            initialValues={activeInitialValues}
            disabled={!!selectedPreviewAgent && !selectedPreviewAgent.loaded}
            onTest={testFn}
            onChange={setCurrentCommand}
          />
        {/if}
      {/if}

      {#if selectedPreviewAgent && !selectedPreviewAgent.loaded}
        <div class="eim-not-loaded">
          {selectedAgent} preview isn't loaded yet — Play is disabled, but you can still {mode === "edit" ? "save" : "insert"}.
        </div>
      {/if}
    </div>
    <div class="eim-footer">
      <button type="button" class="eim-cancel" on:click={() => onClose?.()}>Cancel</button>
      <button type="button" class="eim-insert" disabled={!canSave} on:click={saveNow}>{saveLabel}</button>
    </div>
  </div>
</div>

<style>
  /* Border/radius/shadow/background come from the shared .modal/.modal-backdrop classes
     (app.css) — same design language as the "Command(s) executed at…" (.cmd-modal) and
     "Add/Edit variable definition" (.def-modal) windows. Only what's actually specific to this
     dialog (compact width, draggable header, grid→flex layout for the header/body/footer bands)
     is overridden here — mirrors ActionCommandModal's own layout, which this replaces. */
  .eim-modal {
    width: min(380px, 92vw);
    display: flex;
    flex-direction: column;
    padding: 0;
    gap: 0;
    overflow: hidden;
  }

  .eim-header {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    padding: 0.6rem 0.8rem;
    background: var(--panel-soft);
    border-bottom: 1px solid var(--stroke);
    cursor: move;
    touch-action: none;
    user-select: none;
  }

  .eim-title {
    flex: 1;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .eim-close {
    font-size: 1.1rem;
    line-height: 1;
    padding: 0;
    flex-shrink: 0;
  }

  .eim-body {
    padding: 0.8rem;
    display: grid;
    gap: 0.5rem;
  }

  .eim-row {
    display: flex;
    align-items: center;
    gap: 0.4rem;
  }

  .eim-target-label {
    font-size: 0.85rem;
    color: var(--muted);
    width: 3.8rem;
    flex-shrink: 0;
  }

  .eim-target-select {
    flex: 1;
  }

  .eim-hint {
    font-size: 0.78rem;
    color: var(--muted);
  }

  .eim-not-loaded {
    font-size: 0.8rem;
    color: #9a7d2a;
    background: #fbf3de;
    border: 1px solid #ecdcb0;
    border-radius: var(--radius-sm);
    padding: 0.4rem 0.6rem;
  }

  .eim-footer {
    display: flex;
    justify-content: flex-end;
    gap: 0.5rem;
    padding: 0.6rem 0.8rem;
    border-top: 1px solid var(--stroke);
    background: var(--panel-soft);
  }

  .eim-cancel,
  .eim-insert {
    font-size: 0.85rem;
    padding: 0.45rem 1rem;
    border-radius: var(--radius-sm);
    cursor: pointer;
  }

  .eim-cancel {
    border: 1px solid var(--stroke);
    background: var(--panel);
    color: var(--ink);
  }

  .eim-cancel:hover {
    background: var(--panel-soft);
  }

  .eim-insert {
    border: 1px solid var(--accent);
    background: var(--accent);
    color: #fff;
    font-weight: 600;
  }

  .eim-insert:hover:not(:disabled) {
    background: var(--button-pressed);
  }

  .eim-insert:disabled {
    border-color: var(--stroke);
    background: var(--panel-soft);
    color: var(--muted);
    cursor: default;
  }
</style>
