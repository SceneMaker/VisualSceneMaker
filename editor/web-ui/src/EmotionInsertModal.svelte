<script>
  import ParameterEnvelopeEditor from "./ParameterEnvelopeEditor.svelte";

  const EMOTION_TYPES = [
    "happy", "sad", "angry", "tear", "disgust", "surprise",
    "smile", "excited", "fear", "bored", "relaxed"
  ];

  export let speakerName = "";
  export let instanceName = "";
  export let loaded = false;
  export let x = null; // null = centered (CSS flex); number = dragged to an absolute viewport position
  export let y = null;
  export let projectId = null;
  export let apiPost;
  export let onInsert = null;    // (bracketText: string) => void — also responsible for closing the modal
  export let onClose = null;     // () => void — cancel without inserting
  export let onDragStart = null; // (event, rect) => void — rect is the modal's current bounding rect

  let currentCommand = "";
  let modalEl;

  function handleHeaderPointerDown(e) {
    onDragStart?.(e, modalEl?.getBoundingClientRect());
  }

  async function testAction(command) {
    await apiPost(`/api/v1/projects/${projectId}/plugins/${instanceName}/preview/action`, { command });
  }

  function insertNow() {
    if (!currentCommand) return;
    onInsert?.(`[${currentCommand}]`);
  }

  function handleBackdropClick(e) {
    if (e.target === e.currentTarget) onClose?.();
  }
</script>

<div class="eim-backdrop" role="presentation" on:click={handleBackdropClick}>
  <div
    class="eim-modal"
    bind:this={modalEl}
    role="dialog"
    aria-label="Insert emotion"
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
      <span class="eim-title">Insert emotion — {speakerName}</span>
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
      <ParameterEnvelopeEditor
        actionName="emotion"
        typeOptions={EMOTION_TYPES}
        disabled={!loaded}
        onTest={testAction}
        onChange={(cmd) => (currentCommand = cmd)}
      />
      {#if !loaded}
        <div class="eim-not-loaded">
          {speakerName} preview isn't loaded yet — Play is disabled, but you can still insert.
        </div>
      {/if}
    </div>
    <div class="eim-footer">
      <button type="button" class="eim-cancel" on:click={() => onClose?.()}>Cancel</button>
      <button type="button" class="eim-insert" disabled={!currentCommand} on:click={insertNow}>Insert</button>
    </div>
  </div>
</div>

<style>
  .eim-backdrop {
    position: fixed;
    inset: 0;
    z-index: 800;
    background: rgba(0, 0, 0, 0.35);
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .eim-modal {
    width: 360px;
    background: #fff;
    border: 1px solid #c0b8ae;
    border-radius: 10px;
    box-shadow: 0 12px 32px rgba(0, 0, 0, 0.28);
    display: flex;
    flex-direction: column;
    overflow: hidden;
  }

  .eim-header {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    padding: 0.6rem 0.7rem;
    background: #f8f6f2;
    border-bottom: 1px solid #e2ddd4;
    cursor: move;
    touch-action: none;
    user-select: none;
  }

  .eim-title {
    flex: 1;
    font-size: 0.85rem;
    font-weight: 600;
    color: #3d3d3d;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  /* Sizing/shape/hover come from the shared .ghost.icon-button classes (app.css) — same
     convention as the "Add/Edit variable definition" (.def-close) and "Command(s) executed
     at..." (.cmd-modal-close) windows. This just matches their small font-size tweak. */
  .eim-close {
    font-size: 1.1rem;
    line-height: 1;
    padding: 0;
    flex-shrink: 0;
  }

  .eim-body {
    padding: 0.7rem;
  }

  .eim-not-loaded {
    margin-top: 0.5rem;
    font-size: 0.72rem;
    color: #9a7d2a;
    background: #fbf3de;
    border: 1px solid #ecdcb0;
    border-radius: 6px;
    padding: 0.4rem 0.5rem;
  }

  .eim-footer {
    display: flex;
    justify-content: flex-end;
    gap: 0.5rem;
    padding: 0.6rem 0.7rem;
    border-top: 1px solid #e2ddd4;
    background: #fbfaf8;
  }

  .eim-cancel,
  .eim-insert {
    font-size: 0.78rem;
    padding: 0.35rem 0.9rem;
    border-radius: 6px;
    cursor: pointer;
  }

  .eim-cancel {
    border: 1px solid #c0b8ae;
    background: #fff;
    color: #3d3d3d;
  }

  .eim-cancel:hover {
    background: #f2efe9;
  }

  .eim-insert {
    border: 1px solid #5b8edc;
    background: #5b8edc;
    color: #fff;
    font-weight: 600;
  }

  .eim-insert:hover:not(:disabled) {
    background: #4a7dcb;
  }

  .eim-insert:disabled {
    border-color: #c9c4b8;
    background: #e5e1d8;
    color: #9ca3af;
    cursor: default;
  }
</style>
