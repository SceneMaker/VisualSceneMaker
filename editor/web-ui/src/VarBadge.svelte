<script>
  import IconChevronDown from './icons/IconChevronDown.svelte';
  import IconChevronUp from './icons/IconChevronUp.svelte';

  export let title = "";
  export let subtitle = "";
  export let variables = [];    // [{line, description}]
  export let loading = false;
  export let error = "";
  export let expanded = true;
  export let x = 16;
  export let y = 16;
  export let w = 200;
  export let h = 150;
  export let color = "#f8f6f2";
  export let onDragStart = null;    // callback(event)
  export let onToggle = null;       // callback()
  export let onResizeStart = null;  // callback(event)

  function handlePointerDown(e) {
    if (e.target.closest('.var-badge-resize') || e.target.closest('.var-badge-content')) return;
    onDragStart?.(e);
  }
</script>

<div
  class="var-badge"
  style:left="{x}px"
  style:top="{y}px"
  style:width="{w}px"
  style:background={color}
  on:pointerdown|stopPropagation={handlePointerDown}
  on:mousedown|stopPropagation={handlePointerDown}
  role="presentation"
>
  <div class="var-badge-title">
    <span class="var-badge-title-text">{title}</span>
    {#if subtitle}
      <span class="var-badge-subtitle">{subtitle}</span>
    {/if}
    <button
      class="var-badge-toggle"
      type="button"
      on:click|stopPropagation={() => onToggle?.()}
      title={expanded ? 'Collapse' : 'Expand'}
      aria-label={expanded ? 'Collapse' : 'Expand'}
    >
      {#if expanded}
        <IconChevronUp className="var-badge-chevron" />
      {:else}
        <IconChevronDown className="var-badge-chevron" />
      {/if}
    </button>
  </div>
  {#if expanded}
    <div class="var-badge-content" style:max-height="{h}px">
      {#if error}
        <span class="var-badge-error">{error}</span>
      {:else if loading}
        <span class="var-badge-muted">Loading...</span>
      {:else if variables.length === 0}
        <span class="var-badge-muted">No variables.</span>
      {:else}
        {#each variables as v}
          <div class="var-badge-row" title={v.description || v.line}>{v.line}</div>
        {/each}
      {/if}
    </div>
    <div
      class="var-badge-resize"
      aria-hidden="true"
      on:pointerdown|stopPropagation={(e) => onResizeStart?.(e)}
      on:mousedown|stopPropagation={(e) => onResizeStart?.(e)}
    />
  {/if}
</div>

<style>
  .var-badge {
    position: absolute;
    z-index: 4;
    opacity: 0.92;
    border-radius: 10px;
    padding: 0.4rem 0.6rem 0.6rem;
    min-width: 120px;
    user-select: none;
    cursor: move;
    touch-action: none;
    pointer-events: auto;
    box-sizing: border-box;
  }

  .var-badge-title {
    display: flex;
    flex-direction: row;
    align-items: center;
    gap: 0.3rem;
    font-size: 0.78rem;
    font-weight: 600;
    color: #3d3d3d;
  }

  .var-badge-title-text {
    flex: 1;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .var-badge-subtitle {
    font-size: 0.72rem;
    font-weight: 400;
    color: #7a7a7a;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    max-width: 60px;
    flex-shrink: 1;
  }

  .var-badge-toggle {
    width: 18px;
    height: 18px;
    flex-shrink: 0;
    background: transparent;
    border: 1px solid #c0b8ae;
    border-radius: 4px;
    cursor: pointer;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 0;
    color: #5a5a5a;
  }

  .var-badge-toggle :global(svg) {
    width: 12px;
    height: 12px;
    display: block;
  }

  .var-badge-content {
    margin-top: 0.3rem;
    overflow-y: auto;
  }

  .var-badge-muted {
    font-size: 0.75rem;
    color: #9a9a9a;
  }

  .var-badge-error {
    font-size: 0.75rem;
    color: #c0392b;
  }

  .var-badge-row {
    font-size: 0.75rem;
    color: #3d3d3d;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    padding: 0.05rem 0;
    line-height: 1.3;
  }

  .var-badge-resize {
    position: absolute;
    bottom: 2px;
    right: 2px;
    width: 14px;
    height: 14px;
    cursor: se-resize;
    touch-action: none;
    background:
      linear-gradient(135deg, transparent 45%, #c0b8ae 45%, #c0b8ae 55%, transparent 55%),
      linear-gradient(135deg, transparent 65%, #c0b8ae 65%, #c0b8ae 75%, transparent 75%),
      linear-gradient(135deg, transparent 82%, #c0b8ae 82%);
    border-radius: 0 0 8px 0;
  }
</style>
