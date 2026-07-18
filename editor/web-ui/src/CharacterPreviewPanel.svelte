<script>
  import { onMount, onDestroy } from "svelte";

  export let instanceName = "";
  export let displayName = "";
  export let projectId = null;
  export let x = 300;
  export let y = 60;
  export let w = 360;
  export let h = 320;
  export let z = 0;
  export let open = true; // CSS-hidden (not unmounted) when false — keeps the iframe/WS alive so background loading isn't lost
  export let apiGet;
  export let onDragStart = null;   // callback(event)
  export let onResizeStart = null; // callback(event)
  export let onClose = null;       // callback()
  export let onFocus = null;       // callback() — raise this panel above others
  export let onProgress = null;    // callback(0-100) — character model load progress
  export let onSpeaking = null;    // callback(boolean) — true while this character is actively speaking
  // True while a real SceneFlow run is active (see App.svelte's previewSuspendedByRuntime). The
  // character engine (VuppetMaster) keeps its own persistent connection to its cloud backend for
  // as long as this iframe exists, entirely independent of our own WS mute mechanism — muting only
  // stops VSM from sending it speak commands, it does NOT disconnect the engine itself. A real run
  // opens its own separate audience-facing character page using the same license, and two engine
  // instances live at once reliably breaks both (confirmed 2026-07-18: every subsequent speak
  // failed with an opaque engine-side error until the second session was closed). So the iframe is
  // torn down here for the run's duration and reloaded fresh once it ends, rather than just muted.
  export let suspended = false;

  let previewUrl = null;
  let loadError = "";
  let loading = true;
  let iframeEl;
  let wasSuspended = false;

  function handlePreviewMessage(event) {
    if (!iframeEl || event.source !== iframeEl.contentWindow) return;
    const progress = event.data?.vsmPreviewProgress;
    if (typeof progress === "number") {
      onProgress?.(progress);
    }
    const speaking = event.data?.vsmSpeaking;
    if (typeof speaking === "boolean") {
      onSpeaking?.(speaking);
    }
  }

  onMount(() => {
    window.addEventListener("message", handlePreviewMessage);
  });
  onDestroy(() => {
    window.removeEventListener("message", handlePreviewMessage);
  });

  async function loadPreviewInfo() {
    loading = true;
    loadError = "";
    previewUrl = null;
    try {
      const data = await apiGet(`/api/v1/projects/${projectId}/plugins/${instanceName}/preview`);
      // The server only returns the character page's own port — a "localhost"-based URL would
      // only work for whoever is running VSM itself, not a remote LAN collaborator (their
      // browser's own localhost has nothing listening there). Build the final URL against the
      // host this browser actually used to reach the server, mirroring htmlGuiUrl()'s pattern.
      if (data?.previewPort) {
        const host = (typeof window !== "undefined" && window.location.hostname) || "localhost";
        const scheme = data.previewSecure ? "https" : "http";
        previewUrl = `${scheme}://${host}:${data.previewPort}${data.previewPath || "/character.html"}`;
      } else {
        previewUrl = null;
      }
    } catch (err) {
      loadError = err?.message || "Failed to load preview";
    } finally {
      loading = false;
    }
  }

  // Reload whenever the panel is (re)pointed at a different project/instance.
  $: if (projectId && instanceName && !suspended) {
    loadPreviewInfo();
  }

  // Tear the iframe down for the duration of a real run, reload it fresh once the run ends.
  $: if (suspended !== wasSuspended) {
    wasSuspended = suspended;
    if (suspended) {
      previewUrl = null;
      loading = false;
      loadError = "";
    } else if (projectId && instanceName) {
      loadPreviewInfo();
    }
  }
</script>

<div
  class="preview-panel"
  style:left="{x}px"
  style:top="{y}px"
  style:width="{w}px"
  style:height="{h}px"
  style:z-index={5 + z}
  style:display={open ? null : "none"}
  on:pointerdown|capture={() => onFocus?.()}
  on:mousedown|capture={() => onFocus?.()}
  role="presentation"
>
  <div
    class="preview-panel-title"
    on:pointerdown|stopPropagation={(e) => onDragStart?.(e)}
    on:mousedown|stopPropagation={(e) => onDragStart?.(e)}
  >
    <span class="preview-panel-title-text" title={instanceName}>{displayName || instanceName}</span>
    <button
      type="button"
      class="preview-panel-close"
      on:pointerdown|stopPropagation
      on:click|stopPropagation={() => onClose?.()}
      title="Close preview"
      aria-label="Close preview"
    >
      &#10005;
    </button>
  </div>

  <div class="preview-panel-body">
    {#if suspended}
      <div class="preview-panel-message">A SceneFlow run is active — preview paused to avoid a duplicate character connection.</div>
    {:else if loadError}
      <div class="preview-panel-message preview-panel-error">{loadError}</div>
    {:else if loading}
      <div class="preview-panel-message">Loading preview…</div>
    {:else if previewUrl}
      <iframe
        class="preview-panel-frame"
        src={previewUrl}
        title="{displayName || instanceName} preview"
        allow="autoplay"
        bind:this={iframeEl}
      ></iframe>
    {:else}
      <div class="preview-panel-message">No preview page available.</div>
    {/if}
  </div>

  <div
    class="preview-panel-resize"
    aria-hidden="true"
    on:pointerdown|stopPropagation={(e) => onResizeStart?.(e)}
    on:mousedown|stopPropagation={(e) => onResizeStart?.(e)}
  />
</div>

<style>
  .preview-panel {
    position: fixed;
    z-index: 5;
    display: flex;
    flex-direction: column;
    background: #fff;
    border: 1px solid #c0b8ae;
    border-radius: 10px;
    box-shadow: 0 6px 18px rgba(0, 0, 0, 0.18);
    box-sizing: border-box;
    overflow: hidden;
    pointer-events: auto;
  }

  .preview-panel-title {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    padding: 0.4rem 0.5rem;
    font-size: 0.78rem;
    font-weight: 600;
    color: #3d3d3d;
    background: #f8f6f2;
    border-bottom: 1px solid #e2ddd4;
    cursor: move;
    touch-action: none;
    user-select: none;
    flex-shrink: 0;
  }

  .preview-panel-title-text {
    flex: 1;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .preview-panel-close {
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
    font-size: 0.65rem;
    line-height: 1;
    color: #5a5a5a;
  }

  .preview-panel-close:hover {
    background: #efe9e0;
  }

  .preview-panel-body {
    flex: 1;
    min-height: 0;
    position: relative;
    background: #000;
  }

  .preview-panel-message {
    position: absolute;
    inset: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 0.5rem;
    font-size: 0.78rem;
    color: #ccc;
    text-align: center;
  }

  .preview-panel-error {
    color: #ff8a80;
  }

  .preview-panel-frame {
    width: 100%;
    height: 100%;
    border: none;
    display: block;
  }

  .preview-panel-resize {
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
