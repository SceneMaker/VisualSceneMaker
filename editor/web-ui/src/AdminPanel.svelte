<script>
  // Phase 3 (doc/vsm-workspace-platform-plan.md): minimal admin UI for
  // ProjectAssignmentTable — list/assign/unassign users and the admin flag,
  // so this no longer requires hand-editing the flat JSON file on the server.
  export let open = false;
  export let onClose = () => {};
  export let apiFetch; // from App.svelte — attaches Authorization automatically

  let users = []; // [{ userId, admin, projects: string[], projectsText }]
  let loading = false;
  let error = "";
  let newUserId = "";

  function rowFromEntry(userId, entry) {
    const projects = Array.isArray(entry.projects) ? entry.projects : [];
    return { userId, admin: !!entry.admin, projects, projectsText: projects.join("\n") };
  }

  // apiFetch (App.svelte) returns already-parsed JSON on success and *throws* an Error
  // (with the server's response body as the message) on any non-2xx status — it never
  // hands back a raw Response, so there's no res.ok/res.json() to call here.
  async function load() {
    loading = true;
    error = "";
    try {
      const data = await apiFetch("/api/v1/admin/users", { method: "GET" });
      const entries = data?.users || {};
      users = Object.keys(entries)
        .sort()
        .map((userId) => rowFromEntry(userId, entries[userId]));
    } catch (err) {
      error = err?.message || "Failed to load users.";
    } finally {
      loading = false;
    }
  }

  async function save(row) {
    error = "";
    const projects = row.projectsText
      .split("\n")
      .map((p) => p.trim())
      .filter((p) => p.length > 0);
    try {
      await apiFetch(`/api/v1/admin/users/${encodeURIComponent(row.userId)}`, {
        method: "PUT",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ admin: row.admin, projects })
      });
      await load();
    } catch (err) {
      error = err?.message || "Failed to save.";
    }
  }

  async function remove(row) {
    error = "";
    try {
      await apiFetch(`/api/v1/admin/users/${encodeURIComponent(row.userId)}`, { method: "DELETE" });
      await load();
    } catch (err) {
      error = err?.message || "Failed to remove.";
    }
  }

  async function addUser() {
    const userId = newUserId.trim();
    if (!userId) return;
    error = "";
    try {
      await apiFetch(`/api/v1/admin/users/${encodeURIComponent(userId)}`, {
        method: "PUT",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ admin: false, projects: [] })
      });
      newUserId = "";
      await load();
    } catch (err) {
      error = err?.message || "Failed to add user.";
    }
  }

  $: if (open) {
    load();
  }

  function handleBackdropClick() {
    onClose();
  }

  function handleKeydown(e) {
    if (e.key === "Escape") onClose();
  }
</script>

{#if open}
  <!-- svelte-ignore a11y-click-events-have-key-events -->
  <!-- svelte-ignore a11y-no-static-element-interactions -->
  <div class="modal-backdrop admin-panel-backdrop" role="presentation" on:click={handleBackdropClick}>
    <!-- svelte-ignore a11y-no-noninteractive-element-interactions -->
    <div
      class="modal admin-panel-modal"
      role="dialog"
      aria-modal="true"
      aria-labelledby="admin-panel-title"
      tabindex="-1"
      on:click|stopPropagation
      on:keydown={handleKeydown}
    >
      <div class="admin-panel-header">
        <h3 id="admin-panel-title">Project Assignments</h3>
        <button class="admin-panel-close" on:click={onClose} aria-label="Close">✕</button>
      </div>

      {#if error}
        <div class="admin-panel-error">{error}</div>
      {/if}

      {#if loading}
        <p>Loading…</p>
      {:else}
        <table class="admin-panel-table">
          <thead>
            <tr>
              <th>User</th>
              <th>Admin</th>
              <th>Assigned projects (one path per line)</th>
              <th></th>
            </tr>
          </thead>
          <tbody>
            {#each users as row (row.userId)}
              <tr>
                <td class="admin-panel-userid">{row.userId}</td>
                <td><input type="checkbox" bind:checked={row.admin} /></td>
                <td>
                  <textarea rows="2" bind:value={row.projectsText}></textarea>
                </td>
                <td class="admin-panel-actions">
                  <button on:click={() => save(row)}>Save</button>
                  <button class="admin-panel-danger" on:click={() => remove(row)}>Remove</button>
                </td>
              </tr>
            {/each}
            <tr>
              <td>
                <input
                  type="text"
                  placeholder="username"
                  bind:value={newUserId}
                  on:keydown={(e) => e.key === "Enter" && addUser()}
                />
              </td>
              <td colspan="2"></td>
              <td class="admin-panel-actions">
                <button on:click={addUser}>Add</button>
              </td>
            </tr>
          </tbody>
        </table>
        <p class="admin-panel-hint">
          Project paths must match exactly what a project resolves to on this server
          (e.g. the path shown after "Open by path"). New users get no projects and
          non-admin until assigned here.
        </p>
      {/if}
    </div>
  </div>
{/if}

<style>
  .modal.admin-panel-modal {
    width: min(900px, 96vw);
    max-height: 85vh;
    overflow-y: auto;
    display: flex;
    flex-direction: column;
    gap: 0.75rem;
    padding: 1rem;
  }

  .admin-panel-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
  }

  .admin-panel-close {
    background: none;
    border: none;
    font-size: 1.1rem;
    cursor: pointer;
  }

  .admin-panel-error {
    color: #b91c1c;
    background: #fee2e2;
    border-radius: 4px;
    padding: 0.5rem 0.75rem;
  }

  .admin-panel-table {
    width: 100%;
    border-collapse: collapse;
  }

  .admin-panel-table th,
  .admin-panel-table td {
    border-bottom: 1px solid var(--border, #ddd);
    padding: 0.4rem 0.5rem;
    text-align: left;
    vertical-align: top;
  }

  .admin-panel-userid {
    font-family: monospace;
    white-space: nowrap;
  }

  .admin-panel-table textarea {
    width: 100%;
    font-family: monospace;
    font-size: 0.85rem;
  }

  .admin-panel-actions {
    white-space: nowrap;
  }

  .admin-panel-danger {
    color: #b91c1c;
  }

  .admin-panel-hint {
    font-size: 0.8rem;
    opacity: 0.75;
  }
</style>
