package de.dfki.vsm.web;

import de.dfki.vsm.model.behavior.placement.PlacementContext;
import de.dfki.vsm.model.behavior.placement.PlacementModel;
import org.json.JSONObject;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeMap;

/**
 * Per-project placement observations and the model derived from them — {@code behavior-placement.json}.
 *
 * <p>The file stores the individual observations, not just the model's counts, and the model is
 * <em>rebuilt</em> from them whenever they change. That costs nothing at this scale and buys three
 * things a running tally cannot: syncing the same script twice is idempotent, deleting a command
 * actually removes its evidence rather than requiring a decrement that could drift negative, and
 * moving a command is a delete plus an insert with no special case.
 *
 * <p>Each observation is keyed by a fingerprint identifying the command's position in the script, so
 * re-analysing an unchanged script is a no-op.
 *
 * <p>Not thread-safe. {@link WebUiServer} confines one instance per project and synchronises on it.
 */
final class PlacementStore {

    static final int STORE_VERSION = 1;

    /** fingerprint → observation. Sorted so the persisted file has a stable diff. */
    private final Map<String, JSONObject> mObservations = new TreeMap<>();
    private PlacementModel mModel;

    private PlacementStore() {
    }

    static PlacementStore empty() {
        return new PlacementStore();
    }

    static PlacementStore load(final Path file) {
        final PlacementStore store = new PlacementStore();
        if (file == null || !Files.exists(file)) {
            return store;
        }
        try {
            final JSONObject json = new JSONObject(Files.readString(file, StandardCharsets.UTF_8));
            final JSONObject observations = json.optJSONObject("observations");
            if (observations != null) {
                for (final String key : observations.keySet()) {
                    final JSONObject entry = observations.optJSONObject(key);
                    if (entry != null) {
                        store.mObservations.put(key, entry);
                    }
                }
            }
        } catch (final Exception exc) {
            // A corrupt or hand-edited file must not take the editor down. Starting from no
            // observations degrades to the hand-written prior, which is the n=0 behaviour anyway.
            return new PlacementStore();
        }
        return store;
    }

    void save(final Path file) throws IOException {
        if (file == null) {
            return;
        }
        Files.writeString(file, toJson().toString(2), StandardCharsets.UTF_8);
    }

    /**
     * Build the fingerprint for a placement: which command, at which position in the script.
     *
     * <p>Position is part of the identity on purpose. Moving a command to a different slot is a
     * different placement decision and must replace the old one, not accumulate beside it.
     */
    static String fingerprint(
            final String plugin, final String command,
            final int line, final int sentence, final int tokenIndex) {
        return (plugin == null ? "" : plugin) + "|" + (command == null ? "" : command)
                + "|L" + line + "|S" + sentence + "|T" + tokenIndex;
    }

    static JSONObject observation(
            final String slot, final boolean snapped, final PlacementContext context) {
        final JSONObject out = new JSONObject();
        out.put("slot", slot);
        if (snapped) {
            out.put("snapped", true);
        }
        if (context.getFunction() != null) {
            out.put("function", context.getFunction());
        }
        if (context.getAffiliate() != null) {
            out.put("affiliate", context.getAffiliate());
        }
        if (context.getClauseType() != null) {
            out.put("clauseType", context.getClauseType());
        }
        out.put("turnPosition", context.getTurnPosition().name());
        if (context.getDialogueAct() != null) {
            out.put("dialogueAct", context.getDialogueAct());
        }
        return out;
    }

    /** @return true when the set actually changed, so the caller knows whether to persist. */
    boolean put(final String fingerprint, final JSONObject observation) {
        if (fingerprint == null || observation == null) {
            return false;
        }
        final JSONObject previous = mObservations.put(fingerprint, observation);
        final boolean changed = previous == null || !previous.similar(observation);
        if (changed) {
            mModel = null;
        }
        return changed;
    }

    boolean remove(final String fingerprint) {
        if (fingerprint == null || mObservations.remove(fingerprint) == null) {
            return false;
        }
        mModel = null;
        return true;
    }

    /**
     * Replace every observation belonging to one script scope with a fresh set.
     *
     * <p>Used by sync: the analysis reports all placements currently in the script, so anything
     * previously recorded for that scope and now absent has been deleted by the author. Scope is a
     * key prefix, which for a whole-script sync is the empty string.
     */
    SyncResult replaceScope(final String scopePrefix, final Map<String, JSONObject> current) {
        final Map<String, JSONObject> retained = new LinkedHashMap<>();
        int removed = 0;
        for (final Map.Entry<String, JSONObject> entry : mObservations.entrySet()) {
            final boolean inScope = scopePrefix == null || scopePrefix.isEmpty()
                    || entry.getKey().startsWith(scopePrefix);
            if (!inScope) {
                retained.put(entry.getKey(), entry.getValue());
            } else if (!current.containsKey(entry.getKey())) {
                removed += 1;
            }
        }
        int added = 0;
        int updated = 0;
        for (final Map.Entry<String, JSONObject> entry : current.entrySet()) {
            final JSONObject previous = mObservations.get(entry.getKey());
            if (previous == null) {
                added += 1;
            } else if (!previous.similar(entry.getValue())) {
                updated += 1;
            }
        }
        mObservations.clear();
        mObservations.putAll(retained);
        mObservations.putAll(current);
        mModel = null;
        return new SyncResult(added, updated, removed, mObservations.size());
    }

    /** The model implied by the current observations, rebuilt on demand and cached until they change. */
    PlacementModel model() {
        if (mModel == null) {
            final PlacementModel rebuilt = PlacementModel.empty();
            for (final JSONObject entry : mObservations.values()) {
                final String slot = entry.optString("slot", "");
                if (slot.isEmpty()) {
                    continue;
                }
                final PlacementContext context = new PlacementContext(
                        entry.optString("function", null),
                        entry.optString("affiliate", null),
                        entry.optString("clauseType", null),
                        turnPositionOf(entry.optString("turnPosition", "")),
                        entry.optString("dialogueAct", null));
                if (entry.optBoolean("snapped", false)) {
                    rebuilt.observeSnapped(context, slot);
                } else {
                    rebuilt.observe(context, slot);
                }
            }
            mModel = rebuilt;
        }
        return mModel;
    }

    private static PlacementContext.TurnPosition turnPositionOf(final String name) {
        try {
            return PlacementContext.TurnPosition.valueOf(name);
        } catch (final IllegalArgumentException exc) {
            return PlacementContext.TurnPosition.UNKNOWN;
        }
    }

    int size() {
        return mObservations.size();
    }

    JSONObject toJson() {
        final JSONObject observations = new JSONObject();
        for (final Map.Entry<String, JSONObject> entry : mObservations.entrySet()) {
            observations.put(entry.getKey(), entry.getValue());
        }
        return new JSONObject()
                .put("version", STORE_VERSION)
                .put("updatedAt", java.time.Instant.now().toString())
                .put("observations", observations)
                .put("model", model().toJson());
    }

    /** What a sync changed — reported back so the caller can see the model move. */
    static final class SyncResult {
        final int added;
        final int updated;
        final int removed;
        final int total;

        SyncResult(final int added, final int updated, final int removed, final int total) {
            this.added = added;
            this.updated = updated;
            this.removed = removed;
            this.total = total;
        }

        boolean changed() {
            return added > 0 || updated > 0 || removed > 0;
        }

        JSONObject toJson() {
            return new JSONObject()
                    .put("added", added)
                    .put("updated", updated)
                    .put("removed", removed)
                    .put("total", total);
        }
    }
}
