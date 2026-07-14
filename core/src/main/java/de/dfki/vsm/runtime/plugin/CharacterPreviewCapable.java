package de.dfki.vsm.runtime.plugin;

/**
 * Optional capability for a {@link RunTimePlugin} whose character can be driven directly for
 * authoring-time preview, independent of any running Interpreter/SceneFlow. A plugin implementing
 * this is discoverable generically (e.g. via a {@code "previewCapable": true} flag in its
 * plugin-properties.json) — no plugin-id special-casing in callers.
 *
 * @author Patrick Gebhard
 */
public interface CharacterPreviewCapable {

    /** Base URL of this character's live preview page (e.g. {@code http://localhost:3040/character.html}),
     *  or {@code null} if not available yet (e.g. transport not started). */
    String getPreviewUrl();

    /**
     * Parses and performs a raw turn (the same syntax authored in the Script Editor, e.g.
     * {@code "Xenia: Lass mich [emotion type='happy' intensity='1.0'] einen Vorschlag machen."}),
     * non-blocking, without requiring a running Interpreter. Embedded actions addressed at a
     * different actor (e.g. {@code "[Bob smile]"}) are routed to that agent's own device, if resolvable.
     */
    void previewTurn(String rawTurn);

    /**
     * Parses and performs a single standalone bracket-body action command (e.g.
     * {@code "emotion type='happy' intensity='1.0'"}), non-blocking.
     */
    void previewAction(String rawActionBody);

    /**
     * Mutes or unmutes commands to the authoring-time preview page specifically, leaving every
     * other connected viewer (e.g. a "follow the player" audience page embedding the same
     * character) untouched. Without this, a real SceneFlow run broadcasts every speak/emotion
     * command to *all* connected pages — the preview panel and an audience-facing page both run
     * their own character engine and both audibly perform the same line at once. Muted while the
     * real Interpreter is running; unmuted when stopped/paused, so per-turn preview testing keeps
     * working between runs.
     */
    void setPreviewMuted(boolean muted);
}
