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

    /** Port of this character's live preview page (its own HTTP+WebSocket server, separate from
     *  the main VSM server), or {@code -1} if not available (transport not started, or no such
     *  page — e.g. Android). Only the port, not a full URL: the caller (a remote browser over
     *  LAN, not necessarily the machine running VSM) must build the final URL against whatever
     *  host it used to reach the main server, not a server-side guess — see
     *  {@code CharacterPreviewPanel.svelte}'s use of {@code window.location.hostname}, mirroring
     *  the same pattern already used for the "follow the player" GUI URL. */
    int getPreviewPort();

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
     * Sends {@code rawText} verbatim to the character's speech engine, bypassing SceneScript
     * parsing entirely (no scene/turn grammar, no bracket-action splitting, no marker embedding).
     * A diagnostic escape hatch for testing what the underlying TTS/engine does with markup VSM's
     * own grammar can't represent or would choke on — e.g. embedded SSML ({@code <break .../>}) to
     * check whether the engine's TTS backend honors it. Not for authoring use.
     */
    void previewRawText(String rawText);

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
