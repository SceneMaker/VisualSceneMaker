package de.dfki.vsm.runtime.activity;

import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.model.scenescript.ActionObject;

import java.util.LinkedList;

/**
 * Shared "does this inline action need to split the utterance and pause speech around it" rule,
 * used by both real playback ({@code ReactivePlayer.playScene()}) and authoring-time preview
 * ({@code CharamelEmbedExecutor.previewTurn()}) so the two never drift apart.
 *
 * <p>Two independent ways an inline action opts in:
 * <ul>
 *   <li>{@code pause} — its own action name, e.g. {@code [pause duration='500']}. Always a split
 *       point; it has no non-blocking form, since a "pause" that doesn't pause is meaningless. Pure
 *       timing — no visual effect, no target device, handled entirely by the caller (a plain sleep),
 *       never dispatched to any {@code ActivityExecutor}.</li>
 *   <li>A {@code blocking='true'} feature on some other action (today: {@code emotion} and its
 *       named-emotion aliases; later, gesture). Opt-in per action — most actions (timer, logging,
 *       a background-color change, ...) have no meaningful "duration" to wait out, so they stay
 *       fire-and-forget by default.</li>
 * </ul>
 *
 * @author Patrick Gebhard
 */
public final class ActionBlockingUtil {

    public static final String PAUSE_ACTION_NAME = "pause";
    public static final long PAUSE_DEFAULT_DURATION_MS = 500;

    private ActionBlockingUtil() {
    }

    /** True if this inline action must split the utterance and pause speech around it — either
     *  it's a {@code pause} (always) or it opted in via {@code blocking='true'}. */
    public static boolean requiresUtteranceSplit(final ActionObject action) {
        if (isPause(action)) {
            return true;
        }
        return hasBlockingFeature(action);
    }

    public static boolean isPause(final ActionObject action) {
        return action != null && PAUSE_ACTION_NAME.equalsIgnoreCase(action.getName());
    }

    /** {@code duration} feature of a {@code pause} action, in ms; {@link #PAUSE_DEFAULT_DURATION_MS}
     *  if absent or unparsable. */
    public static long parsePauseDurationMs(final ActionObject action) {
        return parseMsOrDefault(featureValue(action, "duration"), PAUSE_DEFAULT_DURATION_MS);
    }

    private static boolean hasBlockingFeature(final ActionObject action) {
        if (action == null) {
            return false;
        }
        final LinkedList<ActionFeature> features = action.getFeatureList();
        if (features == null) {
            return false;
        }
        for (final ActionFeature f : features) {
            if ("blocking".equalsIgnoreCase(f.getKey())) {
                return "true".equalsIgnoreCase(f.getValNoQuotes());
            }
        }
        return false;
    }

    private static String featureValue(final ActionObject action, final String key) {
        if (action == null) {
            return null;
        }
        final LinkedList<ActionFeature> features = action.getFeatureList();
        if (features == null) {
            return null;
        }
        for (final ActionFeature f : features) {
            if (key.equalsIgnoreCase(f.getKey())) {
                return f.getValNoQuotes();
            }
        }
        return null;
    }

    private static long parseMsOrDefault(final String val, final long defaultMs) {
        if (val == null || val.isBlank()) {
            return defaultMs;
        }
        try {
            return Math.round(Double.parseDouble(val.trim()));
        } catch (final NumberFormatException exc) {
            return defaultMs;
        }
    }
}
