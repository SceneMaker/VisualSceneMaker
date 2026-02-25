package de.dfki.vsm.sceneflow.ir;

import java.util.Locale;
import java.util.Map;

public final class EdgeLabelMapper {

    private static final Map<String, String> CANONICAL_TO_HUMAN = Map.of(
            "TEDGE", "timeout edge",
            "EEDGE", "unconditional edge",
            "CEDGE", "conditional edge",
            "PEDGE", "probabilistic edge",
            "FEDGE", "fork edge",
            "IEDGE", "interrupt edge"
    );

    private EdgeLabelMapper() {
    }

    public static String toHumanLabel(final String rawEdgeLabel) {
        final String canonical = canonicalize(rawEdgeLabel);
        return CANONICAL_TO_HUMAN.getOrDefault(canonical, fallbackHuman(rawEdgeLabel));
    }

    public static String canonicalize(final String rawEdgeLabel) {
        if (rawEdgeLabel == null || rawEdgeLabel.isBlank()) {
            return "";
        }
        final String compact = rawEdgeLabel
                .trim()
                .toUpperCase(Locale.ROOT)
                .replace("-", "")
                .replace("_", "")
                .replace(" ", "");

        if (compact.equals("TEdge".toUpperCase(Locale.ROOT)) || compact.equals("T")) {
            return "TEDGE";
        }
        if (compact.equals("EEdge".toUpperCase(Locale.ROOT)) || compact.equals("E")) {
            return "EEDGE";
        }
        if (compact.equals("CEdge".toUpperCase(Locale.ROOT)) || compact.equals("C")) {
            return "CEDGE";
        }
        if (compact.equals("PEdge".toUpperCase(Locale.ROOT)) || compact.equals("P")) {
            return "PEDGE";
        }
        if (compact.equals("FEdge".toUpperCase(Locale.ROOT)) || compact.equals("F")) {
            return "FEDGE";
        }
        if (compact.equals("IEdge".toUpperCase(Locale.ROOT)) || compact.equals("I") || compact.equals("IEGDE")) {
            return "IEDGE";
        }
        return compact;
    }

    private static String fallbackHuman(final String rawEdgeLabel) {
        if (rawEdgeLabel == null || rawEdgeLabel.isBlank()) {
            return "edge";
        }
        final String compact = rawEdgeLabel.trim().toLowerCase(Locale.ROOT).replace('_', ' ');
        return compact.endsWith("edge") ? compact : compact + " edge";
    }
}
