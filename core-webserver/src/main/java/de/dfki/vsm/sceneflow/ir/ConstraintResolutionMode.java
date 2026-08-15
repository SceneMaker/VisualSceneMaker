package de.dfki.vsm.sceneflow.ir;

import java.util.Locale;

public enum ConstraintResolutionMode {
    STRICT,
    PERMISSIVE;

    public static ConstraintResolutionMode from(final String value) {
        if (value == null || value.isBlank()) {
            return PERMISSIVE;
        }
        return "strict".equals(value.trim().toLowerCase(Locale.ROOT)) ? STRICT : PERMISSIVE;
    }
}

