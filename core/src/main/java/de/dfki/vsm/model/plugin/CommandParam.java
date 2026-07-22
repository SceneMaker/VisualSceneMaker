package de.dfki.vsm.model.plugin;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A single parameter declared by a plugin command in plugin-properties.json, e.g.
 * {@code {"name": "intensity", "type": "string", "required": true, "default": "1.0"}}.
 * Parsing is lenient: every field falls back to a safe default so existing
 * plugin-properties.json files keep parsing unchanged.
 *
 * @author Patrick Gebhard
 */
public final class CommandParam {

    private final String mName;
    private final String mType;
    private final boolean mRequired;
    private final String mDefault;
    private final String mDescription;
    private final List<String> mEnum;

    public CommandParam(
            final String name,
            final String type,
            final boolean required,
            final String defaultValue,
            final String description,
            final List<String> enumValues) {
        mName = name;
        mType = (type == null || type.isEmpty()) ? "string" : type;
        mRequired = required;
        mDefault = defaultValue;
        mDescription = description;
        mEnum = (enumValues == null) ? Collections.emptyList() : enumValues;
    }

    public final String getName() {
        return mName;
    }

    public final String getType() {
        return mType;
    }

    public final boolean isRequired() {
        return mRequired;
    }

    public final String getDefault() {
        return mDefault;
    }

    public final String getDescription() {
        return mDescription;
    }

    public final List<String> getEnum() {
        return mEnum;
    }

    public static CommandParam fromJson(final JSONObject json) {
        final String name = json.optString("name", null);
        final String type = json.optString("type", "string");
        final boolean required = json.optBoolean("required", false);
        final String defaultValue = json.has("default") ? json.opt("default").toString() : null;
        final String description = json.optString("description", null);
        final JSONArray enumJson = json.optJSONArray("enum");
        final List<String> enumValues = new ArrayList<>();
        if (enumJson != null) {
            for (int i = 0; i < enumJson.length(); i++) {
                enumValues.add(enumJson.optString(i));
            }
        }
        return new CommandParam(name, type, required, defaultValue, description, enumValues);
    }
}
