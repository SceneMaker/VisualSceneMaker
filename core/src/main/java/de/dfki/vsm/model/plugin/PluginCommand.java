package de.dfki.vsm.model.plugin;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A single entry of a plugin's declared {@code commands} array in plugin-properties.json —
 * the typed counterpart of the raw {@link JSONObject} that {@code WebUiServer} carried around
 * until now. Parsing is lenient: every field falls back to a safe default so existing
 * plugin-properties.json files keep parsing unchanged.
 *
 * @author Patrick Gebhard
 */
public final class PluginCommand {

    private final String mName;
    private final String mType;
    private final String mSummary;
    private final String mDescription;
    private final String mUiCategory;
    private final List<CommandParam> mParams;

    public PluginCommand(
            final String name,
            final String type,
            final String summary,
            final String description,
            final String uiCategory,
            final List<CommandParam> params) {
        mName = name;
        mType = (type == null || type.isEmpty()) ? "action" : type;
        mSummary = summary;
        mDescription = description;
        mUiCategory = uiCategory;
        mParams = (params == null) ? Collections.emptyList() : params;
    }

    public final String getName() {
        return mName;
    }

    public final String getType() {
        return mType;
    }

    public final String getSummary() {
        return mSummary;
    }

    public final String getDescription() {
        return mDescription;
    }

    /** Optional grouping hint (e.g. "emotion"/"background"/"gesture") consumed by preview UIs
     *  such as SiaPanel to lay commands out in deliberate columns instead of one per command. */
    public final String getUiCategory() {
        return mUiCategory;
    }

    public final List<CommandParam> getParams() {
        return mParams;
    }

    public static PluginCommand fromJson(final JSONObject json) {
        final String name = json.optString("name", null);
        final String type = json.optString("type", "action");
        final String summary = json.optString("summary", null);
        final String description = json.optString("description", null);
        final String uiCategory = json.optString("uiCategory", null);
        final JSONArray paramsJson = json.optJSONArray("params");
        final List<CommandParam> params = new ArrayList<>();
        if (paramsJson != null) {
            for (int i = 0; i < paramsJson.length(); i++) {
                final JSONObject paramJson = paramsJson.optJSONObject(i);
                if (paramJson != null) {
                    params.add(CommandParam.fromJson(paramJson));
                }
            }
        }
        return new PluginCommand(name, type, summary, description, uiCategory, params);
    }

    public static List<PluginCommand> fromJsonArray(final JSONArray json) {
        final List<PluginCommand> commands = new ArrayList<>();
        if (json != null) {
            for (int i = 0; i < json.length(); i++) {
                final JSONObject commandJson = json.optJSONObject(i);
                if (commandJson != null) {
                    commands.add(PluginCommand.fromJson(commandJson));
                }
            }
        }
        return commands;
    }
}
