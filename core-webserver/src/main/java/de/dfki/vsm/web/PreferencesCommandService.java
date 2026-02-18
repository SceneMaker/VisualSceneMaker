package de.dfki.vsm.web;

import org.json.JSONObject;

import java.util.function.Consumer;

/**
 * Handles Preferences.Update command.
 */
public final class PreferencesCommandService {

    public interface Context {
        JSONObject errorResponse(String code, String message);

        void removePreference(String key);

        void setPreference(String key, String value);

        void savePreferences();

        JSONObject preferencesToJson();
    }

    public JSONObject dispatch(final JSONObject params,
                               final Consumer<String> broadcaster,
                               final Context context) {
        JSONObject values = params.optJSONObject("values");
        if (values == null) {
            return context.errorResponse("BAD_REQUEST", "Missing values");
        }
        for (String key : values.keySet()) {
            Object raw = values.get(key);
            if (raw == null || raw == JSONObject.NULL) {
                context.removePreference(key);
            } else {
                context.setPreference(key, String.valueOf(raw));
            }
        }
        context.savePreferences();
        JSONObject prefs = context.preferencesToJson();
        if (broadcaster != null) {
            JSONObject evt = new JSONObject();
            evt.put("event", "system.preferences");
            evt.put("preferences", prefs);
            broadcaster.accept(evt.toString());
        }
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("preferences", prefs);
        return response;
    }
}
