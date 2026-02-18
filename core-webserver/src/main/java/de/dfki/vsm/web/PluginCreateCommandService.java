package de.dfki.vsm.web;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Handles ProjectConfig.Plugin.Create command.
 */
public final class PluginCreateCommandService {

    public interface Context {
        JSONObject errorResponse(String code, String message);

        JSONObject pluginSpec(String className);

        JSONObject variablesSpec(String className);
    }

    public JSONObject dispatch(final JSONObject params, final Context context) {
        String className = params.optString("className", "").trim();
        String name = params.optString("name", "").trim();
        String type = params.optString("type", "device").trim();
        boolean load = params.optBoolean("load", true);

        if (className.isEmpty()) {
            return context.errorResponse("BAD_REQUEST", "Missing className");
        }

        JSONObject plugin = new JSONObject();
        plugin.put("name", name);
        plugin.put("className", className);
        plugin.put("type", type);
        plugin.put("load", load);

        JSONArray features = new JSONArray();
        JSONArray sceneflowVars = new JSONArray();
        Map<String, String> configDefaults = new HashMap<>();
        Map<String, String> configSceneFlowTypes = new HashMap<>();
        Set<String> sceneflowVarDedup = new HashSet<>();

        JSONObject pluginSpec = context.pluginSpec(className);
        if (pluginSpec != null) {
            JSONArray optional = pluginSpec.optJSONArray("optional");
            JSONArray required = pluginSpec.optJSONArray("required");
            if (required != null) {
                collectConfigFeatures(required, true, features, sceneflowVars, configDefaults, configSceneFlowTypes, sceneflowVarDedup);
            }
            if (optional != null) {
                collectConfigFeatures(optional, false, features, sceneflowVars, configDefaults, configSceneFlowTypes, sceneflowVarDedup);
            }
            JSONArray pluginSpecific = pluginSpec.optJSONArray("pluginSpecific");
            if (pluginSpecific != null) {
                collectConfigFeatures(pluginSpecific, true, features, sceneflowVars, configDefaults, configSceneFlowTypes, sceneflowVarDedup);
            }
        }

        JSONObject variables = context.variablesSpec(className);
        if (variables != null) {
            JSONArray writes = variables.optJSONArray("writes");
            if (writes != null) {
                for (int i = 0; i < writes.length(); i++) {
                    JSONObject write = writes.optJSONObject(i);
                    if (write == null) {
                        continue;
                    }
                    String rawVar = write.optString("var", "").trim();
                    if (rawVar.isEmpty()) {
                        continue;
                    }
                    String resolvedVarName = configDefaults.getOrDefault(rawVar, rawVar).trim();
                    String resolvedType = write.optString("type", "").trim();
                    if (resolvedType.isEmpty()) {
                        resolvedType = configSceneFlowTypes.getOrDefault(rawVar, "").trim();
                    }
                    if (resolvedVarName.isEmpty() || resolvedType.isEmpty()) {
                        continue;
                    }
                    addSceneflowVar(sceneflowVars, sceneflowVarDedup, resolvedVarName, resolvedType);
                }
            }
        }

        plugin.put("features", features);
        JSONObject templates = pluginSpec != null ? pluginSpec.optJSONObject("templates") : null;

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("plugin", plugin);
        response.put("sceneflowVars", sceneflowVars);
        if (templates != null) {
            response.put("templates", templates);
        }
        return response;
    }

    private void collectConfigFeatures(final JSONArray items,
                                       final boolean includeAsFeature,
                                       final JSONArray features,
                                       final JSONArray sceneflowVars,
                                       final Map<String, String> configDefaults,
                                       final Map<String, String> configSceneFlowTypes,
                                       final Set<String> sceneflowVarDedup) {
        for (int i = 0; i < items.length(); i++) {
            JSONObject item = items.optJSONObject(i);
            if (item == null) {
                continue;
            }
            String key = item.optString("name", "").trim();
            if (key.isEmpty()) {
                continue;
            }
            Object defaultVal = item.opt("default");
            String value = defaultVal != null ? String.valueOf(defaultVal) : "";
            if (includeAsFeature) {
                JSONObject feature = new JSONObject();
                feature.put("key", key);
                feature.put("value", value);
                features.put(feature);
            }
            if (includeAsFeature) {
                configDefaults.put(key, value);
            } else {
                configDefaults.putIfAbsent(key, value);
            }

            String sceneflowType = item.optString("sceneflowtype", "").trim();
            if (!sceneflowType.isEmpty()) {
                if (includeAsFeature) {
                    configSceneFlowTypes.put(key, sceneflowType);
                } else {
                    configSceneFlowTypes.putIfAbsent(key, sceneflowType);
                }
                if (!value.isEmpty()) {
                    addSceneflowVar(sceneflowVars, sceneflowVarDedup, value, sceneflowType);
                }
            }
        }
    }

    private void addSceneflowVar(final JSONArray sceneflowVars,
                                 final Set<String> dedup,
                                 final String name,
                                 final String type) {
        String dedupKey = name + "::" + type;
        if (!dedup.add(dedupKey)) {
            return;
        }
        JSONObject varDef = new JSONObject();
        varDef.put("name", name);
        varDef.put("type", type);
        sceneflowVars.put(varDef);
    }
}
