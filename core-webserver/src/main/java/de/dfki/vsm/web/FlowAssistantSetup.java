package de.dfki.vsm.web;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * What a project would have to gain before a described situation can work, and why.
 *
 * <p>A flow is only half of what an interaction needs. The other half is a device that can show
 * something, an agent to speak through it, and a screen with a control on it. None of those can be
 * expressed as an operation on a flow, which is why they are planned separately and carried out
 * before the flow is applied. See {@code doc/sceneflow-modelling-support-concept.md} section 4b.
 *
 * <p>Each step says what it adds and why that is needed, because an author who has never built one
 * of these has no way of knowing that a scene without a device plays to nobody.
 */
final class FlowAssistantSetup {

    /** Where the screen templates the editor offers live on the classpath. */
    private static final String TEMPLATE_INDEX = "/web-ui/screen-templates/index.json";

    /** One thing to add, in the order it has to be added. */
    record Step(String kind, String label, String why, String pluginClass, String templateId,
                String deviceName, String agentName) {

        JSONObject toJson() {
            final JSONObject out = new JSONObject()
                    .put("kind", kind)
                    .put("label", label)
                    .put("why", why);
            if (deviceName != null && !deviceName.isBlank()) {
                out.put("deviceName", deviceName);
            }
            if (agentName != null && !agentName.isBlank()) {
                out.put("agentName", agentName);
            }
            if (templateId != null && !templateId.isBlank()) {
                out.put("templateId", templateId);
            }
            return out;
        }
    }

    private final List<Step> mSteps = new ArrayList<>();

    private FlowAssistantSetup() {
    }

    List<Step> steps() {
        return mSteps;
    }

    boolean isEmpty() {
        return mSteps.isEmpty();
    }

    JSONArray toJson() {
        final JSONArray out = new JSONArray();
        for (Step step : mSteps) {
            out.put(step.toJson());
        }
        return out;
    }

    /**
     * Plans what this project is missing for {@code situation}.
     *
     * @param installedPlugins every plugin class this deployment carries, as class to display name
     */
    static FlowAssistantSetup plan(final JSONObject capabilities,
                                   final String situation,
                                   final Map<String, String> installedPlugins) {
        final FlowAssistantSetup plan = new FlowAssistantSetup();
        final String lower = situation == null ? "" : situation.toLowerCase(Locale.ROOT);

        final boolean needsAnAnswer = lower.contains("answer") || lower.contains("reply")
                || lower.contains("ask") || lower.contains("respond") || lower.contains("choose")
                || lower.contains("press") || lower.contains("button");

        final boolean hasAgent = !agentNames(capabilities).isEmpty();
        final String uiClass = classProviding(installedPlugins, "htmlgui");
        final boolean hasUi = usesPlugin(capabilities, "htmlgui");

        // The device first: an agent needs one, and a screen belongs to one.
        String deviceName = pluginNameFor(capabilities, "htmlgui");
        if (!hasUi && !uiClass.isEmpty() && (needsAnAnswer || !hasAgent)) {
            deviceName = freeDeviceName(capabilities, "webpage");
            plan.mSteps.add(new Step(
                    "device",
                    "Add the " + installedPlugins.get(uiClass) + " as a device called “"
                            + deviceName + "”",
                    "A device is the thing a flow can actually talk to. Without one, a step that "
                            + "says something has nowhere to say it, and nothing appears anywhere.",
                    uiClass, "", deviceName, ""));
        }

        if (!hasAgent && !deviceName.isBlank()) {
            final String agentName = freeAgentName(capabilities, "alex");
            plan.mSteps.add(new Step(
                    "agent",
                    "Add an agent called “" + agentName + "” on “" + deviceName + "”",
                    "Scenes are written as lines someone says, so every line needs a someone. The "
                            + "agent is the name you write in front of a line in the script.",
                    "", "", deviceName, agentName));
        }

        // The screen last, because it belongs to the device and is only needed when the person has
        // to give something back.
        if (needsAnAnswer && !deviceName.isBlank() && screenTemplate(uiClass) != null
                && !hasScreenThatWrites(capabilities)) {
            final JSONObject template = screenTemplate(uiClass);
            plan.mSteps.add(new Step(
                    "screen",
                    "Add the “" + template.optString("label") + "” screen",
                    "Waiting for an answer only ends when something hands one back. This screen has "
                            + "a box the person types into, and what they type arrives in the "
                            + "variable the flow waits on.",
                    "", template.optString("id"), deviceName, ""));
        }
        return plan;
    }

    /**
     * The snapshot as it would be once this plan has been carried out.
     *
     * <p>The flow is generated against this rather than against what the project has today, so the
     * proposal is one coherent thing: a flow that waits on the screen this plan adds, rather than a
     * flow that waits on nothing and a note saying it will not work.
     *
     * <p>Deliberately not added: the flow variables the new plugin brings. Leaving them out is what
     * makes the generator declare them itself, so applying the flow and applying the plan cannot
     * disagree about which of them owns the declaration.
     */
    JSONObject project(final JSONObject capabilities) {
        if (isEmpty()) {
            return capabilities;
        }
        final JSONObject projected = new JSONObject(capabilities.toString());
        final JSONObject project = projected.optJSONObject("project");
        if (project == null) {
            return projected;
        }
        final JSONArray plugins = project.optJSONArray("plugins") == null
                ? new JSONArray()
                : project.getJSONArray("plugins");
        final JSONArray agents = project.optJSONArray("agents") == null
                ? new JSONArray()
                : project.getJSONArray("agents");

        for (Step step : mSteps) {
            switch (step.kind()) {
                case "device" -> plugins.put(new JSONObject()
                        .put("name", step.deviceName())
                        .put("className", step.pluginClass())
                        .put("type", "device")
                        .put("load", true)
                        .put("commands", declaredCommands(step.pluginClass()))
                        .put("writesVariables", declaredVariables(step.pluginClass(), "writes"))
                        .put("readsVariables", declaredVariables(step.pluginClass(), "reads")));
                case "agent" -> agents.put(new JSONObject()
                        .put("name", step.agentName())
                        .put("device", step.deviceName())
                        .put("features", new JSONArray()));
                case "screen" -> addProjectedScreen(projected, step.templateId());
                default -> {
                }
            }
        }
        project.put("plugins", plugins);
        project.put("agents", agents);
        return projected;
    }

    private void addProjectedScreen(final JSONObject projected, final String templateId) {
        final JSONObject template = readTemplate(templateId);
        if (template == null) {
            return;
        }
        final JSONObject screens = projected.optJSONObject("screens") == null
                ? projected.put("screens", new JSONObject().put("screens", new JSONArray()))
                        .getJSONObject("screens")
                : projected.getJSONObject("screens");
        final JSONArray list = screens.optJSONArray("screens") == null
                ? screens.put("screens", new JSONArray()).getJSONArray("screens")
                : screens.getJSONArray("screens");

        final JSONObject defined = template.optJSONObject("screens");
        for (String name : defined == null ? Set.<String>of() : defined.keySet()) {
            final Set<String> reads = new LinkedHashSet<>();
            final Set<String> writes = new LinkedHashSet<>();
            collectBindings(defined.opt(name), reads, writes);
            list.put(new JSONObject()
                    .put("name", name)
                    .put("readsVariables", new JSONArray(new ArrayList<>(reads)))
                    .put("writesVariables", new JSONArray(new ArrayList<>(writes))));
        }
    }

    /** The screen template bound to this deployment's user-input plugin, or null. */
    static JSONObject screenTemplate(final String pluginClass) {
        if (pluginClass == null || pluginClass.isBlank()) {
            return null;
        }
        final JSONArray index = readTemplateIndex();
        for (int i = 0; index != null && i < index.length(); i++) {
            final JSONObject entry = index.optJSONObject(i);
            if (entry == null) {
                continue;
            }
            // The chat screen is the one with a control that sends what the person types, which is
            // what an ask-and-wait flow needs. The others display rather than collect.
            if ("chat-interview".equals(entry.optString("id"))) {
                return entry;
            }
        }
        return null;
    }

    static JSONObject readTemplate(final String templateId) {
        if (templateId == null || templateId.isBlank()) {
            return null;
        }
        final String body = readClasspath("/web-ui/screen-templates/" + templateId + ".json");
        return body == null ? null : new JSONObject(body);
    }

    private static JSONArray readTemplateIndex() {
        final String body = readClasspath(TEMPLATE_INDEX);
        return body == null ? null : new JSONArray(body);
    }

    private static String readClasspath(final String resource) {
        try (InputStream in = FlowAssistantSetup.class.getResourceAsStream(resource)) {
            return in == null ? null : new String(in.readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException | RuntimeException unreadable) {
            return null;
        }
    }

    private static void collectBindings(
            final Object node, final Set<String> reads, final Set<String> writes) {
        if (node instanceof JSONObject object) {
            for (String key : object.keySet()) {
                final Object value = object.opt(key);
                if (value instanceof String text && !text.isBlank()) {
                    if ("bindVar".equals(key) || "dataVar".equals(key) || "srcVar".equals(key)) {
                        reads.add(text.trim());
                    } else if ("sendsVar".equals(key)) {
                        writes.add(text.trim());
                    }
                }
                collectBindings(value, reads, writes);
            }
        } else if (node instanceof JSONArray array) {
            for (int i = 0; i < array.length(); i++) {
                collectBindings(array.opt(i), reads, writes);
            }
        }
    }

    private static JSONArray declaredCommands(final String className) {
        final JSONArray out = new JSONArray();
        for (var command : WebUiServer.pluginCommandsForClassName(className)) {
            out.put(new JSONObject()
                    .put("name", command.getName() == null ? "" : command.getName())
                    .put("type", command.getType() == null ? "" : command.getType())
                    .put("summary", command.getSummary() == null ? "" : command.getSummary())
                    .put("params", new JSONArray()));
        }
        return out;
    }

    private static JSONArray declaredVariables(final String className, final String direction) {
        final JSONArray out = new JSONArray();
        final JSONObject variables = WebUiServer.pluginVariablesForClassName(className);
        final JSONArray declared = variables == null ? null : variables.optJSONArray(direction);
        for (int i = 0; declared != null && i < declared.length(); i++) {
            final JSONObject entry = declared.optJSONObject(i);
            if (entry == null) {
                continue;
            }
            final String name = entry.optString("var", entry.optString("name", "")).trim();
            if (name.isEmpty()) {
                continue;
            }
            final String declaredDefault = WebUiServer.pluginConfigDefaultForClassName(className, name);
            final String boundTo = declaredDefault == null ? name : declaredDefault.trim();
            final JSONObject reported = new JSONObject()
                    .put("name", name)
                    .put("type", entry.optString("type", ""));
            if (!boundTo.isEmpty()) {
                reported.put("boundTo", boundTo);
            }
            final String description = entry.optString("description", "").trim();
            if (!description.isEmpty()) {
                reported.put("description", description);
            }
            out.put(reported);
        }
        return out;
    }

    // ---------------------------------------------------------------- reading what a project has

    private static List<String> agentNames(final JSONObject capabilities) {
        final List<String> names = new ArrayList<>();
        final JSONObject project = capabilities == null ? null : capabilities.optJSONObject("project");
        final JSONArray agents = project == null ? null : project.optJSONArray("agents");
        for (int i = 0; agents != null && i < agents.length(); i++) {
            final JSONObject agent = agents.optJSONObject(i);
            if (agent != null && !agent.optString("name", "").isBlank()) {
                names.add(agent.optString("name"));
            }
        }
        return names;
    }

    private static boolean usesPlugin(final JSONObject capabilities, final String classMarker) {
        return !pluginNameFor(capabilities, classMarker).isEmpty();
    }

    /** The project's own name for a plugin whose class contains {@code classMarker}, or empty. */
    private static String pluginNameFor(final JSONObject capabilities, final String classMarker) {
        final JSONObject project = capabilities == null ? null : capabilities.optJSONObject("project");
        final JSONArray plugins = project == null ? null : project.optJSONArray("plugins");
        for (int i = 0; plugins != null && i < plugins.length(); i++) {
            final JSONObject plugin = plugins.optJSONObject(i);
            if (plugin == null) {
                continue;
            }
            if (plugin.optString("className", "").toLowerCase(Locale.ROOT).contains(classMarker)) {
                return plugin.optString("name", "");
            }
        }
        return "";
    }

    private static boolean hasScreenThatWrites(final JSONObject capabilities) {
        final JSONObject screens = capabilities == null ? null : capabilities.optJSONObject("screens");
        final JSONArray list = screens == null ? null : screens.optJSONArray("screens");
        for (int i = 0; list != null && i < list.length(); i++) {
            final JSONObject screen = list.optJSONObject(i);
            final JSONArray writes = screen == null ? null : screen.optJSONArray("writesVariables");
            if (writes != null && writes.length() > 0) {
                return true;
            }
        }
        return false;
    }

    private static String classProviding(final Map<String, String> installed, final String marker) {
        for (String className : installed.keySet()) {
            if (className.toLowerCase(Locale.ROOT).contains(marker)) {
                return className;
            }
        }
        return "";
    }

    private static String freeDeviceName(final JSONObject capabilities, final String preferred) {
        final Set<String> taken = new LinkedHashSet<>();
        final JSONObject project = capabilities == null ? null : capabilities.optJSONObject("project");
        final JSONArray plugins = project == null ? null : project.optJSONArray("plugins");
        for (int i = 0; plugins != null && i < plugins.length(); i++) {
            taken.add(plugins.getJSONObject(i).optString("name", ""));
        }
        return freeName(taken, preferred);
    }

    private static String freeAgentName(final JSONObject capabilities, final String preferred) {
        return freeName(new LinkedHashSet<>(agentNames(capabilities)), preferred);
    }

    private static String freeName(final Set<String> taken, final String preferred) {
        if (!taken.contains(preferred)) {
            return preferred;
        }
        for (int i = 2; i < 100; i++) {
            if (!taken.contains(preferred + i)) {
                return preferred + i;
            }
        }
        return preferred;
    }
}
