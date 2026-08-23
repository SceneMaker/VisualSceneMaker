package de.dfki.vsm.xtension.llm;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.util.llm.LLMSupport;

import java.time.Duration;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

/**
 * LLM plugin — wraps LLMSupport to expose a `send` SceneFlow action.
 *
 * Action: send(prompt='...', system='...', responseVar='varName')
 *
 * {varName} placeholders in prompt/system are replaced with live SceneFlow
 * variable values at call time. The LLM call is async; poll the responseVar
 * with a CEdge loop (see IntakeInterview tutorial project for example).
 */
public class LlmExecutor extends ActivityExecutor {

    private LLMSupport mLlm;
    private ExecutorService mExecutor;

    public LlmExecutor(final PluginConfig config, final RunTimeProject project) {
        super(config, project);
    }

    @Override
    public String marker(final long id) {
        return "$(" + id + ")";
    }

    @Override
    public void launch() {
        String rawUrl     = configOrDefault("base_url",    "https://api.openai.com/v1/");
        String baseUrl    = LLMSupport.normalizeBaseUrl(rawUrl, null);
        String apiKey     = configOrDefault("api_key",     "");
        String model      = configOrDefault("model",       "");
        double temp       = parseDoubleOrDefault(configOrDefault("temperature", "0.7"), 0.7);
        int    timeoutSec = parseIntOrDefault(configOrDefault("timeout_sec",    "30"),  30);
        boolean disableThinking = parseBooleanOrDefault(configOrDefault("disable_thinking", "true"), true);
        String  reasoningEffort = configOrDefault("reasoning_effort", "low");

        mLlm = new LLMSupport(baseUrl, apiKey.isBlank() ? null : apiKey, Duration.ofSeconds(timeoutSec));
        mLlm.setDefaultTemperature(temp);
        mLlm.setDefaultDisableThinking(disableThinking);
        mLlm.setDefaultReasoningEffort(reasoningEffort.isBlank() || "none".equalsIgnoreCase(reasoningEffort) ? null : reasoningEffort);

        mExecutor = Executors.newCachedThreadPool(r -> {
            Thread t = new Thread(r, "llm-executor");
            t.setDaemon(true);
            return t;
        });

        // Discover available models; if model is empty/auto, pick the first one.
        String resolvedModel = resolveModel(model, baseUrl);
        mLlm.setSelectedModel(resolvedModel.isBlank() ? "gpt-4o-mini" : resolvedModel);
        mLogger.message("[llm] launched, model=" + mLlm.getSelectedModel().id() + ", base_url=" + baseUrl
                + ", disable_thinking=" + disableThinking + ", reasoning_effort=" + reasoningEffort);
    }

    private String resolveModel(final String configured, final String baseUrl) {
        boolean autoDetect = configured.isBlank() || "auto".equalsIgnoreCase(configured.trim());
        // A specific model id needs no model-list round trip — skipping it here matters because
        // launch() runs synchronously on RunTimeProject's plugin-launch loop (blocking the
        // WS command thread that handles Runtime.Play): confirmed 2026-08-22 that with an
        // explicit model id and an unreachable base_url, this fetch was the entire multi-second
        // delay before the SceneFlow interpreter even got created.
        if (!autoDetect) {
            return configured.trim();
        }
        try {
            List<LLMSupport.LLMModel> available = mLlm.fetchAvailableModels();
            String ids = available.stream().map(LLMSupport.LLMModel::id).collect(Collectors.joining(", "));
            mLogger.message("[llm] available models at " + baseUrl + ": " + ids);
            if (!available.isEmpty()) {
                String picked = available.get(0).id();
                mLogger.message("[llm] auto-selected model: " + picked);
                return picked;
            }
        } catch (Exception e) {
            mLogger.warning("[llm] could not fetch models from " + baseUrl + ": " + e.getMessage());
        }
        return "gpt-4o-mini";
    }

    @Override
    public void unload() {
        if (mExecutor != null) mExecutor.shutdownNow();
        mLogger.message("[llm] unloaded");
    }

    @Override
    public void execute(final AbstractActivity activity) {
        // SpeechActivity arrives when the SceneTurn has embedded ActionObjects.
        // Fire the time-markers immediately so the registered ActionActivities execute.
        if (activity instanceof SpeechActivity) {
            // Fire embedded ActionObject markers immediately (no TTS in LLM executor).
            SpeechActivity sa = (SpeechActivity) activity;
            for (String tm : sa.getTimeMarks("$(")) {
                mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
            }
            return;
        }
        final String name = activity.getName() == null ? "" : activity.getName().trim();
        if (!name.equalsIgnoreCase("send") && !name.equalsIgnoreCase("ask")) {
            mLogger.warning("[llm] unknown action: " + name);
            return;
        }

        final String rawPrompt   = getActionFeatureValue("prompt",      activity.getFeatures());
        final String rawSystem   = getActionFeatureValue("system",      activity.getFeatures());
        final String rawRespVar  = getActionFeatureValue("responseVar", activity.getFeatures());

        if (rawPrompt == null || rawPrompt.isBlank()) {
            mLogger.warning("[llm] 'send' requires a 'prompt' parameter");
            return;
        }

        final String targetVar = (rawRespVar != null && !rawRespVar.isBlank())
                ? stripQuotes(rawRespVar) : "llm_response";

        mLogger.message("[llm] queuing send → responseVar='" + targetVar + "'");
        mExecutor.execute(() -> {
            try {
                String prompt = resolveVariables(stripQuotes(rawPrompt));
                String system = rawSystem != null ? resolveVariables(stripQuotes(rawSystem)) : null;
                mLogger.message("[llm] calling model=" + mLlm.getSelectedModel().id()
                        + " system=" + (system != null && !system.isBlank() ? "yes" : "no")
                        + " promptLen=" + prompt.length());

                LLMSupport.LLMPrompt llmPrompt;
                if (system != null && !system.isBlank()) {
                    llmPrompt = LLMSupport.LLMPrompt.builder()
                            .addSystemMessage(system)
                            .addUserMessage(prompt)
                            .build();
                } else {
                    llmPrompt = LLMSupport.LLMPrompt.of(prompt);
                }

                LLMSupport.LLMCompletion result = mLlm.sendPrompt(llmPrompt);
                String content = result.content();
                mProject.setVariable(targetVar, content);
                mLogger.message("[llm] stored " + result.usage().totalTokens()
                        + " tokens → '" + targetVar + "': " + content.substring(0, Math.min(80, content.length())) + "…");
            } catch (Exception ex) {
                mLogger.failure("[llm] send failed: " + ex.getMessage());
                mProject.setVariable(targetVar, "");
            }
        });
    }

    /** Replaces {varName} in text with the current SceneFlow variable value. */
    private String resolveVariables(final String text) {
        if (text == null || !text.contains("{")) return text;
        StringBuilder sb = new StringBuilder();
        int pos = 0;
        while (pos < text.length()) {
            int open  = text.indexOf('{', pos);
            if (open < 0) { sb.append(text, pos, text.length()); break; }
            int close = text.indexOf('}', open + 1);
            if (close < 0) { sb.append(text, pos, text.length()); break; }
            sb.append(text, pos, open);
            String varName = text.substring(open + 1, close);
            sb.append(readVar(varName));
            pos = close + 1;
        }
        return sb.toString();
    }

    private String readVar(final String varName) {
        try {
            AbstractValue val = mProject.getValueOf(varName);
            return val != null ? String.valueOf(val.getValue()) : "";
        } catch (Exception ex) {
            return "";
        }
    }

    private String configOrDefault(final String key, final String fallback) {
        String v = mConfig.getProperty(key);
        return (v == null || v.isBlank()) ? fallback : v;
    }

    private static String stripQuotes(final String raw) {
        if (raw == null) return "";
        String s = raw.trim();
        if (s.length() >= 2) {
            char f = s.charAt(0), l = s.charAt(s.length() - 1);
            if ((f == '\'' && l == '\'') || (f == '"' && l == '"')) s = s.substring(1, s.length() - 1);
        }
        return s;
    }

    private int parseIntOrDefault(final String v, final int def) {
        try { return Integer.parseInt(v.trim()); } catch (Exception e) { return def; }
    }

    private double parseDoubleOrDefault(final String v, final double def) {
        try { return Double.parseDouble(v.trim()); } catch (Exception e) { return def; }
    }

    private boolean parseBooleanOrDefault(final String v, final boolean def) {
        if (v == null || v.isBlank()) return def;
        return Boolean.parseBoolean(v.trim());
    }
}
