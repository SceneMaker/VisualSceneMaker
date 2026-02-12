package de.dfki.vsm.services.semantic;

import io.javalin.Javalin;
import org.json.JSONArray;
import org.json.JSONObject;

import java.time.Instant;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class SemanticAnalysisService {

    private static final Pattern TOKEN_PATTERN = Pattern.compile("[A-Za-z][A-Za-z0-9_'-]*");

    public static void main(String[] args) {
        int port = readPort();
        String host = System.getProperty("semantic.host", "127.0.0.1");

        Javalin app = Javalin.create(config -> {
            config.showJavalinBanner = false;
            config.defaultContentType = "application/json";
            config.enableCorsForAllOrigins();
        });

        app.get("/health", ctx -> ctx.result(new JSONObject()
            .put("status", "ok")
            .put("service", "semantic-analysis")
            .toString()));

        app.post("/analyze", ctx -> {
            JSONObject body = new JSONObject(ctx.body());
            String text = body.optString("text", "");
            JSONObject response = new JSONObject();
            response.put("version", 1);
            response.put("generatedAt", Instant.now().toString());
            response.put("annotations", analyze(text));
            ctx.result(response.toString());
        });

        app.start(host, port);
    }

    private static JSONArray analyze(String text) {
        JSONArray annotations = new JSONArray();
        if (text == null || text.isBlank()) return annotations;
        String[] lines = text.split("\\n", -1);
        int cursor = 0;
        for (int i = 0; i < lines.length; i += 1) {
            String line = lines[i];
            int colon = line.indexOf(':');
            if (colon <= 0 || colon >= line.length() - 1) {
                cursor += line.length() + 1;
                continue;
            }
            String speaker = line.substring(0, colon).trim();
            String utteranceRaw = line.substring(colon + 1);
            String utterance = utteranceRaw.trim();
            if (speaker.isEmpty() || utterance.isEmpty()) {
                cursor += line.length() + 1;
                continue;
            }
            int leadingSpaces = utteranceRaw.indexOf(utterance);
            int utteranceStart = cursor + colon + 1 + Math.max(0, leadingSpaces);
            JSONArray tokens = extractTokens(utterance, utteranceStart);
            JSONObject ann = new JSONObject();
            ann.put("id", "ann-" + UUID.randomUUID());
            ann.put("line", i + 1);
            ann.put("speaker", speaker);
            ann.put("text", utterance);

            JSONObject basic = new JSONObject();
            if (tokens.length() > 0) basic.put("subject", tokens.getJSONObject(0));
            if (tokens.length() > 1) basic.put("verb", tokens.getJSONObject(1));
            if (tokens.length() > 2) {
                JSONObject obj = tokens.getJSONObject(2);
                obj.put("text", utterance.substring(obj.getInt("from") - utteranceStart));
                basic.put("object", obj);
            }
            ann.put("basic", basic);

            JSONObject da = new JSONObject();
            da.put("label", detectDialogueAct(utterance));
            da.put("confidence", 0.55);
            ann.put("dialogueAct", da);

            JSONObject tr = new JSONObject();
            String theme = tokens.length() > 0 ? tokens.getJSONObject(0).optString("text", "") : "";
            tr.put("theme", theme);
            tr.put("rheme", theme.isEmpty() ? utterance : utterance.replaceFirst("^" + Pattern.quote(theme) + "\\s*", ""));
            tr.put("confidence", 0.5);
            ann.put("themeRheme", tr);

            annotations.put(ann);
            cursor += line.length() + 1;
        }
        return annotations;
    }

    private static JSONArray extractTokens(String text, int absoluteStart) {
        JSONArray out = new JSONArray();
        Matcher matcher = TOKEN_PATTERN.matcher(text);
        while (matcher.find()) {
            JSONObject token = new JSONObject();
            token.put("text", matcher.group());
            token.put("from", absoluteStart + matcher.start());
            token.put("to", absoluteStart + matcher.end());
            out.put(token);
        }
        return out;
    }

    private static String detectDialogueAct(String utterance) {
        String lower = utterance.toLowerCase();
        if (utterance.endsWith("?")) return "question";
        if (lower.startsWith("please ") || lower.startsWith("can you") || lower.startsWith("could you")) return "request";
        if (lower.startsWith("hello") || lower.startsWith("hi")) return "greeting";
        return "inform";
    }

    private static int readPort() {
        String env = System.getenv("SEMANTIC_PORT");
        if (env != null && !env.isBlank()) {
            try {
                return Integer.parseInt(env.trim());
            } catch (NumberFormatException ignored) {
            }
        }
        String prop = System.getProperty("semantic.port", "4060");
        try {
            return Integer.parseInt(prop.trim());
        } catch (NumberFormatException ignored) {
            return 4060;
        }
    }

    private SemanticAnalysisService() {
    }
}
