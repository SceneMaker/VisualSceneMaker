package de.dfki.vsm.services.embeddings;

import io.javalin.Javalin;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

public final class EmbeddingsService {

    public static void main(String[] args) {
        int port = readPort();
        String host = System.getProperty("embeddings.host", "127.0.0.1");

        Javalin app = Javalin.create(config -> {
            config.showJavalinBanner = false;
            config.defaultContentType = "application/json";
            config.enableCorsForAllOrigins();
        });

        app.get("/health", ctx -> {
            JSONObject resp = new JSONObject();
            resp.put("status", "ok");
            resp.put("model", SimilarityService.getModelName());
            resp.put("ready", ModelRegistry.isReady());
            resp.put("modelPath", ModelRegistry.getModelPath());
            resp.put("error", ModelRegistry.getLoadError());
            ctx.result(resp.toString());
        });

        app.post("/similarity", ctx -> {
            JSONObject body = new JSONObject(ctx.body());
            String removed = body.optString("query", "").trim();
            if (removed.isBlank()) {
                removed = body.optString("removed", "").trim();
            }
            JSONArray candidatesJson = body.optJSONArray("candidates");
            int topN = Math.max(1, body.optInt("topN", 3));
            if (removed.isBlank() || candidatesJson == null) {
                ctx.status(400).result(new JSONObject()
                        .put("error", "removed and candidates are required")
                        .toString());
                return;
            }
            List<String> candidates = new ArrayList<>();
            for (int i = 0; i < candidatesJson.length(); i++) {
                String entry = candidatesJson.optString(i, "").trim();
                if (!entry.isEmpty()) {
                    candidates.add(entry);
                }
            }
            JSONArray results = SimilarityService.rankSimilar(removed, candidates, topN);
            JSONObject resp = new JSONObject();
            resp.put("model", SimilarityService.getModelName());
            resp.put("results", results);
            ctx.result(resp.toString());
        });

        app.post("/embed", ctx -> {
            JSONObject body = new JSONObject(ctx.body());
            JSONArray textsJson = body.optJSONArray("texts");
            if (textsJson == null || textsJson.length() == 0) {
                ctx.status(400).result(new JSONObject()
                        .put("error", "texts are required")
                        .toString());
                return;
            }
            EmbeddingModel model = ModelRegistry.getModel();
            if (model == null) {
                ctx.status(503).result(new JSONObject()
                        .put("error", "model not ready")
                        .put("detail", ModelRegistry.getLoadError())
                        .toString());
                return;
            }
            JSONArray vectors = new JSONArray();
            for (int i = 0; i < textsJson.length(); i++) {
                String text = textsJson.optString(i, "");
                float[] vec = model.embed(text);
                JSONArray arr = new JSONArray();
                for (float v : vec) {
                    arr.put(v);
                }
                vectors.put(arr);
            }
            JSONObject resp = new JSONObject();
            resp.put("model", SimilarityService.getModelName());
            resp.put("vectors", vectors);
            ctx.result(resp.toString());
        });

        app.start(host, port);
    }

    private static int readPort() {
        String env = System.getenv("EMBEDDINGS_PORT");
        if (env != null && !env.isBlank()) {
            try {
                return Integer.parseInt(env.trim());
            } catch (NumberFormatException ignored) {
            }
        }
        String prop = System.getProperty("embeddings.port", "4050");
        try {
            return Integer.parseInt(prop.trim());
        } catch (NumberFormatException ignored) {
            return 4050;
        }
    }

    private EmbeddingsService() {}
}
