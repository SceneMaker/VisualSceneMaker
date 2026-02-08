package de.dfki.vsm.services.embeddings;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

final class SimilarityService {

    private static final Logger LOG = LoggerFactory.getLogger(SimilarityService.class);

    static String getModelName() {
        return ModelRegistry.isReady() ? "paraphrase-multilingual-MiniLM-L12-v2" : "lexical";
    }

    static JSONArray rankSimilar(String removed, List<String> candidates, int topN) {
        EmbeddingModel model = ModelRegistry.getModel();
        if (model == null) {
            return rankLexical(removed, candidates, topN);
        }
        try {
            float[] target = model.embed(removed);
            List<JSONObject> scored = new ArrayList<>();
            java.util.Map<String, float[]> cache = new java.util.HashMap<>();
            for (String candidate : candidates) {
                if (!cache.containsKey(candidate)) {
                    cache.put(candidate, model.embed(candidate));
                }
                float[] vec = cache.get(candidate);
                double score = cosine(target, vec);
                JSONObject entry = new JSONObject();
                entry.put("name", candidate);
                entry.put("score", score);
                scored.add(entry);
            }
            scored.sort(Comparator.comparingDouble(o -> -o.optDouble("score", 0)));
            JSONArray out = new JSONArray();
            int limit = Math.min(topN, scored.size());
            for (int i = 0; i < limit; i++) {
                out.put(scored.get(i));
            }
            LOG.info("[embeddings] semantic results for '{}': {}", removed, scored);
            System.out.println("[embeddings] semantic results for '" + removed + "': " + scored);
            return out;
        } catch (Exception exc) {
            return rankLexical(removed, candidates, topN);
        }
    }

    private static JSONArray rankLexical(String removed, List<String> candidates, int topN) {
        List<JSONObject> scored = new ArrayList<>();
        for (String candidate : candidates) {
            double score = lexicalScore(removed, candidate);
            JSONObject entry = new JSONObject();
            entry.put("name", candidate);
            entry.put("score", score);
            scored.add(entry);
        }
        scored.sort(Comparator.comparingDouble(o -> -o.optDouble("score", 0)));
        JSONArray out = new JSONArray();
        int limit = Math.min(topN, scored.size());
        for (int i = 0; i < limit; i++) {
            out.put(scored.get(i));
        }
        return out;
    }

    private static double lexicalScore(String a, String b) {
        String s = a == null ? "" : a.trim().toLowerCase();
        String t = b == null ? "" : b.trim().toLowerCase();
        int max = Math.max(s.length(), t.length());
        if (max == 0) return 1.0;
        int dist = levenshteinDistance(s, t);
        return Math.max(0.0, 1.0 - ((double) dist / (double) max));
    }

    private static int levenshteinDistance(String s, String t) {
        int m = s.length();
        int n = t.length();
        if (m == 0) return n;
        if (n == 0) return m;
        int[][] dp = new int[m + 1][n + 1];
        for (int i = 0; i <= m; i++) dp[i][0] = i;
        for (int j = 0; j <= n; j++) dp[0][j] = j;
        for (int i = 1; i <= m; i++) {
            for (int j = 1; j <= n; j++) {
                int cost = s.charAt(i - 1) == t.charAt(j - 1) ? 0 : 1;
                dp[i][j] = Math.min(
                        Math.min(dp[i - 1][j] + 1, dp[i][j - 1] + 1),
                        dp[i - 1][j - 1] + cost
                );
            }
        }
        return dp[m][n];
    }

    private static double cosine(float[] a, float[] b) {
        if (a == null || b == null || a.length == 0 || a.length != b.length) return 0.0;
        double dot = 0.0;
        for (int i = 0; i < a.length; i++) {
            dot += a[i] * b[i];
        }
        return dot;
    }

    private SimilarityService() {}
}
