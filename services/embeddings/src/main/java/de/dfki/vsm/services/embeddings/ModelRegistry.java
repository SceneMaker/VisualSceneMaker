package de.dfki.vsm.services.embeddings;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.atomic.AtomicReference;

final class ModelRegistry {

    private static final AtomicReference<EmbeddingModel> MODEL = new AtomicReference<>();
    private static final AtomicReference<String> MODEL_PATH = new AtomicReference<>("");
    private static final AtomicReference<String> LOAD_ERROR = new AtomicReference<>("");

    static EmbeddingModel getModel() {
        EmbeddingModel existing = MODEL.get();
        if (existing != null) {
            return existing;
        }
        synchronized (MODEL) {
            if (MODEL.get() != null) return MODEL.get();
            try {
                Path modelDir = resolveModelPath();
                EmbeddingModel model = new EmbeddingModel(modelDir);
                MODEL_PATH.set(modelDir.toString());
                MODEL.set(model);
                return model;
            } catch (Exception exc) {
                LOAD_ERROR.set(exc.getMessage() == null ? exc.toString() : exc.getMessage());
                return null;
            }
        }
    }

    static boolean isReady() {
        return MODEL.get() != null;
    }

    static String getModelPath() {
        return MODEL_PATH.get();
    }

    static String getLoadError() {
        return LOAD_ERROR.get();
    }

    private static Path resolveModelPath() {
        String env = System.getenv("EMBEDDINGS_MODEL_PATH");
        if (env != null && !env.isBlank()) {
            return Paths.get(env.trim());
        }
        String prop = System.getProperty("embeddings.modelPath", "");
        if (!prop.isBlank()) {
            return Paths.get(prop.trim());
        }
        Path cwd = Paths.get(".").toAbsolutePath().normalize();
        Path local = cwd.resolve("services/embeddings/src/main/resources/models/paraphrase-multilingual-MiniLM-L12-v2");
        if (Files.exists(local)) {
            return local;
        }
        return cwd.resolve("models/paraphrase-multilingual-MiniLM-L12-v2");
    }

    private ModelRegistry() {}
}
