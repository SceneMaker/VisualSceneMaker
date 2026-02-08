package de.dfki.vsm.services.embeddings;

import ai.djl.huggingface.tokenizers.Encoding;
import ai.djl.huggingface.tokenizers.HuggingFaceTokenizer;
import ai.onnxruntime.OnnxTensor;
import ai.onnxruntime.OrtEnvironment;
import ai.onnxruntime.OrtException;
import ai.onnxruntime.NodeInfo;
import ai.onnxruntime.OrtSession;
import ai.onnxruntime.OrtSession.Result;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

final class EmbeddingModel implements AutoCloseable {

    private final OrtEnvironment env;
    private final OrtSession session;
    private final HuggingFaceTokenizer tokenizer;

    EmbeddingModel(Path modelDir) throws Exception {
        Path modelFile = modelDir.resolve("model.onnx");
        Path tokenizerFile = modelDir.resolve("tokenizer.json");
        if (!Files.exists(modelFile)) {
            throw new IllegalStateException("Missing model.onnx at " + modelFile);
        }
        if (!Files.exists(tokenizerFile)) {
            throw new IllegalStateException("Missing tokenizer.json at " + tokenizerFile);
        }
        env = OrtEnvironment.getEnvironment();
        session = env.createSession(modelFile.toString(), new OrtSession.SessionOptions());
        tokenizer = HuggingFaceTokenizer.newInstance(tokenizerFile);
    }

    float[] embed(String text) throws OrtException {
        Encoding encoding = tokenizer.encode(text);
        long[] ids = encoding.getIds();
        long[] attention = encoding.getAttentionMask();
        long[] typeIds = encoding.getTypeIds();

        long[][] inputIds = new long[][]{ids};
        long[][] attentionMask = new long[][]{attention};
        long[][] tokenTypeIds = new long[][]{typeIds};

        Map<String, OnnxTensor> inputs = new HashMap<>();
        for (Map.Entry<String, NodeInfo> entry : session.getInputInfo().entrySet()) {
            String name = entry.getKey();
            if ("input_ids".equals(name)) {
                inputs.put(name, OnnxTensor.createTensor(env, inputIds));
            } else if ("attention_mask".equals(name)) {
                inputs.put(name, OnnxTensor.createTensor(env, attentionMask));
            } else if ("token_type_ids".equals(name)) {
                inputs.put(name, OnnxTensor.createTensor(env, tokenTypeIds));
            }
        }

        try (Result result = session.run(inputs)) {
            float[][][] output = (float[][][]) result.get(0).getValue();
            return meanPool(output[0], attention);
        } finally {
            for (OnnxTensor tensor : inputs.values()) {
                tensor.close();
            }
        }
    }

    private float[] meanPool(float[][] tokenEmbeddings, long[] attentionMask) {
        int hidden = tokenEmbeddings.length > 0 ? tokenEmbeddings[0].length : 0;
        float[] sum = new float[hidden];
        float count = 0f;
        for (int i = 0; i < tokenEmbeddings.length; i++) {
            if (i < attentionMask.length && attentionMask[i] == 0) {
                continue;
            }
            float[] token = tokenEmbeddings[i];
            for (int j = 0; j < hidden; j++) {
                sum[j] += token[j];
            }
            count += 1f;
        }
        if (count == 0f) return sum;
        for (int j = 0; j < hidden; j++) {
            sum[j] /= count;
        }
        normalize(sum);
        return sum;
    }

    private void normalize(float[] vector) {
        double norm = 0.0;
        for (float v : vector) {
            norm += v * v;
        }
        norm = Math.sqrt(norm);
        if (norm <= 0.0) return;
        for (int i = 0; i < vector.length; i++) {
            vector[i] = (float) (vector[i] / norm);
        }
    }

    @Override
    public void close() throws Exception {
        session.close();
        env.close();
    }
}
