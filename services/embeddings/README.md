# Embeddings Service

This service provides semantic similarity for scene name suggestions.

## Model Download

Download the selected medium model (paraphrase-multilingual-MiniLM-L12-v2) with:

```bash
./gradlew :services:embeddings:downloadModel
```

This will place files under:

```
models/paraphrase-multilingual-MiniLM-L12-v2/
  model.onnx
  tokenizer.json
```

You can override the model path via `EMBEDDINGS_MODEL_PATH`.

## Run (manual)

```bash
./gradlew :services:embeddings:shadowJar
java -jar services/embeddings/build/libs/embeddings-all.jar
```

The editor auto-starts the service when semantic suggestions are needed.
