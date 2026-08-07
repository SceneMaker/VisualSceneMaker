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

The editor auto-starts the service when semantic suggestions are needed — by spawning it as a
subprocess. That works for a desktop or bare-metal VSM and **not** in the container deployment; see
below.

## PENDING: not yet available in the vsm-server deployment

**Status (2026-08-07): the feature is unavailable on the containerised server.** What an author loses
is the safety net when deleting a scene or scene group — the similar-name lookup that warns the
SceneFlow would be corrupted. It is absent rather than broken: nothing misbehaves, the suggestion
simply never appears.

Symptom in the browser console: `Embeddings.Start` returns `NOT_FOUND`, because `WebUiServer` looks
for `services/embeddings/build/libs/*-all.jar` and spawns it as a subprocess, and the vsm-server
image contains neither the shadow jar nor a way to run a second process.

One prerequisite is already done: the client's four calls to `/api/v1/embeddings/*` were
unauthenticated and 401'd under OIDC regardless of whether the sidecar ran (fixed 2026-08-07). The
rest is deployment work, roughly the shape of `services/semantic-ud/deploy/`:

| Piece | Work |
|---|---|
| Proxy target | `WebUiServer.embeddingsBaseUrl()` hardcodes `127.0.0.1:${EMBEDDINGS_PORT:-4050}`. Needs a host or full-URL env var so it can reach a sibling container. |
| `Embeddings.Start` | Must not attempt to spawn a subprocess when the service is externally managed, or the client keeps retrying and logging `NOT_FOUND`. |
| Image | Shadow jar on a JRE base. The existing `vsm-server` Dockerfile runs `gradle jar`, not `shadowJar`, so this needs its own build stage. |
| Model | `model.onnx` is ~470 MB and **not in git** — only `tokenizer.json` (9 MB) is tracked. Needs a warmed named volume, as the parser's models do, or baking into the image. |
| Compose | Service, volume and `depends_on` in `docker-compose.yml`; the container name in `update.sh`'s removal list and in `vsm-stack.service`. Since 2026-08-06 `update.sh` fails the deploy when a compose service is missing from its removal list, so a forgotten entry is loud rather than silent. |
| Memory | ONNX runtime plus the model. Needs a measured `MemoryMax`, not a guessed one. |

Worth settling before starting: whether a second ~470 MB model volume is acceptable on that host, and
whether the delete-time safety net justifies it — since today its absence is a missing warning, not a
failure.

`services/semantic-ud/deploy/` is the working precedent for all of this, including
`verify-container.sh` as a template for a cold-start check. Every failure in that deployment came
from a clean-state case a development machine hid, so the same check is worth having here before the
first deploy rather than after.
