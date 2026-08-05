# semantic-ud service

Neural dependency parsing service for Semantic Analysis (`basic` layer: subject/verb/object) using Stanza (UD).

## Run

```bash
cd services/semantic-ud
python3 -m pip install -r requirements.txt
python3 server.py
```

or via Gradle tasks (recommended):

```bash
./gradlew :services:semantic-ud:installRequirements
./gradlew :services:semantic-ud:startService
```

Default endpoint is `http://127.0.0.1:4061`.

`runService` is still available and now includes dependency installation automatically.

## Server-environment configuration

| Variable | Default | Purpose |
|---|---|---|
| `SEMANTIC_UD_HOST` | `127.0.0.1` | Bind address |
| `SEMANTIC_UD_PORT` | `4061` | Port |
| `SEMANTIC_UD_LANG` | `de` | Language assumed when a request omits one |
| `SEMANTIC_UD_PRELOAD` | value of `SEMANTIC_UD_LANG` | Comma-separated languages built **at startup**. Empty disables preloading. |
| `SEMANTIC_UD_RESOURCES_DIR` | — | Stanza model directory (`STANZA_RESOURCES_DIR` also honoured) |
| `SEMANTIC_UD_AUTO_DOWNLOAD` | `true` | When `false`, never reach the network — a missing model is a hard error |

### Offline / pinned-model contract

For CI, corpus runs and any deployment that must not reach the network:

```bash
export SEMANTIC_UD_AUTO_DOWNLOAD=false
export SEMANTIC_UD_RESOURCES_DIR="$HOME/stanza_resources"   # must already contain the models
export SEMANTIC_UD_PRELOAD=de,en
python3 server.py
```

With `AUTO_DOWNLOAD=false`, **a missing model fails at startup with exit code 1** rather than
surfacing as an opaque HTTP 500 in the middle of a batch:

```
[semantic-ud] FATAL: cannot load model for 'xx': ... Language xx is currently unsupported
[semantic-ud] Set SEMANTIC_UD_RESOURCES_DIR to your stanza_resources directory, or allow
              downloads with SEMANTIC_UD_AUTO_DOWNLOAD=true.
```

`GET /health` reports the effective configuration and which pipelines are actually loaded, so an
orchestrator can verify the contract instead of assuming it.

### Concurrency

The server is a `ThreadingHTTPServer`, but Stanza pipelines are **not** safe to call concurrently, so
each language's pipeline is serialised behind its own lock. Consequences:

- `/health` and short requests stay responsive while a long parse runs. (Measured: `/health`
  answered in 17–30 ms, eight times over, during a 9.3 s parse. The former single-threaded
  `HTTPServer` could not even accept the connection.)
- Two different languages parse in parallel; two requests for the *same* language queue.

## API

- `GET /health`
- `POST /analyze` — one sentence
- `POST /analyze/batch` — many sentences in one round trip

Request:

```json
{
  "text": "Hallo $user, wie geht's Dir heute?",
  "language": "de",
  "line": 2,
  "speaker": "Anne",
  "baseOffset": 0
}
```

Response is `semantic-annotations v2` compatible and fills `basic` + provenance.

### Example call

```bash
curl -s -X POST http://127.0.0.1:4061/analyze \
  -H 'Content-Type: application/json' \
  -d '{
    "text": "Hallo $user, wie geht'\''s Dir heute?",
    "language": "de",
    "line": 2,
    "speaker": "Anne",
    "baseOffset": 0
  }' | jq
```

Health:

```bash
curl -s http://127.0.0.1:4061/health | jq
```

### Batch

One round trip for many sentences — what a corpus run over a project should use. Each item is
analysed independently, and a failing item yields an `error` entry instead of failing the batch, so
one unparseable sentence cannot cost a whole run.

```bash
curl -s -X POST http://127.0.0.1:4061/analyze/batch \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "de",
    "sentences": [
      { "text": "Ich gebe dem Kind den roten Ball.", "line": 1, "speaker": "Anne" },
      { "text": "This is English.", "language": "en", "line": 2, "speaker": "Bob" }
    ]
  }' | jq
```

Response: `{ "version": 2, "count": n, "results": [ <same shape as /analyze>, … ] }`. Per-item
`language` overrides the request default.

## Web-UI API exposure

The editor backend exposes UD syntax parsing via project API:

- `POST /api/v1/projects/{pid}/semantic/syntax`

Example:

```bash
curl -s -X POST "http://127.0.0.1:8090/api/v1/projects/<projectId>/semantic/syntax" \
  -H 'Content-Type: application/json' \
  -d '{
    "text": "Hallo $user, wie geht'\''s Dir heute?",
    "language": "de",
    "line": 2,
    "speaker": "Anne",
    "baseOffset": 0,
    "persist": false
  }' | jq
```

## UD role mapping (step 3)

The service maps UD dependencies to S/V/O per language (`de`, `en`) with confidence values:

- `subject`: prefers `nsubj`, `nsubj:pass`, `csubj`
- `verb`: prefers `root` when UPOS is `VERB|AUX`, then verbal fallbacks
- `object`: prefers `obj`, `iobj`, `obl:arg` and complements; German fallback also accepts `obl`/`nmod` with case hints (`Acc|Dat|Gen`)

Each extracted span includes `confidence` and annotation-level provenance (`layers.basic = "ud"`).

## Evaluation harness

Run a small DE/EN gold-set evaluation for S/V/O mapping:

```bash
./gradlew :services:semantic-ud:evaluateMapping
```

Default cases file:

- `references/eval-cases.json`

Direct script invocation:

```bash
cd services/semantic-ud
python3 eval_harness.py --cases references/eval-cases.json
```

If your models are stored in a custom location, set:

```bash
export SEMANTIC_UD_RESOURCES_DIR="/path/to/stanza_resources"
```

The evaluator runs with `SEMANTIC_UD_AUTO_DOWNLOAD=false` to avoid accidental model downloads.
