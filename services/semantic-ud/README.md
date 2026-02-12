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

## API

- `GET /health`
- `POST /analyze`

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
