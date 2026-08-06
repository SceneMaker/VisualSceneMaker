# Deploying semantic-ud on Ubuntu

> Scope: **only** the UD parser that backs Semantic Analysis. It is not the VSM editor, not
> `runtime-server`, and unrelated to the Docker package in the repository's top-level
> [`deploy/`](../../../deploy/README.md).

VSM's Java server calls this parser over loopback; browsers never do. So it binds `127.0.0.1`, needs
no TLS, no auth and no firewall rule — and **must not be exposed**, because it has none of those.

## Layout, and why

| What | Where | Why |
|---|---|---|
| Code | `/opt/vsm-server/services/semantic-ud` | Ships inside the repo you already deploy. Nothing to create. |
| Virtualenv | `/var/lib/vsm-semantic-ud/venv` | Outside the deploy tree, so `update.sh` cannot delete it and it never appears as untracked files. |
| Models | `/var/lib/vsm-semantic-ud/stanza_resources` + `.../huggingface` | Several hundred MB. Same reason, more urgently. |
| Config | `/etc/default/vsm-semantic-ud` | The only file you edit. The unit reads paths from it. |

`/var/lib` rather than `/opt` because this is mutable application state, not the application.
Anything under `/opt/vsm-server` is at the mercy of whatever the deploy script does to that tree.

## Manual preparation — once, in this order

Everything below is done **before** `./update.sh`, except step 6 which needs the synced repo.

### 1. Packages

Ubuntu 24.04 refuses system-wide `pip install` (PEP 668), so a virtualenv is required, not optional.

```bash
sudo apt update
sudo apt install -y python3 python3-venv
```

### 2. Service user and state directory

```bash
sudo useradd --system --no-create-home --shell /usr/sbin/nologin vsm-semantic
sudo mkdir -p /var/lib/vsm-semantic-ud/{stanza_resources,huggingface}
sudo chown -R vsm-semantic:vsm-semantic /var/lib/vsm-semantic-ud
```

### 3. Virtualenv

```bash
sudo -u vsm-semantic python3 -m venv /var/lib/vsm-semantic-ud/venv
sudo -u vsm-semantic /var/lib/vsm-semantic-ud/venv/bin/pip install --upgrade pip
sudo -u vsm-semantic /var/lib/vsm-semantic-ud/venv/bin/pip install 'stanza>=1.8.0'
```

### 4. Warm the model cache — the step that is easy to miss

Two separate downloads are needed and `stanza.download()` alone does **not** get both: the Stanza
model, and the transformer encoder from HuggingFace
(`german-nlp-group/electra-base-german-uncased`). The reliable way to fetch both is to build the
pipeline once, which is what the service does at startup. So run it by hand with downloads enabled:

```bash
cd /opt/vsm-server/services/semantic-ud   # or wherever the repo is, if you have not synced yet
sudo -u vsm-semantic env \
  HOME=/var/lib/vsm-semantic-ud \
  HF_HOME=/var/lib/vsm-semantic-ud/huggingface \
  STANZA_RESOURCES_DIR=/var/lib/vsm-semantic-ud/stanza_resources \
  SEMANTIC_UD_AUTO_DOWNLOAD=true \
  SEMANTIC_UD_PACKAGE=de:combined_german-nlp-electra \
  SEMANTIC_UD_PRELOAD=de \
  /var/lib/vsm-semantic-ud/venv/bin/python3 server.py
```

Expect 1–2 GB and a few minutes. When it prints `listening on http://127.0.0.1:4061`, check what it
actually loaded, then stop it with Ctrl-C:

```bash
curl -s http://127.0.0.1:4061/health
```

`loaded` must contain `de:combined_german-nlp-electra`. If it says something else, the encoder did
not arrive: **the service falls back to Stanza's default rather than failing**, so this is the moment
to catch it. Do not proceed until `loaded` is right — a fallback is only visible afterwards in each
document's `provenance.package`.

### 5. Install the unit and its config

```bash
sudo cp deploy/vsm-semantic-ud.default /etc/default/vsm-semantic-ud
sudo cp deploy/semantic-ud.service /etc/systemd/system/semantic-ud.service
sudo systemctl daemon-reload
sudo systemctl enable --now semantic-ud
```

Verify it came up under systemd, not just by hand:

```bash
systemctl status semantic-ud --no-pager
curl -s http://127.0.0.1:4061/health
journalctl -u semantic-ud -n 30 --no-pager
```

The config file ships with `SEMANTIC_UD_AUTO_DOWNLOAD=false` and `HF_HUB_OFFLINE=1`, which is the
steady state: a missing model now fails at startup with exit 1 instead of surfacing as an opaque 500
in the middle of an author's analysis.

### 6. VSM side

If VSM runs on the same host, **nothing to configure** — `http://127.0.0.1:4061/analyze` is the
default. Otherwise set, in order of precedence:

1. `SemanticServices/udUrl` in the project's `project.xml`
2. `-Dsemantic.ud.url=…` on the VSM JVM

Two settings are per project and do not travel with a deployment, so check them on the server's
projects rather than assuming your local values apply:

- `SemanticServices/udTimeoutMs` — the default assumes a warm local parser.
- The LLM timeout, if you use the DA/TR layer. That layer needs an LLM reachable **from the server**,
  and it is where the latency is: 3–27 s measured against a local Qwen3-30B.

### 7. Now run `./update.sh`

Nothing in steps 1–5 lives inside the deploy tree, so the update cannot disturb it. Only step 4's
working directory referenced the tree, and only to read `server.py`.

After the update, confirm the service still answers — a repo sync can change `server.py`:

```bash
sudo systemctl restart semantic-ud
curl -s http://127.0.0.1:4061/health
```

## Checking it end to end

Run a semantic analysis from the web UI and confirm the parser that actually ran:

```bash
curl -s -X POST "http://127.0.0.1:8090/api/v1/projects/<pid>/semantic/analyze-script" \
  -H 'Content-Type: application/json' \
  -d '{"layers":{"basic":true},"useLlm":false,"persist":false,"language":"de"}' \
  | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d.get("provenance",{}).get("package"), "| warnings:", len(d.get("warnings") or []))'
```

Expected: `combined_german-nlp-electra | warnings: 0`. If the service is down you get one
`UD analysis unavailable` warning per sentence, which the editor's Semantic Analysis panel now
collapses into a single actionable message.

## Notes

- **Concurrency.** Stanza pipelines are not thread-safe, so each language is serialised behind a
  lock. Two authors analysing German at the same time queue; `/health` stays responsive.
- **Memory.** `MemoryMax=4G` in the unit. The transformer is the bulk of it; lower only if you have
  measured your own footprint via `systemctl status`.
- **Logs.** `journalctl -u semantic-ud -f`. The service logs the effective package and warns loudly
  when it falls back.
