#!/bin/sh
# Downloads the parser's models into the compose volume. Run ONCE, before the parser can start.
#
#   ./warm-models.sh                      # from vsm-server-git, after a build
#   ./warm-models.sh my-project my-image  # if compose derived different names
#
# Why this is a separate step rather than something the container does on first boot: the image
# refuses to reach the network (SEMANTIC_UD_AUTO_DOWNLOAD=false, HF_HUB_OFFLINE=1) so that a missing
# model is a loud startup failure instead of an opaque HTTP 500 in the middle of an author's
# analysis. That guarantee is worth one manual step. Downloading on demand would also mean the first
# author to hit Analyze waits several minutes with no explanation.
#
# What it fetches — two separate things, which is why `stanza.download()` alone is not enough:
#   1. the Stanza German model (combined_german-nlp-electra)
#   2. the transformer encoder from HuggingFace (german-nlp-group/electra-base-german-uncased)
# Building the pipeline once, which is what the service does at startup, gets both.
set -eu

# Optional HuggingFace token. Only affects rate limits and download speed — the encoder is public,
# so an anonymous download works, it is just slower and can be throttled. Prompted for rather than
# required, and never echoed or written anywhere.
if [ -z "${HF_TOKEN:-}" ] && [ -t 0 ]; then
    printf 'HuggingFace token (optional, press Enter to skip): '
    stty -echo 2>/dev/null || true
    read -r HF_TOKEN || true
    stty echo 2>/dev/null || true
    printf '\n'
    export HF_TOKEN
fi
if [ -n "${HF_TOKEN:-}" ]; then
    HF_ARGS="-e HF_TOKEN=${HF_TOKEN}"
    echo "==> Using the supplied HuggingFace token."
else
    HF_ARGS=""
    echo "==> No HuggingFace token; anonymous download (slower, subject to rate limits)."
fi

PROJECT="${1:-vsm-server}"
IMAGE="${2:-localhost/${PROJECT}_semantic-ud}"
VOLUME="${PROJECT}_semantic-ud-models"

if ! podman image exists "$IMAGE"; then
    echo "ERROR: image '$IMAGE' not found. Build it first (./update.sh), or pass the name:" >&2
    echo "  podman images | grep semantic" >&2
    echo "  $0 $PROJECT <image-name>" >&2
    exit 1
fi

podman volume exists "$VOLUME" || podman volume create "$VOLUME" >/dev/null
echo "==> Warming $VOLUME from $IMAGE (1-2 GB, a few minutes) ..."

# Runs the service with downloads enabled, waits for it to report a loaded pipeline, then exits.
# Not `podman run ... server.py` interactively, because knowing when it is finished then means
# reading the log and guessing.
# shellcheck disable=SC2086  # HF_ARGS is deliberately word-split: empty must add no argument.
podman run --rm \
    -v "${VOLUME}:/models" \
    -e SEMANTIC_UD_AUTO_DOWNLOAD=true \
    -e HF_HUB_OFFLINE=0 \
    $HF_ARGS \
    --entrypoint sh \
    "$IMAGE" -c '
        python3 server.py &
        pid=$!
        i=0
        while [ $i -lt 60 ]; do
            sleep 10
            i=$((i + 1))
            # The service exits non-zero when it cannot load a model. Without this the loop kept
            # printing "still building" for another two minutes after a FATAL had already scrolled
            # past, which buried the actual error.
            if ! kill -0 $pid 2>/dev/null; then
                echo "ERROR: the service exited. The real cause is in the output above -" >&2
                echo "       look for a line starting with [semantic-ud] FATAL." >&2
                exit 1
            fi
            loaded=$(python3 - <<PY 2>/dev/null || true
import json, urllib.request
try:
    d = json.load(urllib.request.urlopen("http://127.0.0.1:4061/health", timeout=4))
    print(",".join(d.get("loaded") or []))
except Exception:
    print("")
PY
)
            case "$loaded" in
                *electra*)
                    echo "    loaded: $loaded"
                    kill $pid 2>/dev/null || true
                    exit 0
                    ;;
            esac
            echo "    still building (${i}0s) ..."
        done
        echo "ERROR: gave up after 10 minutes. Check the log above for a download failure." >&2
        kill $pid 2>/dev/null || true
        exit 1
    '

echo "==> Done. Verifying the volume is usable with downloads OFF (what the container will do) ..."
# The real test: start exactly as the deployed container does. If this fails, the volume is
# incomplete and the parser would crash-loop after the next deploy.
podman run --rm -v "${VOLUME}:/models" --entrypoint sh "$IMAGE" -c '
    timeout 240 python3 server.py & pid=$!
    sleep 60
    python3 - <<PY
import json, sys, urllib.request
try:
    d = json.load(urllib.request.urlopen("http://127.0.0.1:4061/health", timeout=4))
except Exception as exc:
    print("  FAIL: /health unreachable:", exc); sys.exit(1)
loaded = d.get("loaded") or []
print("  loaded:", loaded)
sys.exit(0 if any("electra" in p for p in loaded) else 1)
PY
    rc=$?
    kill $pid 2>/dev/null || true
    exit $rc
'

echo "==> Volume is ready. Restart the parser:  podman restart vsm-semantic-ud"
