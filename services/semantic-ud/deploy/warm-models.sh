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

# Downloads directly rather than by starting the service, deliberately. The service's own
# auto-download lives in the *image*, so warming through it silently depends on how old the image
# is: run against an image built before a download fix and it fails exactly as it did before, with
# no download attempted. Owning the download here makes warming a property of this script and the
# volume, not of the image, and the verification step below still exercises the image's code — which
# is the right place for that.
PKG="${SEMANTIC_UD_PACKAGE_NAME:-combined_german-nlp-electra}"
LANG_CODE="${SEMANTIC_UD_LANG_CODE:-de}"

# shellcheck disable=SC2086  # HF_ARGS is deliberately word-split: empty must add no argument.
podman run --rm \
    -v "${VOLUME}:/models" \
    -e HF_HUB_OFFLINE=0 \
    -e "PKG=${PKG}" \
    -e "LANG_CODE=${LANG_CODE}" \
    $HF_ARGS \
    --entrypoint python3 \
    "$IMAGE" -c '
import os, pathlib, sys
import stanza

pkg = os.environ["PKG"]
lang = os.environ["LANG_CODE"]
model_dir = "/models/stanza_resources"
pathlib.Path(model_dir).mkdir(parents=True, exist_ok=True)

print(f"    stanza {stanza.__version__}: downloading {lang}/{pkg} ...", flush=True)
# package= for the processors that do not vary by treebank, per-processor dict for the two that do.
stanza.download(lang, model_dir=model_dir, package="default",
                processors={"pos": pkg, "depparse": pkg}, verbose=False)

want = pathlib.Path(model_dir) / lang / "pos" / f"{pkg}.pt"
if not want.exists():
    print(f"    FAILED: {want} still missing after download", file=sys.stderr)
    sys.exit(1)
print(f"    model present: {want} ({want.stat().st_size // 1_000_000} MB)", flush=True)

# Building the pipeline is what pulls the transformer encoder from HuggingFace — a separate
# download from the Stanza model, and the one people miss.
# Deliberately WITHOUT download_method=None. That kwarg is right for serving — it is what makes a
# missing model fail loudly instead of downloading mid-analysis — but Stanza turns it into
# local_files_only=True for transformers, so during warming it caused
# "We couldn't connect to huggingface.co ... and couldn't find them in the cached files" even with
# HuggingFace perfectly reachable. Warming is the one moment downloading is the point.
print("    building the pipeline once, to fetch the encoder ...", flush=True)
stanza.Pipeline(lang=lang, dir=model_dir, processors="tokenize,mwt,pos,lemma,depparse",
                package={"pos": pkg, "depparse": pkg}, use_gpu=False, verbose=False)
print("    encoder cached.", flush=True)
'

echo "==> Verifying with downloads OFF, using the IMAGE's own code (what the container will do) ..."
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
' || {
    echo "" >&2
    echo "ERROR: the volume is warm, but the image cannot use it." >&2
    echo "" >&2
    echo "Most likely the image is older than the source: server.py is baked in at build time, so a" >&2
    echo "fix synced into vsm/ does not reach a container until the image is rebuilt. Rebuild, then" >&2
    echo "re-run this script's verification:" >&2
    echo "    ./update.sh                 # rebuilds everything, or just this service:" >&2
    echo "    podman compose build semantic-ud && podman restart vsm-semantic-ud" >&2
    exit 1
}

echo "==> Volume is ready and the image can load it."
echo "    Restart the parser:  podman restart vsm-semantic-ud"
