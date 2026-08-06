#!/bin/sh
# Builds the parser image and warms a throwaway volume, from nothing, then asserts the service can
# load offline. Run before deploying a change to server.py, requirements.txt or the Dockerfile.
#
#   ./verify-container.sh              # docker or podman, whichever is present
#   ./verify-container.sh --keep       # leave the image and volume behind for inspection
#
# Why this exists: every deployment failure in this service so far was a clean-state case that a
# development machine hides. transformers was missing from requirements but present locally from
# other work; the electra model was never downloaded by the fallback but was already cached locally;
# a warm-up step silently did nothing because an apostrophe in a comment closed a shell string early.
# None were visible without building the image and starting from an empty volume, which is exactly
# what this does.
#
# Not wired into `check`: it downloads 1-2 GB and builds a torch image, so it belongs in a
# deliberate run, not in every build.
set -eu

cd "$(dirname "$0")/.."   # services/semantic-ud, the build context

KEEP=0
[ "${1:-}" = "--keep" ] && KEEP=1

if command -v podman >/dev/null 2>&1; then
    RT=podman
elif command -v docker >/dev/null 2>&1; then
    RT=docker
else
    echo "ERROR: neither podman nor docker found." >&2
    exit 1
fi
if [ "$RT" = docker ] && ! docker info >/dev/null 2>&1; then
    echo "ERROR: docker is installed but its daemon is not running." >&2
    exit 1
fi

IMAGE=semantic-ud-verify
VOLUME=semantic-ud-verify-models
PKG=combined_german-nlp-electra

cleanup() {
    [ "$KEEP" = 1 ] && { echo "==> Keeping $IMAGE and $VOLUME as asked."; return; }
    echo "==> Cleaning up ..."
    $RT volume rm -f "$VOLUME" >/dev/null 2>&1 || true
    $RT rmi -f "$IMAGE" >/dev/null 2>&1 || true
}
trap cleanup EXIT

echo "==> [1/5] Building the image with $RT (no cache: the point is a clean install) ..."
$RT build --no-cache -f deploy/Dockerfile -t "$IMAGE" . >/tmp/semantic-ud-verify-build.log 2>&1 || {
    echo "FAIL: build failed. Last lines:" >&2
    tail -25 /tmp/semantic-ud-verify-build.log >&2
    exit 1
}

echo "==> [2/5] Checking what got installed ..."
$RT run --rm --entrypoint python3 "$IMAGE" -c '
import sys
import stanza, transformers, torch
print("    stanza      ", stanza.__version__)
print("    transformers", transformers.__version__)
print("    torch       ", torch.__version__, "| cuda:", torch.version.cuda)
# CPU-only torch is a deliberate choice: the CUDA wheels add roughly 2.5 GB for a parser that never
# uses a GPU. If a future dependency change reintroduces them, fail here rather than on a deploy.
if torch.version.cuda is not None:
    print("    FAIL: torch was built for CUDA; expected the CPU-only wheel.", file=sys.stderr)
    sys.exit(1)
' || exit 1

echo "==> [3/5] Warming a fresh volume (1-2 GB; this is the slow part) ..."
$RT volume rm -f "$VOLUME" >/dev/null 2>&1 || true
$RT volume create "$VOLUME" >/dev/null
$RT run --rm -v "${VOLUME}:/models" -e HF_HUB_OFFLINE=0 -e "PKG=${PKG}" \
    --entrypoint python3 "$IMAGE" -c '
import os, pathlib, sys
import stanza
pkg = os.environ["PKG"]
model_dir = "/models/stanza_resources"
pathlib.Path(model_dir).mkdir(parents=True, exist_ok=True)
stanza.download("de", model_dir=model_dir, package="default",
                processors={"pos": pkg, "depparse": pkg}, verbose=False)
want = pathlib.Path(model_dir) / "de" / "pos" / (pkg + ".pt")
if not want.exists():
    print("    FAIL: %s missing after download" % want, file=sys.stderr)
    sys.exit(1)
print("    stanza model: %d MB" % (want.stat().st_size // 1_000_000))
# No download_method=None here: that kwarg makes Stanza ask transformers for local files only, which
# is right when serving and wrong when the whole purpose is to fetch.
stanza.Pipeline(lang="de", dir=model_dir, processors="tokenize,mwt,pos,lemma,depparse",
                package={"pos": pkg, "depparse": pkg}, use_gpu=False, verbose=False)
hf = pathlib.Path(os.environ.get("HF_HOME", "/models/huggingface"))
if not (hf.exists() and list(hf.rglob("*electra*"))):
    print("    FAIL: no encoder under %s" % hf, file=sys.stderr)
    sys.exit(1)
print("    encoder cached: %d MB" % (sum(f.stat().st_size for f in hf.rglob("*") if f.is_file()) // 1_000_000))
' >/tmp/semantic-ud-verify-warm.log 2>&1 || {
    echo "FAIL: warming failed. Last lines:" >&2
    tail -20 /tmp/semantic-ud-verify-warm.log >&2
    exit 1
}
grep -E "stanza model:|encoder cached:" /tmp/semantic-ud-verify-warm.log || true

echo "==> [4/5] Starting the service OFFLINE from that volume, as the deployed container does ..."
# The image already sets SEMANTIC_UD_AUTO_DOWNLOAD=false and HF_HUB_OFFLINE=1, so nothing here needs
# to force offline mode — using the image defaults is precisely the point.
$RT run --rm -v "${VOLUME}:/models" --entrypoint sh "$IMAGE" -c '
python3 server.py & pid=$!
i=0
while [ $i -lt 36 ]; do
    sleep 5; i=$((i + 1))
    if ! kill -0 $pid 2>/dev/null; then echo "    FAIL: the service exited" >&2; exit 1; fi
    python3 - <<PY && exit 0
import json, sys, urllib.request
try:
    d = json.load(urllib.request.urlopen("http://127.0.0.1:4061/health", timeout=3))
except Exception:
    sys.exit(1)
loaded = d.get("loaded") or []
if any("electra" in p for p in loaded):
    print("    health loaded:", loaded)
    sys.exit(0)
sys.exit(1)
PY
done
echo "    FAIL: never became healthy" >&2
exit 1
' || { echo "FAIL: the service could not load the warmed volume offline." >&2; exit 1; }

echo "==> [5/5] Parsing one sentence, to prove the pipeline actually answers ..."
$RT run --rm -v "${VOLUME}:/models" --entrypoint sh "$IMAGE" -c '
python3 server.py & pid=$!
i=0; while [ $i -lt 36 ]; do sleep 5; i=$((i+1))
  python3 -c "import urllib.request;urllib.request.urlopen(\"http://127.0.0.1:4061/health\",timeout=3)" 2>/dev/null && break
done
python3 - <<PY
import json, sys, urllib.request
body = json.dumps({"text": "Super gemacht!", "language": "de",
                   "layers": {"basic": True}}).encode()
req = urllib.request.Request("http://127.0.0.1:4061/analyze", data=body,
                             headers={"Content-Type": "application/json"})
d = json.load(urllib.request.urlopen(req, timeout=120))
ann = (d.get("annotations") or [{}])[0]
pkg = (d.get("provenance") or {}).get("package")
mods = [m.get("text") for m in ((ann.get("basic") or {}).get("verbModifiers") or [])]
print("    package:", pkg, "| verb:", ((ann.get("basic") or {}).get("verb") or {}).get("text"),
      "| verbModifiers:", mods)
ok = pkg and "electra" in pkg and mods == ["Super"]
sys.exit(0 if ok else 1)
PY
rc=$?; kill $pid 2>/dev/null || true; exit $rc
' || { echo "FAIL: the parse did not return the expected analysis." >&2; exit 1; }

echo ""
echo "PASS: image builds clean, volume warms from empty, service loads offline, and parses."
