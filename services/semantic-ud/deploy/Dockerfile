# semantic-ud parser image.
#
# Build context is this directory's parent (services/semantic-ud), so that server.py and
# requirements.txt are in scope:
#
#   podman build -f deploy/Dockerfile -t semantic-ud .
#
# In the vsm-server deployment the compose file does that for you — see deploy/README.md §A.
FROM python:3.12-slim

# CPU-only torch, installed first so that stanza's dependency resolution finds it already
# satisfied. Without this pip takes torch from PyPI, which drags in the whole CUDA stack —
# measured on this deployment: cublas 423 MB, cudnn 366 MB, cufft 214 MB, nccl 206 MB,
# cusolver 201 MB, triton 198 MB, cusparselt 170 MB and a dozen more, roughly 2.5 GB of wheels
# for a parser that never touches a GPU. The image and every deploy pay for it.
COPY requirements.txt /app/requirements.txt
RUN pip install --no-cache-dir --index-url https://download.pytorch.org/whl/cpu torch \
 && pip install --no-cache-dir -r /app/requirements.txt

COPY server.py /app/server.py
WORKDIR /app

# Models live on a mounted volume, never in the image. Two reasons: update.sh rebuilds with
# --build on every deploy, and baking 1-2 GB in would either re-download on any Dockerfile
# change or silently serve a stale model; and a volume is the same pattern the deployment
# already uses for port-registry and vsm-assignments.
ENV STANZA_RESOURCES_DIR=/models/stanza_resources \
    SEMANTIC_UD_RESOURCES_DIR=/models/stanza_resources \
    HOME=/models \
    HF_HOME=/models/huggingface \
    SEMANTIC_UD_HOST=0.0.0.0 \
    SEMANTIC_UD_PORT=4061 \
    SEMANTIC_UD_LANG=de \
    SEMANTIC_UD_PRELOAD=de \
    SEMANTIC_UD_PACKAGE=de:combined_german-nlp-electra \
    # Refuse to reach the network in steady state: a missing model is then a loud startup
    # failure instead of an opaque 500 in the middle of an author's analysis. Warming the
    # volume is a deliberate one-off step, documented in deploy/README.md.
    SEMANTIC_UD_AUTO_DOWNLOAD=false \
    HF_HUB_OFFLINE=1

# 0.0.0.0 is safe here and necessary: the only route to this port is the compose network,
# since nothing publishes it to the host. VSM reaches it as http://semantic-ud:4061/analyze.
EXPOSE 4061

# Asserts an electra pipeline is really loaded, so a fallback to Stanza's default parser surfaces
# as unhealthy rather than as quietly worse output.
#
# NOTE: podman ignores this unless the image is built with --format docker; with the default OCI
# format it warns "HEALTHCHECK is not supported for OCI image format and will be ignored", and
# podman-compose gives no way to pass the format. So under the vsm-server deployment there is no
# (healthy) state to read — check it explicitly instead:
#   podman exec vsm-semantic-ud curl -sf http://127.0.0.1:4061/health
# Kept for anyone building with docker/BuildKit, where it does apply.
HEALTHCHECK --interval=30s --timeout=5s --start-period=180s --retries=3 \
    CMD python3 -c "import urllib.request,json,sys; \
d=json.load(urllib.request.urlopen('http://127.0.0.1:4061/health', timeout=4)); \
sys.exit(0 if any('electra' in p for p in d.get('loaded', [])) else 1)"

CMD ["python3", "server.py"]
