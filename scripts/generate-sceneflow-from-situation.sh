#!/usr/bin/env bash
set -euo pipefail

MODE="template"
LLM_BASE_URL=""
LLM_API_KEY=""
LLM_MODEL=""
LLM_TIMEOUT_SEC="30"
LLM_MAX_CANDIDATES="3"
SITUATION="Wait until the user pressed the Okay button"
SNAPSHOT=""
SCENEFLOW=""
OUT=""
REPORT=""

usage() {
  cat <<'EOF'
Usage:
  scripts/generate-sceneflow-from-situation.sh [options]

Core options:
  --mode template|llm|hybrid
  --llm-base-url URL
  --llm-api-key KEY
  --llm-model MODEL_ID
  --llm-timeout-sec SECONDS
  --llm-max-candidates N

Additional options:
  --situation TEXT
  --snapshot PATH
  --sceneflow PATH
  --out PATH
  --report PATH
  -h, --help

Examples:
  scripts/generate-sceneflow-from-situation.sh \
    --mode hybrid \
    --llm-base-url http://localhost:8234/v1/ \
    --llm-model local-model-id \
    --situation "Wait until the user pressed the Okay button"

  scripts/generate-sceneflow-from-situation.sh \
    --mode template \
    --out build/reports/generated.xml \
    --report build/reports/generation-report.json
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --mode)
      MODE="${2:-}"
      shift 2
      ;;
    --llm-base-url)
      LLM_BASE_URL="${2:-}"
      shift 2
      ;;
    --llm-api-key)
      LLM_API_KEY="${2:-}"
      shift 2
      ;;
    --llm-model)
      LLM_MODEL="${2:-}"
      shift 2
      ;;
    --llm-timeout-sec)
      LLM_TIMEOUT_SEC="${2:-}"
      shift 2
      ;;
    --llm-max-candidates)
      LLM_MAX_CANDIDATES="${2:-}"
      shift 2
      ;;
    --situation)
      SITUATION="${2:-}"
      shift 2
      ;;
    --snapshot)
      SNAPSHOT="${2:-}"
      shift 2
      ;;
    --sceneflow)
      SCENEFLOW="${2:-}"
      shift 2
      ;;
    --out)
      OUT="${2:-}"
      shift 2
      ;;
    --report)
      REPORT="${2:-}"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 2
      ;;
  esac
done

if [[ "$MODE" != "template" && "$MODE" != "llm" && "$MODE" != "hybrid" ]]; then
  echo "Invalid --mode '$MODE'. Use template|llm|hybrid." >&2
  exit 2
fi

GRADLE_ARGS=(
  "generateSceneFlowFromSituation"
  "-Pmode=$MODE"
  "-PllmBaseUrl=$LLM_BASE_URL"
  "-PllmApiKey=$LLM_API_KEY"
  "-PllmModel=$LLM_MODEL"
  "-PllmTimeoutSec=$LLM_TIMEOUT_SEC"
  "-PllmMaxCandidates=$LLM_MAX_CANDIDATES"
  "-Psituation=$SITUATION"
)

if [[ -n "$SNAPSHOT" ]]; then
  GRADLE_ARGS+=("-Psnapshot=$SNAPSHOT")
fi
if [[ -n "$SCENEFLOW" ]]; then
  GRADLE_ARGS+=("-Psceneflow=$SCENEFLOW")
fi
if [[ -n "$OUT" ]]; then
  GRADLE_ARGS+=("-Pout=$OUT")
fi
if [[ -n "$REPORT" ]]; then
  GRADLE_ARGS+=("-Preport=$REPORT")
fi

exec ./gradlew "${GRADLE_ARGS[@]}"

