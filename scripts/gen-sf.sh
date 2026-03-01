#!/usr/bin/env bash
set -euo pipefail

OUTPUT_MODE="standalone"
MODE="llm"
LLM_BASE_URL="${LLM_BASE_URL:-https://api.openai.com/v1}"
LLM_API_KEY="${LLM_API_KEY:-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}"
LLM_MODEL="${LLM_MODEL:-gpt-5.2}"
LLM_TIMEOUT_SEC="30"
LLM_MAX_CANDIDATES="1"
SITUATION="Wait until the user pressed the Okay button or the Cancel button"
SNAPSHOT=""
SCENEFLOW=""
OUT=""
REPORT=""
OUT_PROJECT_DIR=""

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
  --output-mode patch|standalone
  --situation TEXT
  --snapshot PATH
  --sceneflow PATH
  --out PATH
  --report PATH
  --out-project-dir PATH
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
    --output-mode)
      OUTPUT_MODE="${2:-}"
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
    --out-project-dir)
      OUT_PROJECT_DIR="${2:-}"
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
if [[ "$OUTPUT_MODE" != "patch" && "$OUTPUT_MODE" != "standalone" ]]; then
  echo "Invalid --output-mode '$OUTPUT_MODE'. Use patch|standalone." >&2
  exit 2
fi

# Auto-pick key from env if not passed explicitly.
if [[ -z "$LLM_API_KEY" ]]; then
  if [[ -n "${OPENAI_API_KEY:-}" ]]; then
    LLM_API_KEY="$OPENAI_API_KEY"
  fi
fi

if [[ "$MODE" == "llm" || "$MODE" == "hybrid" ]]; then
  if [[ -z "$LLM_BASE_URL" || -z "$LLM_MODEL" ]]; then
    echo "LLM mode requires --llm-base-url and --llm-model (or env defaults)." >&2
    exit 2
  fi
  if [[ -z "$LLM_API_KEY" ]]; then
    echo "No LLM API key configured. Provide --llm-api-key or set OPENAI_API_KEY/LLM_API_KEY." >&2
    exit 2
  fi
fi

GRADLE_ARGS=(
  "generateSceneFlowFromSituation"
  "-Pmode=$MODE"
  "-PoutputMode=$OUTPUT_MODE"
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
if [[ -n "$OUT_PROJECT_DIR" ]]; then
  GRADLE_ARGS+=("-PoutProjectDir=$OUT_PROJECT_DIR")
fi

exec ./gradlew "${GRADLE_ARGS[@]}"
