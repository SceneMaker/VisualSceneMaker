#!/bin/sh
# Builds RuntimeMain's CLI args from environment variables set in docker-compose.yml.
set -e

PORT="${VSM_PORT:-8091}"
PROJECT_PATH="${VSM_PROJECT_PATH:-/app/project}"
AUTOSTART="${VSM_AUTOSTART:-true}"

ARGS="--allow-lan --port=${PORT}"

if [ -n "$VSM_TOKEN" ]; then
    ARGS="$ARGS --token=${VSM_TOKEN}"
fi

if [ -d "$PROJECT_PATH" ]; then
    ARGS="$ARGS --project=${PROJECT_PATH}"
    if [ "$AUTOSTART" = "true" ]; then
        ARGS="$ARGS --autostart"
    fi
else
    echo "WARNING: project path '$PROJECT_PATH' not found — starting with no project loaded." >&2
fi

# shellcheck disable=SC2086
exec java -jar /app/runtime-server.jar $ARGS
