#!/usr/bin/env bash
set -euo pipefail

SSH_HOST="mbabb@mbabb.fridayinstitute.net"
SSH_PORT=1022
REMOTE_DIR="/var/www/grammar"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LOCAL_DIR="${REPO_ROOT}/playground/dist"

# Build WASM (ensures playground bundles latest Rust changes)
echo "Building WASM module..."
(cd "${REPO_ROOT}/wasm" && wasm-pack build --target web --out-dir ../playground/src/wasm)

# Build playground
echo "Building playground..."
(cd "${REPO_ROOT}/playground" && npm run build)

# Deploy
echo "Deploying to ${SSH_HOST}:${REMOTE_DIR}..."
rsync -avz --delete -e "ssh -p ${SSH_PORT}" "${LOCAL_DIR}/" "${SSH_HOST}:${REMOTE_DIR}/"

echo "Done — https://grammar.babb.dev"
