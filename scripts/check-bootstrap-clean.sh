#!/usr/bin/env bash
# Fails CI when committed generated.rs drifts from a fresh bootstrap.
#
# Runs after toolchain + cargo-expand are installed; assumes the repo
# is a clean checkout. The script is the structural enforcement of
# AW.0.7 — hand-patched slips to `crates/core/src/grammar/generated.rs`
# that bypass `bash scripts/bootstrap-bbnf.sh` become CI-rejected.

set -euo pipefail

# Clear caches so the bootstrap re-expands current grammar inputs.
# The V0 close-out regression ("r#as" token surprise) traced to a
# stale .bbnf-cache surviving across a grammar change; the same
# risk lives in CI runners that reuse caches between jobs.
find . -name .bbnf-cache -exec rm -rf {} + 2>/dev/null || true

GENERATED=crates/core/src/grammar/generated.rs
if [[ ! -f "$GENERATED" ]]; then
    echo "::error::$GENERATED does not exist — cannot compare against fresh bootstrap output."
    exit 1
fi

# Preserve the committed artefact, regen, diff.
cp "$GENERATED" /tmp/generated.committed.rs

bash scripts/bootstrap-bbnf.sh

if ! diff -q /tmp/generated.committed.rs "$GENERATED" > /dev/null; then
    echo "::error::generated.rs diverges from fresh bootstrap output. Re-run \`bash scripts/bootstrap-bbnf.sh\` locally and commit the result."
    diff /tmp/generated.committed.rs "$GENERATED" | head -200
    exit 1
fi

# Sanity floor: the V0 stale-cache regression produced truncated
# 23-line outputs. A fixed 8000-line floor catches that failure mode
# while leaving headroom for AW.W1's planned shrink (28K → ≤12K).
LINES=$(wc -l < "$GENERATED" | tr -d ' ')
if (( LINES < 8000 )); then
    echo "::error::generated.rs has only $LINES lines, which looks truncated. The V0 stale-cache regression produced 23-line outputs; the AW.W1 shrink target is ≤12K. Check bootstrap diagnostic logs and the .bbnf-cache state."
    exit 1
fi

echo "generated.rs clean: $LINES lines match fresh bootstrap."
