#!/bin/bash
set -euo pipefail
DOCS="$(cd "$(dirname "$0")/.." && pwd)/docs"
REPOS="$(cd "$(dirname "$0")/../.." && pwd)"

# Sync parse-that playground-facing docs
if [ -d "$REPOS/parse-that/docs/playground" ]; then
    rsync -av --delete "$REPOS/parse-that/docs/playground/" "$DOCS/parse-that/"
    echo "Synced parse-that docs"
else
    echo "Skipping parse-that (no docs/playground/ found)"
fi

# Sync pprint playground-facing docs
if [ -d "$REPOS/pprint/docs/playground" ]; then
    rsync -av --delete "$REPOS/pprint/docs/playground/" "$DOCS/pprint/"
    echo "Synced pprint docs"
else
    echo "Skipping pprint (no docs/playground/ found)"
fi

# Sync gorgeous playground-facing docs
if [ -d "$REPOS/gorgeous/docs/playground" ]; then
    rsync -av --delete "$REPOS/gorgeous/docs/playground/" "$DOCS/gorgeous/"
    echo "Synced gorgeous docs"
else
    echo "Skipping gorgeous (no docs/playground/ found)"
fi

echo "Done. Synced docs committed to git — builds need no sibling repos."
