#!/bin/bash
set -euo pipefail

export LC_ALL=C

if [[ $# -ne 1 ]]; then
    echo "usage: $0 SOURCE_DIRECTORY" >&2
    exit 64
fi

source_dir=$1
(
    cd "$source_dir"
    while IFS= read -r -d '' file; do
        file_hash=$(shasum -a 256 "$file" | awk '{print $1}')
        file_mode=$(stat -f '%Lp' "$file")
        file_size=$(stat -f '%z' "$file")
        printf '%s\t%s\t%s\t%s\n' "$file_hash" "$file_mode" "$file_size" "$file"
    done < <(
        find . \
            -type d \( -name .git -o -name target -o -name node_modules \
                -o -name __pycache__ -o -name .cache -o -name .pytest_cache \
                -o -name .mypy_cache \) -prune -o \
            -type f ! -name .DS_Store ! -name '*.pyc' -print0 | sort -z
    )
)
