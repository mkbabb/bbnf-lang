#!/bin/bash
set -euo pipefail

export LC_ALL=C

total=0
for source_dir in \
    /Users/mkbabb/Programming/parse-that/rust/parse_that \
    /Users/mkbabb/Programming/parse-that/rust/regex \
    /Users/mkbabb/Programming/pprint/rust \
    /Users/mkbabb/Programming/pprint/rust/derive; do
    printf 'component=%s\n' "$source_dir"
    count=$(find "$source_dir" \
        -type d \( -name .git -o -name target -o -name node_modules \
            -o -name __pycache__ -o -name .cache -o -name .pytest_cache \
            -o -name .mypy_cache \) -prune -o -type l -print | tee /dev/stderr | wc -l | tr -d ' ')
    printf 'symlinks=%s\n' "$count"
    total=$((total + count))
done
printf 'total_symlinks=%s\n' "$total"
