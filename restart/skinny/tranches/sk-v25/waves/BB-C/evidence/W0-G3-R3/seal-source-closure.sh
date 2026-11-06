#!/bin/bash
set -euo pipefail

export LC_ALL=C
export TZ=UTC
export COPYFILE_DISABLE=1

evidence_dir=/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v25/waves/BB-C/evidence/W0-G3-R3
closure_dir="$evidence_dir/closure"

if [[ -e "$closure_dir" ]]; then
    echo "refusing to replace existing closure: $closure_dir" >&2
    exit 73
fi

mkdir -p "$closure_dir/archives" "$closure_dir/ledgers" "$closure_dir/reproduction"
work_dir=$(mktemp -d /tmp/bbnf-g3-r3-seal.XXXXXX)
trap 'rm -rf "$work_dir"' EXIT

write_ledger() {
    local source_dir=$1
    local output_file=$2
    (
        cd "$source_dir"
        while IFS= read -r -d '' file; do
            local_hash=$(shasum -a 256 "$file" | awk '{print $1}')
            local_mode=$(stat -f '%Lp' "$file")
            local_size=$(stat -f '%z' "$file")
            printf '%s\t%s\t%s\t%s\n' "$local_hash" "$local_mode" "$local_size" "$file"
        done < <(
            find . \
                -type d \( -name .git -o -name target -o -name node_modules \
                    -o -name __pycache__ -o -name .cache -o -name .pytest_cache \
                    -o -name .mypy_cache \) -prune -o \
                -type f ! -name .DS_Store ! -name '*.pyc' -print0 | sort -z
        )
    ) > "$output_file"
}

seal_component() {
    local label=$1
    local source_dir=$2
    local stage_dir="$work_dir/$label"
    local archive_a="$work_dir/$label-a.tar"
    local archive_b="$work_dir/$label-b.tar"
    local source_ledger="$closure_dir/ledgers/$label.before.tsv"
    local staged_ledger="$closure_dir/ledgers/$label.frozen.tsv"
    local archive_list="$work_dir/$label.archive-files.txt"

    write_ledger "$source_dir" "$source_ledger"
    mkdir -p "$stage_dir"
    rsync -a \
        --exclude=.git --exclude=target --exclude=node_modules \
        --exclude=__pycache__ --exclude=.cache --exclude=.pytest_cache \
        --exclude=.mypy_cache --exclude=.DS_Store --exclude='*.pyc' \
        "$source_dir/" "$stage_dir/"
    find "$stage_dir" -type f -exec touch -t 198001010000.00 {} +
    write_ledger "$stage_dir" "$staged_ledger"
    cmp "$source_ledger" "$staged_ledger"

    (
        cd "$stage_dir"
        find . -type f -print | sort > "$archive_list"
    )
    tar -c --format pax --uid 0 --gid 0 --uname root --gname root \
        -f "$archive_a" -C "$stage_dir" -T "$archive_list"
    tar -c --format pax --uid 0 --gid 0 --uname root --gname root \
        -f "$archive_b" -C "$stage_dir" -T "$archive_list"

    local hash_a
    local hash_b
    hash_a=$(shasum -a 256 "$archive_a" | awk '{print $1}')
    hash_b=$(shasum -a 256 "$archive_b" | awk '{print $1}')
    [[ "$hash_a" == "$hash_b" ]]
    cp "$archive_a" "$closure_dir/archives/$label-$hash_a.tar"
    printf 'archive_a_sha256=%s\narchive_b_sha256=%s\nequal=yes\n' \
        "$hash_a" "$hash_b" > "$closure_dir/reproduction/$label.txt"
}

seal_component parse-that-parse-that /Users/mkbabb/Programming/parse-that/rust/parse_that
seal_component parse-that-regex /Users/mkbabb/Programming/parse-that/rust/regex
seal_component pprint-rust /Users/mkbabb/Programming/pprint/rust
seal_component pprint-derive /Users/mkbabb/Programming/pprint/rust/derive

(
    cd "$closure_dir"
    find archives ledgers reproduction -type f -print | sort | while IFS= read -r file; do
        shasum -a 256 "$file"
    done
) > "$closure_dir/MANIFEST.sha256"
