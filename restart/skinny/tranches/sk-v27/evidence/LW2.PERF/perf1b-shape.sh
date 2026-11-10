#!/bin/bash
# LW2.PERF-1 shape falsifier (light: git reads only). Usage:
#   perf1-shape.sh <bbnf-lang tree> <parse-that tree>
# Prints each reading and exits non-zero when any is RED.
#   A  the three source crates are absent from bbnf-lang
#   B  `core::arch` in Rust source of either repo, outside parse-that rust/simd/
#      (records, docs and archives excluded: restart/, docs/, audit/, restart-archive-*),
#      is exactly the 13 files of ruling S6-A (each carried by U5 or U6); any other
#      file, or a missing one, is RED (v2, PERF-1b)
#   C  the generality grep over parse-that rust/simd (`bbnf`, any case) = 0
#   D  parse_that::scan re-exports parse-that-simd's entry points
#   E  no Cargo manifest in either repo names simd-scan or bbnf-simd (v2: with B's
#      records exclusion; v1 counted 3 archived manifests under docs/ and restart/)
BB=${1:?bbnf-lang tree}; PT=${2:?parse-that tree}; red=0
say() { printf '%-2s %-5s %s\n' "$1" "$2" "$3"; [ "$2" = RED ] && red=1; return 0; }
n=0; for d in crates/simd-scan skinny/crates/simd-scan skinny/crates/bbnf-simd; do [ -d "$BB/$d" ] && n=$((n+1)) && echo "   present: $d"; done
[ $n -eq 0 ] && say A GREEN "source crates absent (0 of 3)" || say A RED "$n of 3 source crates still in bbnf-lang"
ex=":!restart :!docs :!audit :!restart-archive-*"
bb=$(cd "$BB" && git grep -l 'core::arch' -- '*.rs' $ex | sed 's|^|bbnf-lang/|')
pt=$(cd "$PT" && git grep -l 'core::arch' -- '*.rs' $ex | grep -v '^rust/simd/' | sed 's|^|parse-that/|')
S6A="bbnf-lang/crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs
bbnf-lang/crates/core/src/generate/regex/emit/simd.rs
bbnf-lang/crates/core/src/grammar/generated/bbnf.rs
bbnf-lang/crates/core/src/grammar/generated/bnf.rs
bbnf-lang/crates/core/src/grammar/generated/css_l4.rs
bbnf-lang/crates/core/src/grammar/generated/css_pretty.rs
bbnf-lang/crates/core/src/grammar/generated/csv.rs
bbnf-lang/crates/core/src/grammar/generated/ebnf.rs
bbnf-lang/crates/core/src/grammar/generated/google_sheets.rs
bbnf-lang/crates/core/src/grammar/generated/json.rs
bbnf-lang/crates/core/src/grammar/generated/math.rs
parse-that/rust/parse_that/src/parsers/scan/lanes.rs
parse-that/rust/parse_that/src/parsers/scan/number_simd.rs"
got=$(printf '%s\n%s\n' "$bb" "$pt" | grep . | sort)
want=$(printf '%s\n' "$S6A" | sort)
out=$(printf '%s\n' "$got" | grep -c .)
extra=$(comm -23 <(printf '%s\n' "$got") <(printf '%s\n' "$want")); missing=$(comm -13 <(printf '%s\n' "$got") <(printf '%s\n' "$want"))
if [ -z "$extra" ] && [ -z "$missing" ]; then say B GREEN "core::arch outside rust/simd/ is exactly the 13 S6-A files"
else say B RED "$out files outside rust/simd/; not the S6-A set:"; [ -n "$extra" ] && printf '%s\n' "$extra" | sed 's/^/     extra   /'; [ -n "$missing" ] && printf '%s\n' "$missing" | sed 's/^/     missing /'; fi
g=$(cd "$PT" && git grep -niE 'bbnf' -- rust/simd | wc -l | tr -d ' ')
if ! (cd "$PT" && git ls-files --error-unmatch rust/simd/Cargo.toml >/dev/null 2>&1); then say C RED "parse-that has no rust/simd crate"
elif [ "$g" -eq 0 ]; then say C GREEN "generality grep over rust/simd = 0"; else say C RED "generality grep over rust/simd = $g"; fi
(cd "$PT" && git grep -q 'pub use parse_that_simd::' -- rust/parse_that/src/parsers/scan/mod.rs) && say D GREEN "parse_that::scan re-exports parse-that-simd" || say D RED "no parse_that::scan face"
m=$( (cd "$BB" && git grep -nE '^(simd-scan|bbnf-simd)|"crates/(simd-scan|bbnf-simd)"|crates/bbnf-simd' -- '*Cargo.toml' $ex; cd "$PT" && git grep -nE 'simd-scan|bbnf-simd' -- '*Cargo.toml' $ex) | wc -l | tr -d ' ')
[ "$m" -eq 0 ] && say E GREEN "no manifest names simd-scan/bbnf-simd" || say E RED "$m manifest lines name simd-scan/bbnf-simd"
exit $red
