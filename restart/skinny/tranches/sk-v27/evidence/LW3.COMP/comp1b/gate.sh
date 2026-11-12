#!/bin/sh
# sk-v27 LW3.COMP-1b GREEN reading (S7 SPINE): every gate LW3-ORDER row COMP-1b
# lists, plus amendment 4's, read once at the bbnf-lang tree <bl>, after
# `cargo clean -p` of the crates COMP-1b touched (ruling TGT-1).
# Usage: gate.sh <bl worktree> <pt worktree at the pin> <pprint tree at the pin> <log dir>
# Every line of <log dir>/gates.tsv is `gate<TAB>rc<TAB>reading`. Exit 0 iff every rc is 0.
set -u
BL=$1; PT=$2; PP=$3; L=$4; mkdir -p "$L"; : > "$L/gates.tsv"
SK=restart/skinny/tranches/sk-v27
export CARGO_TARGET_DIR=${CARGO_TARGET_DIR:-$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang}
row() { printf '%s\t%s\t%s\n' "$1" "$2" "$3" >> "$L/gates.tsv"; }
summary() { grep -E '^ +Summary|tests run:' "$1" | tail -1 | sed 's/^ *//'; }
cd "$BL" || exit 2
echo "bl=$(git rev-parse HEAD) tree=$(git rev-parse 'HEAD^{tree}')" > "$L/heads.txt"
echo "pt=$(git -C "$PT" rev-parse HEAD) pp=$(git -C "$PP" rev-parse HEAD)" >> "$L/heads.txt"

# TGT-1: the crates COMP-1b touched (bbnf; the re-pinned parse-that crates).
cargo clean -p bbnf -p parse_that -p parse-that-regex -p parse-that-simd -p parse-that-egraph \
  -p parse-that-egraph-derive > "$L/clean.log" 2>&1; row tgt1-clean $? "cargo clean -p bbnf + 5 parse-that crates"
cargo clean -p bbnf --profile ax-iter >> "$L/clean.log" 2>&1

nt() { name=$1; shift
  cargo nextest run -p bbnf --locked --no-fail-fast "$@" > "$L/$name.log" 2>&1; rc=$?
  row "$name" $rc "$(summary "$L/$name.log")"; }
nt root_api_present --test root_api_present
cargo xtask regen --check > "$L/regen.log" 2>&1; row regen-check $? "$(tail -1 "$L/regen.log")"
nt w2-laws --test frontend_contract --test dependency_scc_laws --test emitter_contract \
  --test replay_edit_laws --test record_semantic_laws --test carrier_lookup_laws --test identity_opacity
nt w3-type --test type_analysis_laws
nt w3-instance --test w3_instance_frontend_contract
nt w3-pratt --test w3_semantic_pratt_contract
nt w3-replay --test w3_replay_public_contract
nt bootstrap-repro --test bbnf_bootstrap_reproducibility
# The two named born-RED gates (RULINGS 2026-11-12d COMP1B-W3 (b), (c)) still fail for their reasons.
cargo nextest run -p bbnf --locked --no-fail-fast --run-ignored only --test w3_replay_public_contract \
  --test w3_instance_frontend_contract > "$L/born-red.log" 2>&1; brc=$?
n=0; for t in active_roots_remain_preselection_only_and_the_guard_self_falsifies \
  every_core_and_external_control_caller_uses_only_the_clean_break_instance_api; do
  grep -qE "FAIL .*$t" "$L/born-red.log" && n=$((n+1)); done
[ $brc -ne 0 ] && [ "$n" -ge 2 ]; row born-red-still-red $? "rc=$brc named-fails=$n $(summary "$L/born-red.log")"

# Amendment 4: the test-target set is a superset of master's plus the spine's.
cargo test -p bbnf --locked --no-run --message-format=json 2> "$L/targets.err" \
  | python3 -I -c '
import json, sys
names = set()
for line in sys.stdin:
    try: m = json.loads(line)
    except ValueError: continue
    if m.get("reason") == "compiler-artifact" and m["target"]["kind"] == ["test"] and "/crates/core#" in m["package_id"]:
        names.add(m["target"]["name"])
print("\n".join(sorted(names)))' > "$L/targets.txt"
python3 -I - "$L/targets.txt" "$BL/$SK/evidence/LW3.COMP/comp1b/master-test-targets.txt" > "$L/targets-check.log" <<'PY'
import sys
have = set(open(sys.argv[1]).read().split()); master = set(open(sys.argv[2]).read().split())
spine = {"frontend_contract", "dependency_scc_laws", "type_analysis_laws", "emitter_contract",
         "replay_edit_laws", "record_semantic_laws", "carrier_lookup_laws", "identity_opacity",
         "w3_instance_frontend_contract", "w3_semantic_pratt_contract", "w3_replay_public_contract",
         "instance_support", "root_api_present"}
miss = sorted((master | spine) - have)
print(f"targets={len(have)} master={len(master)} spine={len(spine)} missing={len(miss)} {miss}")
sys.exit(1 if miss else 0)
PY
row test-targets $? "$(cat "$L/targets-check.log")"

cargo build --workspace --locked > "$L/build-workspace.log" 2>&1; row build-workspace $? "$(tail -1 "$L/build-workspace.log")"
PATH=$HOME/.dev-logs/bbnf-lockstep/tools/bin:$PATH cargo semver-checks check-release \
  --manifest-path crates/core/Cargo.toml --baseline-version 0.3.0 > "$L/semver.log" 2>&1
grep -qE 'failure [a-z_]*(missing|removed)' "$L/semver.log"; removal=$?
grep -q 'Checked \[' "$L/semver.log"; ran=$?
[ $ran -eq 0 ] && [ $removal -ne 0 ]; row semver-no-removal $? "$(grep -E 'Checked|failure ' "$L/semver.log" | tr '\n' ' ')"
sh scripts/check-repo-dag.sh > "$L/repo-dag.log" 2>&1; row check-repo-dag $? "$(tail -1 "$L/repo-dag.log")"
sh "$SK/evidence/LW2.TOPO-4e/pins.sh" "$PP" "$PT" "$BL" > "$L/pins.log" 2>&1; row pins $? "$(tail -1 "$L/pins.log")"
sh scripts/check-hermetic-build.sh > "$L/hermetic.log" 2>&1; row check-hermetic-build $? "$(tail -1 "$L/hermetic.log")"
(cd "$PT/rust" && CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-parse-that \
  cargo nextest run -p parse_that --locked --no-fail-fast --test kernel_abi --test kernel --test kernel_allocation --test kernel_regex) > "$L/kernel-abi.log" 2>&1
row abi-laws-at-pin $? "$(summary "$L/kernel-abi.log")"
cat "$L/gates.tsv"
! cut -f2 "$L/gates.tsv" | grep -qv '^0$'
