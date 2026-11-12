#!/bin/sh
# LW3.COMP-1b-W falsifier (seat S7 SPINE; RULINGS 2026-11-12b Z32-W, 2026-11-12c P2C-2).
#
# Usage: route-witness.sh <checker.sh> <commit> <scratch-dir> [mutant]
#
# Exports <commit>'s crates/, PLAN.md, LEDGER.md and the census evidence into
# <scratch-dir>, then reads the census checker <checker.sh> on two trees:
#   control  the census of record as committed (CRATE-CENSUS.tsv against
#            ZERO-CALLER-LW1.tsv): must read refused=0.
#   witness  the same plus one recognizer substrate whose only consumer is
#            shape mining: `struct CommentWsMiner`
#            (crates/ir/src/passes/recognizers/comment_ws.rs:17) listed
#            zero-caller, fated ROUTE to LW5.COMP-1-MINE (PLAN §7.2 A-01:
#            "every recognizer of `crates/ir/src/passes/recognizers/` becomes a
#            `ShapeMiner`"), with one SANCTIONED_SUBSTRATES entry citing the
#            unit. Z32-W: the route must be admitted (refused=0).
# The optional [mutant] changes the witness:
#   unadmitted  the ROUTE names LW5.COMP-1-NOPE, which no PLAN or LEDGER row
#               admits: must be refused.
#   unnamed     the witness item is `fn calculate_acyclic_deps_scc`'s site
#               (crates/core/src/graph/scc.rs:210), which no line carrying
#               LW5.COMP-1-MINE names: must be refused.
#   crate-wide  PLAN's A-01 line names only `crates/ir/` instead of the
#               recognizer directory: must be refused (a row cannot cover a
#               whole crate).
# Prints `control: …` and `witness: …` (the checker's last line each) and
# exits 0 only when control reads refused=0 and the witness reads as the case
# requires (admitted with no mutant, refused with one).
set -u
CHK=$(cd "$(dirname "$1")" && pwd)/$(basename "$1"); C=$2; S=$3; MUT=${4:-}
REPO=$(git rev-parse --show-toplevel)
SK=restart/skinny/tranches/sk-v27
EV=$SK/evidence
rm -rf "$S"; mkdir -p "$S/control"
git -C "$REPO" archive "$C" crates $SK/PLAN.md $SK/LEDGER.md \
  $EV/LW1.COMP/CRATE-CENSUS.tsv $EV/LW2.COMP/ZERO-CALLER-LW1.tsv | tar -x -C "$S/control"
run() { # $1 = tree
  sh "$CHK" "$1/$EV/LW1.COMP/CRATE-CENSUS.tsv" "$1/$EV/LW2.COMP/ZERO-CALLER-LW1.tsv" "$1" 2>&1
}
cout=$(run "$S/control"); echo "control: $(printf '%s' "$cout" | tail -1)"
W=$S/witness; cp -R "$S/control" "$W"
unit=LW5.COMP-1-MINE
kind=struct; name=CommentWsMiner; loc=crates/ir/src/passes/recognizers/comment_ws.rs:17
case $MUT in
  '') ;;
  unadmitted) unit=LW5.COMP-1-NOPE ;;
  unnamed) kind=fn; name=calculate_acyclic_deps_scc; loc=crates/core/src/graph/scc.rs:210 ;;
  crate-wide)
    python3 -I - "$W/$SK/PLAN.md" <<'EOF'
import sys
p = sys.argv[1]; t = open(p).read()
old = "every recognizer of `crates/ir/src/passes/recognizers/`"
assert old in t, "A-01 text moved"
open(p, "w").write(t.replace(old, "every recognizer of `crates/ir/`", 1))
EOF
    ;;
  *) echo "unknown mutant $MUT"; exit 2 ;;
esac
# the item must exist where the row says it is (the checker reads the tree)
grep -nE "pub $kind $name([^A-Za-z0-9_]|\$)" "$W/${loc%%:*}" >/dev/null || { echo "witness item $kind $name absent at ${loc%%:*}"; exit 2; }
printf '%s\t%s\t%s\n' "$kind" "$name" "$loc" >> "$W/$EV/LW2.COMP/ZERO-CALLER-LW1.tsv"
printf 'substrate\t%s\tbbnf-ir\t-\t-\t%s %s\tROUTE\t%s\tLW3.COMP-1b-W witness: a recognizer whose only consumer is shape mining (PLAN 7.2 A-01)\n' \
  "$loc" "$kind" "$name" "$unit" >> "$W/$EV/LW1.COMP/CRATE-CENSUS.tsv"
python3 -I - "$W/crates/ir/tests/substrate_audit.rs" "$name" "$unit" <<'EOF'
import sys
p, name, unit = sys.argv[1:4]; t = open(p).read()
head = "const SANCTIONED_SUBSTRATES: &[&str] = &[\n"
assert head in t, "SANCTIONED_SUBSTRATES moved"
open(p, "w").write(t.replace(head, head + f'    "{name}", // {unit}: becomes a ShapeMiner (LW3.COMP-1b-W witness)\n', 1))
EOF
wout=$(run "$W"); echo "witness${MUT:+ [$MUT]}: $(printf '%s' "$wout" | tail -1)"
printf '%s\n' "$wout" | grep '^REFUSED' | sed 's/^/  /'
printf '%s' "$cout" | tail -1 | grep -q 'refused=0$' || exit 1
if [ -z "$MUT" ]; then
  printf '%s' "$wout" | tail -1 | grep -q 'routes=1 sanctioned=1 refused=0$'
else
  printf '%s' "$wout" | tail -1 | grep -qv 'refused=0$'
fi
