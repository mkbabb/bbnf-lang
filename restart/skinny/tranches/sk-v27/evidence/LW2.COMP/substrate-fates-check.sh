#!/bin/sh
# LW1.COMP-1b falsifier (LW2 S2 CENSUS, under Z-32 and LW2-ORDER amendment A5).
#
# Usage: substrate-fates-check.sh <CRATE-CENSUS.tsv> <ZERO-CALLER.tsv> [repo-root]
#
# ZERO-CALLER.tsv is the substrate_audit list read at the integration base
# (kind<TAB>name<TAB>file:line). Every listed item must carry exactly one
# census row (tree = `substrate`, path = file:line, role = `<kind> <name>`)
# whose fate is in {DELETE, ROUTE, MOVE-with-crate}. SANCTION is not a fate.
#   DELETE           the item's `pub` definition is gone from its crate's src/.
#   ROUTE            owning_unit is an LW2..LW4 unit that PLAN.md carries, and
#                    the PLAN.md line carrying that unit names the item; plus one
#                    SANCTIONED_SUBSTRATES entry whose reason cites the unit.
#   MOVE-with-crate  crate is csp-solver (LW2.TOPO-1) or simd-scan (LW2.PERF-1);
#                    with FATES_AT=gate (S16) the crate's directory must also be
#                    gone from this repository, i.e. the move has happened.
# A `pub` definition is any public item form: `pub` followed by any of const,
# unsafe, async or extern "ABI", then fn, struct, enum, trait, static [mut],
# const, type, union or mod. `pub(crate)` and other restricted forms are not
# public and do not count.
# The SANCTIONED_SUBSTRATES list holds exactly as many entries as ROUTE rows.
T=${1:?census tsv}; Z=${2:?zero-caller tsv}; R=${3:-$(git rev-parse --show-toplevel)}
PLAN=$R/restart/skinny/tranches/sk-v27/PLAN.md
AUDIT=$R/crates/ir/tests/substrate_audit.rs
bad=0; routes=0
fail() { echo "REFUSED $*"; bad=$((bad+1)); }
tab=$(printf '\t')
PUB_ITEM='(^|[^A-Za-z0-9_])pub( +(const|unsafe|async|extern( +"[^"]*")?))* +(fn|struct|enum|trait|static( +mut)?|const|type|union|mod)'
while IFS="$tab" read -r kind name loc; do
  [ -n "$name" ] || continue
  rows=$(awk -F'\t' -v p="$loc" -v r="$kind $name" '$1=="substrate" && $2==p && $6==r' "$T")
  n=$(printf '%s' "$rows" | grep -c .)
  [ "$n" -eq 1 ] || { fail "$name: $n census rows (want 1)"; continue; }
  crate=$(printf '%s' "$rows" | cut -f3); fate=$(printf '%s' "$rows" | cut -f7)
  unit=$(printf '%s' "$rows" | cut -f8)
  file=${loc%%:*}; cdir=$(echo "$file" | cut -d/ -f1-2)
  case $fate in
    DELETE)
      if [ -d "$R/$cdir/src" ] && grep -rEq "$PUB_ITEM +$name([^A-Za-z0-9_]|\$)" "$R/$cdir/src"; then
        fail "$name: fate DELETE but \`pub $kind $name\` still defined under $cdir/src"
      fi ;;
    ROUTE)
      routes=$((routes+1))
      echo "$unit" | grep -Eq '^LW[234]\.[A-Z]+-[0-9]+[a-z]?$' || { fail "$name: ROUTE unit '$unit' is not an LW2..LW4 unit id"; continue; }
      grep -Fq "$unit" "$PLAN" || { fail "$name: ROUTE unit $unit absent from PLAN.md"; continue; }
      grep -F "$unit" "$PLAN" | grep -Fwq "$name" || fail "$name: PLAN.md row of $unit does not name the item"
      grep -E "^\s*\"$name\",\s*//.*$unit" "$AUDIT" >/dev/null || fail "$name: no SANCTIONED_SUBSTRATES entry citing $unit" ;;
    MOVE-with-crate)
      case "$crate:$unit" in
        csp-solver:LW2.TOPO-1|simd-scan:LW2.PERF-1)
          if [ "${FATES_AT:-}" = gate ] && [ -d "$R/$cdir" ]; then
            fail "$name: fate MOVE-with-crate but $cdir is still in this repository at the gate"
          fi ;;
        *) fail "$name: MOVE-with-crate needs csp-solver/LW2.TOPO-1 or simd-scan/LW2.PERF-1, got $crate/$unit" ;;
      esac ;;
    *) fail "$name: fate '$fate' is not in {DELETE, ROUTE, MOVE-with-crate}" ;;
  esac
done < "$Z"
extra=$(awk -F'\t' '$1=="substrate"' "$T" | wc -l | tr -d ' ')
want=$(grep -c . "$Z")
[ "$extra" -eq "$want" ] || fail "census holds $extra substrate rows, zero-caller list holds $want"
sanc=$(awk '/^const SANCTIONED_SUBSTRATES/{f=1;next} f&&/^\];/{f=0} f' "$AUDIT" | grep -Ec '^\s*"[A-Za-z_0-9]+",')
[ "$sanc" -eq "$routes" ] || fail "SANCTIONED_SUBSTRATES holds $sanc entries, ROUTE rows = $routes"
echo "rows=$want routes=$routes sanctioned=$sanc refused=$bad"
[ $bad -eq 0 ]
