#!/bin/sh
# S15 FREEZE falsifier: RULINGS.md carries the 2026-11-10 delegate addendum with
# one row each for S4-B and A-8..A-12, and A-8's row says PROVISIONAL and PP-U9.
# Usage: rulings-lw2-check.sh <RULINGS.md>
set -u; F=$1; fail=0
sec=$(sed -n '/^## Addendum, 2026-11-10: LW2 delegate rulings/,$p' "$F")
[ -n "$sec" ] || { echo "RED: no 2026-11-10 LW2 delegate addendum"; exit 1; }
for k in S4-B A-8 A-9 A-10 A-11 A-12; do
  n=$(printf '%s\n' "$sec" | grep -c "^| $k |"); [ "$n" = 1 ] || { echo "RED: $k rows=$n"; fail=1; }
done
printf '%s\n' "$sec" | grep "^| A-8 |" | grep -q PROVISIONAL || { echo "RED: A-8 not PROVISIONAL"; fail=1; }
printf '%s\n' "$sec" | grep "^| A-8 |" | grep -q PP-U9 || { echo "RED: A-8 lacks PP-U9"; fail=1; }
[ $fail -eq 0 ] && echo "GREEN: S4-B, A-8..A-12 recorded"; exit $fail
