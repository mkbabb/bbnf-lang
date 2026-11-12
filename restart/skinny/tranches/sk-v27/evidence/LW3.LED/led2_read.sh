#!/usr/bin/env bash
# LW3.LED-2 read: every file and test the LED-2 status rows cite exists at the
# sk-v27-lw3 heads in heads-LW3.json (`git cat-file -e`), and each cited line
# still says what the row quotes. usage: led2_read.sh <heads-LW3.json>
set -u
H=$1; P=~/Programming; rc=0
head_of() { python3 -c "import json,sys;print(json.load(open('$H'))['repos']['$1']['remote']['refs/heads/sk-v27-lw3'])"; }
BL=$(head_of bbnf-lang); PT=$(head_of parse-that); PP=$(head_of pprint)
SK=restart/skinny/tranches/sk-v27
exists() { # repo rev path
  if git -C $P/$1 cat-file -e "$2:$3" 2>/dev/null; then echo "exists  $1@${2:0:9}:$3"; else echo "ABSENT  $1@${2:0:9}:$3"; rc=1; fi; }
line_has() { # repo rev path line pattern
  local got; got=$(git -C $P/$1 show "$2:$3" 2>/dev/null | sed -n "$4p")
  if printf '%s' "$got" | grep -qE -- "$5"; then echo "line    $1@${2:0:9}:$3:$4 matches /$5/"; else echo "LINE?   $1@${2:0:9}:$3:$4 does not match /$5/"; rc=1; fi; }
# (a) LW1.RX-4, LED:25 UNREAD -> ruled in rx/RX-4-ADDENDUM.md
exists bbnf-lang $BL $SK/rx/RX-4-ADDENDUM.md
line_has bbnf-lang $BL $SK/LEDGER.md 25 '^\| LW1\.RX-4 .*\| UNREAD \|$'
line_has bbnf-lang $BL $SK/rx/RX-4-ADDENDUM.md 3 'Status:\*\* RULED'
line_has bbnf-lang $BL $SK/rx/RX-4-ADDENDUM.md 11 'Ruling: ACCEPTED\.'
line_has bbnf-lang $BL $SK/rx/RX-4-ADDENDUM.md 24 'Ruling: ACCEPTED IN PART\.'
# (b) red_l8_width_zero_indented_newline_panics: cured at LW2.PP-2 (LED:200), un-ignored at pprint master
exists pprint $PP rust/tests/born_red_lw1.rs
line_has pprint $PP rust/tests/born_red_lw1.rs 242 '^fn red_l8_width_zero_indented_newline_panics\('
line_has pprint $PP rust/tests/born_red_lw1.rs 241 '^#\[test\]$'
line_has bbnf-lang $BL $SK/LEDGER.md 200 "Also cured here: LW1's \`red_l8_width_zero_indented_newline_panics\` \(routed to LW3\.PP-1\)"
# (c) consumers critic 2 #7, LED:115 and LED:133: designs/consumers.md:78 rules only the visible part
exists bbnf-lang $BL $SK/designs/consumers.md
line_has bbnf-lang $BL $SK/designs/consumers.md 78 '#7, ACCEPT as far as I can see it'
line_has bbnf-lang $BL $SK/LEDGER.md 115 'consumers #7 \(truncated\) \| UNREAD'
line_has bbnf-lang $BL $SK/LEDGER.md 133 'consumers critic 2 #7 \(the part past its cut\)'
# (d) the three born_red_lw1 tests whose reasons name the closed LW2.RX-4
exists parse-that $PT rust/regex/tests/born_red_lw1.rs
line_has parse-that $PT rust/regex/tests/born_red_lw1.rs 95 '^#\[ignore = .*cure LW2\.RX-4 / LW4\.RX-2"\]$'
line_has parse-that $PT rust/regex/tests/born_red_lw1.rs 96 '^fn superset_absorb_compares_scalar_values\('
line_has parse-that $PT rust/regex/tests/born_red_lw1.rs 109 '^#\[ignore = .*cure LW2\.RX-4 / LW4\.RX-2"\]$'
line_has parse-that $PT rust/regex/tests/born_red_lw1.rs 110 '^fn union_merge_keeps_both_branches_languages\('
line_has parse-that $PT rust/regex/tests/born_red_lw1.rs 137 '^#\[ignore = .*\(SupersetAbsorbAlt, C2-1\).*cure LW2\.RX-4"\]$'
line_has parse-that $PT rust/regex/tests/born_red_lw1.rs 138 '^fn superset_absorb_requires_equal_consumption\('
line_has bbnf-lang $BL $SK/LEDGER.md 73 '^\| regex C2-1 \(`L_cut` certificate; absorb preconditions\) \| LW1\.RX-2; LW2\.RX-4; LW4\.RX-2 \|$'
line_has bbnf-lang $BL $SK/LEDGER.md 208 'RX-4 removes the rewrite, not the DFA'
echo "rc=$rc"; exit $rc
