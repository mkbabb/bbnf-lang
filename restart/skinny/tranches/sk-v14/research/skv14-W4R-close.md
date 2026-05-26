# SK-V14 W4R Close: CSS L4 Ledger PRUNE

Date: 2026-05-26.
Wave: W4R.
Phase: redress close.
Disposition: ADMITTED.

## Scope

W4R closes the amended W4 PRUNE-2 gate as ledger-only work.

Landed behavior:

- 24 CSS L4 rows in `restart/skinny/ROLLING-SOTA-DELTA.md` are restored from
  `ADMITTED` to `OPEN`;
- REDRESS-185 through REDRESS-208 close the 24 CSS L4 row keys as PRUNE;
- `skinny/RESULTS.md` remains read/no-op and retains all CSS L4
  `AUDIT-FALSIFIED` overlays;
- no CSS source, generator, provider, template, runtime-twin, or `regen_css`
  deletion is performed by W4.

## Consumer Correction

The amended W4 same-wave consumer exposed a stale `gate-json` rule: numeric
CSS rolling-delta evidence was still forced to `ADMITTED`. W4R updates the
consumer so numeric CSS rows may remain `OPEN` only when the corresponding
RESULTS row carries both:

- `per_iter_equality` beginning with `not_admitted:`;
- `audit_overlay_verdict=AUDIT-FALSIFIED`.

The Lock 14 baseline parent-diff allowance is narrowed to W4's gate consumer
files (`xtask/src/main.rs` and `crates/bbnf-bench/src/lock14_baseline.rs`) so
the W4 redress commit can be validated without opening provider/runtime
source ownership.

## Evidence

Commands run at HEAD during W4R redress:

```sh
awk 'BEGIN{in_css=0;rows=0;admitted=0} /^## CSS L4 Targets/{in_css=1; next} /^## Gate Notes/{in_css=0} in_css && /^\| css_l4\// {rows++; if ($0 ~ /\| ADMITTED \|$/) admitted++} END{print rows, admitted}' restart/skinny/ROLLING-SOTA-DELTA.md
awk -F'|' '/^\| json\// {plane=$3; status=$7; gsub(/^[ \t]+|[ \t]+$/,"",plane); gsub(/^[ \t]+|[ \t]+$/,"",status); total[plane]++; if(status=="ADMITTED") admitted[plane]++} END{print "parse_only", total["parse_only"], admitted["parse_only"]+0; print "direct_to_struct", total["direct_to_struct"], admitted["direct_to_struct"]+0; print "real_typed_struct", total["real_typed_struct"], admitted["real_typed_struct"]+0}' restart/skinny/ROLLING-SOTA-DELTA.md
rg '^\| css_l4/' skinny/RESULTS.md | rg -c 'AUDIT-FALSIFIED'
rg -c 'Item (18[5-9]|19[0-9]|20[0-8]) closes `css_l4/' skinny/REDRESS.md
git diff --name-status --diff-filter=D HEAD -- grammar/css/l4 crates/core/src/runtime/css_l4 crates/core/src/grammar/generated/css_l4.rs crates/core/src/grammar/generated/css_l4.registry.json skinny/crates/codegen/src 'skinny/crates/runtime/src/grammars/css_l4_*' skinny/xtask/src/regen_css.rs
git diff --name-only HEAD -- skinny/crates/codegen/src skinny/crates/runtime/src/grammars skinny/xtask/src/regen_css.rs
find grammar/css/l4 -type f -name '*.bbnf' | sort | wc -l
find skinny/crates/codegen/src -maxdepth 1 -name 'css_l4_*_provider.rs' | sort | wc -l
find skinny/crates/codegen/src -maxdepth 2 -path '*/css_l4_*_templates/*.rs' -type f | sort | wc -l
find skinny/crates/runtime/src/grammars -maxdepth 2 -path '*/css_l4_*/*.rs' -type f | sort | wc -l
cargo test -p xtask skv13_rolling_delta_accepts_full_json_and_css_universe -- --nocapture
cargo test -p bbnf-bench admits_sk_v14_w4_gate_json_parent_diff_only_under_w4_scope -- --nocapture
cargo xtask gate-json --check-results --skv14-existing-results-capture
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
```

Observed results:

- CSS L4 rolling delta: `24 0`.
- JSON rolling delta: `parse_only 17 0`, `direct_to_struct 17 0`,
  `real_typed_struct 17 0`.
- CSS RESULTS audit overlay: 24 rows retain `AUDIT-FALSIFIED`.
- REDRESS row-key entries: 24.
- Deletion-filter diff over W4 non-owner CSS/generator/runtime paths: empty.
- Source/generator diff over `skinny/crates/codegen/src`,
  `skinny/crates/runtime/src/grammars`, and `skinny/xtask/src/regen_css.rs`:
  empty.
- Preserved file counts: 15 CSS `.bbnf` source files, 7 CSS provider modules,
  35 CSS template files, 35 skinny runtime CSS files.
- `xtask` rolling-delta unit: 1 passed.
- Lock 14 W4 parent-diff unit: 1 passed.
- `cargo xtask gate-json --check-results --skv14-existing-results-capture`:
  pass.
- Lock count invariant: 16.
- Pattern H runtime-file invariant: 67.

## Exit Gate

W4R satisfies amended SPEC §7:

- CSS L4 is 0/24 in the rolling delta;
- JSON rows remain at 0/17 admitted for all three classes;
- REDRESS-185..208 name every CSS L4 row key and cite
  `sk-v13/v1-css-l4-validation:§1-6`;
- RESULTS retains the audit overlay;
- W4 performs no CSS source/generator/provider/template deletion.

W5 PRUNE-3 is now the next executable wave. W6.0 still owns
`crates/core/src/runtime/css_l4/`, and W8/W9/W10 remain blocked until
PRUNE-1 through PRUNE-5 close.
