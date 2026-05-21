# SK-V12 W4 A4 - JSON Guards And Non-Selected Orphan Demotion

Date: 2026-05-20.
Scope: read-only research for W4 ASM-gen CSS consumer planning.
Ownership: W4 research only; no source edits.

## Authority Read

- `SPEC.md` Section 0.5 keeps four JSON direct guard floors and seven JSON
  typed guard floors binding for any wave that can affect JSON-producing
  runtime, codegen, benchmark, report, gate, parser, scanner, or SIMD paths.
- `SPEC.md` Section 9 makes W4 responsible for one selected ASM-gen candidate,
  same-wave CSS or JSON-guard consumer measurement, JSON guard disposition, and
  five-row aarch64 orphan accounting.
- `SPEC.md` Section 10 routes final `RESULTS.md` reconciliation and campaign
  close to W5 after W4 admits, rejects, or records nonclose evidence.
- `skinny/REDRESS.md` through 125 records W1b-2b as a CSS L4
  `PASS-ADMIT-CANDIDATE` companion report, not final campaign close and not a
  `RESULTS.md` movement.
- `skv12-profile-truth-audit.md` names JSON guard hot leaves and confirms the
  current guard floor source. `skv12-aarch64-simd-coverage-audit.md` names the
  five carried W4 orphans and their REDRESS adjacency.
- `bbnf-bench` gate/report code currently supports separate CSS companion and
  JSON no-write guard paths. `--skv12-css-l4-sota-report` validates CSS
  Criterion/fact artifacts; normal `gate` execution validates JSON rows and
  compares rendered output to `skinny/RESULTS.md`.

## W4 JSON Guard Commands

W4 must not use a CSS-only Criterion root as JSON guard proof. The JSON guard
root must contain all `json_<fixture>/...` groups plus the SIMD metadata rows
consumed by `skinny/crates/bbnf-bench/src/bin/gate.rs`.

Use the accepted W1a guard root only for a no-write guard check:

```sh
before=$(shasum -a 256 skinny/RESULTS.md | awk '{print $1}')
CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- \
  --advisory --check-results
after=$(shasum -a 256 skinny/RESULTS.md | awk '{print $1}')
test "$before" = "$after"
awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk \
  skinny/RESULTS.md
```

That command is enough only when W4 proves either no JSON-producing behavior
path moved, or the selected candidate is isolated to CSS and the existing JSON
surface stays byte-identical.

If W4 touches a shared production path reachable from JSON, especially
`bbnf-simd`, `parse-that-regex`, generated JSON templates, JSON runtime,
`report.rs`, or `gate.rs`, use a fresh populated guard root:

```sh
rm -rf /tmp/skv12-w4-json-guard-criterion
CRITERION_HOME=/tmp/skv12-w4-json-guard-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path skinny/Cargo.toml -p xtask -- bench-json --advisory

before=$(shasum -a 256 skinny/RESULTS.md | awk '{print $1}')
CRITERION_HOME=/tmp/skv12-w4-json-guard-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- \
  --advisory --check-results
CRITERION_HOME=/tmp/skv12-w4-json-guard-criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- \
  --with-cost-facts --advisory --check-results
after=$(shasum -a 256 skinny/RESULTS.md | awk '{print $1}')
test "$before" = "$after"
awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk \
  skinny/RESULTS.md
```

Run the CSS W4 candidate gate separately from the JSON guard root. If W4 adds a
new companion report, keep the current W1b-2b split-root rule: CSS companion
validation reads CSS Criterion/artifacts; JSON stale/floor checking runs as a
separate JSON command without the CSS flag.

## RESULTS.md And Demotion Handling

Default W4 behavior is no-write for `skinny/RESULTS.md`. A CSS improvement or
ASM-gen nonclose/reject in W4 should be recorded in REDRESS and W4 artifacts;
W5 owns final close reconciliation and any campaign-close results rendering.

`--update-results`, `--write-results`, and volatile probe flags must not be
combined with companion-report validation. The gate code already rejects these
combinations for W1a/W1b/W1b-2b report flags; W4 should preserve that rule if
it adds a W4 report flag.

If a refreshed JSON guard misses a Section 0.5 floor, W4 has only three honest
routes:

1. Revert or revise the selected source candidate until the guard holds.
2. Record `MEASURED-REJECT` for W4, save the rejected patch, and leave
   `skinny/RESULTS.md` byte-identical.
3. Record an explicit measured JSON guard demotion in REDRESS with all affected
   guard rows, Track 1/Track 2 values, floor source, run id, material cause,
   and a same-wave gate consumer that understands the demotion.

A guard miss without REDRESS demotion fails the wave. A demotion must not be a
silent stale-results rewrite: the artifact needs the full four direct and seven
typed guard table, even if only one row misses, because P3-D rejects partial
JSON guard refreshes.

The current `verify-skv12-json-floors.awk` is a floor-hold verifier, not a
demotion consumer. If W4 chooses route 3, the plan must name the additional
gate/report consumer for the demotion or route final demotion rendering to W5.
If the regression is not recoverable inside W4, surface it immediately under
the campaign escalation rule.

## Non-Selected Orphan Demotion Evidence

SPEC Section 9 allows non-selected orphans to be
`inventory_demoted_with_evidence` only when the plan proves no behavior source
change is needed. "Not selected" is not evidence. Each row in
`restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md` needs:

- `orphan_status`: `consumed`, `removed`, `inventory_demoted`, or `open`;
- `consumer_path` for consumed rows, or demotion/removal artifact for others;
- `lock16_status`;
- `redress_entry`;
- source grep proving current production reachability;
- test/checkasm status for any semantic primitive;
- REDRESS adjacency and material-differential note.

Evidence commands:

```sh
rg -n "bitmap_prefix_xor_64|bitmap_next_set_bit|bulk_emit_positions_64|byte_context|cache_hints" \
  skinny/crates/bbnf-simd/src skinny/crates/bbnf-simd/tests \
  skinny/crates/parse-that-regex/src skinny/crates/runtime/src/grammars/css_l4_declaration_values

RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml \
  -p bbnf-simd --release --test checkasm_bitmap_prefix_xor_64
RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml \
  -p bbnf-simd --release --test checkasm_bitmap_next_set_bit
RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml \
  -p bbnf-simd --release --test checkasm_bulk_emit_positions_64
RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml \
  -p bbnf-simd --release --test aarch64_primitives

rg -n "pmull|vmull|ctz|rbit|clz|prfm|stnp|vextq_u8" skinny/crates/bbnf-simd/src
```

Recommended non-selected row dispositions, assuming W4 selects some other
primary ASM-gen candidate:

| Orphan | Current evidence | Demotion status if non-selected |
|---|---|---|
| `bitmap_prefix_xor_64` | aarch64 wrapper delegates directly to scalar; checkasm exists; REDRESS 88 rejected PMULL default body after JSON regressions. | `inventory_demoted`; `lock16_status=pass:scalar_delegate_checkasm:no_pmull_body`; cite REDRESS 88 and source grep. |
| `bitmap_next_set_bit` | aarch64 wrapper delegates directly to scalar; checkasm exists; REDRESS 89 rejected the CSSC CTZ bulk consumer. | `inventory_demoted`; `lock16_status=pass:scalar_delegate_checkasm:no_ctz_body`; cite REDRESS 89 and source grep. |
| `bulk_emit_positions_64` | aarch64 wrapper delegates to scalar and is called through `compact_mask`; checkasm exists; REDRESS 89 adjacency covers the rejected next-bit/bulk route. | `inventory_demoted`; `lock16_status=pass:scalar_delegate_checkasm:no_neon_body`; cite REDRESS 89 and current scalar delegation. |
| `byte_context` | aarch64 `vextq_u8` support helpers; no production caller found outside tests; no dedicated scalar/checkasm because no semantic primitive is admitted. | `inventory_demoted`; `lock16_status=n/a:inventory-support:no-production-consumer`; cite source grep and note W4 must change to `consumed` if C4/wide-string uses it. |
| `cache_hints` | PRFM/STNP helpers; no production caller found outside tests; SK-V11 hardening demoted PRFM/STNP to inventory-only without fresh row evidence. | `inventory_demoted`; `lock16_status=n/a:inventory-hint:no-production-consumer`; cite source grep, audit, and absence of a W4 hot-leaf consumer. |

If W4 selects one of these five as the primary route, that row cannot be
inventory-demoted in the same plan. It must become `consumed` with scalar
reference, strict checkasm/parity, microbench, same-wave CSS or JSON-guard
consumer, and measurement; or it must be reverted/removed and recorded as
`MEASURED-REJECT`.

Any orphan that still has a production caller but lacks scalar/checkasm,
same-wave consumer, or demotion/removal artifact remains `open`; `open` is
legal during W4 research/plan but fails ADMIT and FIXPOINT close.
