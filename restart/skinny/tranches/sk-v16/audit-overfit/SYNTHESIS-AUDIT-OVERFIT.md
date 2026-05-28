# SK-V16 S-P0 Overfit Audit Synthesis

Date: 2026-05-28.
HEAD: `fc16919d4`.
Status: converged after V1/V2 CH1-CH7 hardening.

## Verdict

S-P0 audit execution is complete. The audit does not permit behavior waves yet.

| Axis | Verdict |
|---|---|
| A1 Measurement integrity | ACCEPT |
| A2 Admit-mechanism integrity | ACCEPT |
| A3 Lock 14 generic-crate scan | FAIL-A3-TARGET |
| A4 Generator-vs-hand-curated | REVISE / PRUNE REQUIRED |
| A5 Decision-engine fold integrity | ACCEPT WITH PRUNE CANDIDATES |
| A6 Pattern recurrence | REVISE / BLOCK A6 CLEAN |

Overall disposition: PRUNE-BLOCKED. S-P1/S-P2/S-P3 may proceed only to profile,
research, and author prune-first waves. SK-V16 behavior/admission waves are
blocked until the critical/high prune list is expressed in the legal wave graph
and consumed by executable gates.

## CH4 Cost Contract

Every prune item consumed by S-P3 must declare:

- owner paths;
- manual source/test LOC budget;
- generated-output status;
- docs/ledger LOC budget;
- phase hard cap;
- split trigger;
- same-commit consumer or executable gate;
- split-prone flag;
- allowed over-cap disposition: split inside the legal wave graph, row-level
  intrinsic block, or gate-routed amendment before redress.

Generated/codegen and Pattern H work is split-prone by default. Generated
output cannot hide manual implementation scope.

## Binding Prune List

### P0-1 Lock 14 Generic Codegen Leak

`skinny/crates/codegen/src/runtime_generator.rs`, `json_*`,
`json_templates`, CSS request-facts code, and `xtask` remain grammar-name
routers or grammar-specific emitters in nominally generic surfaces. Expand
Lock 14 token scanning to include these surfaces or quarantine them from SK-V16
close authority. Replace JSON/CSS constants with grammar-derived provider and
template input before claiming generic close.

S-P3 must split this into separate rows:

| Row | Scope | Split-prone |
|---|---|---|
| P0-1a | CSS request-facts/provider quarantine and replacement | yes |
| P0-1b | JSON parse-only/template quarantine or grammar-derived ownership | yes |
| P0-1c | Lock 14 scan expansion over omitted roots | no |
| P0-1d | `xtask` manifest-driven target ownership or historical quarantine | yes |

### P0-2 CSS Legacy Proof Path

`CSS_GENERATED_RS`, `emit_fact_stream`, `CssFullParseSummary`, `parse_full`,
fact-stream output planes, brace/delimiter summaries, `input_fnv64`, and
`stream_fnv64` remain live code paths but cannot be CSS admission proof.
Wrong-plane comparator admission, cross-plane comparator evidence,
`sidecar-same-run`, `same-plane-source-sidecar`, `sidecar_freshness`,
`historical:*`, and `stale:*` are diagnostic-only/non-admission proof unless a
future SK-V16 typed report gate proves same-workload typed equality and
structured freshness. SK-V16 must land a grammar-derived typed CSS provider and
a typed cssparser same-workload equality gate before any CSS >SOTA row can
admit.

### P0-3 Dirty Generated State

The seven dirty skinny CSS L4 `generated.rs` files and
`skinny/crates/bbnf-bench/src/generated_real_typed.rs` block broad generated
close proof. They must be retired, regenerated, or intrinsically blocked with
exact manifest, broad command result, and owner/disposition per file.

S-P3 must split this into:

- P0-3a: seven skinny CSS L4 generated files, owner `skinny runtime CSS request
  facts`, broad command `cargo test --manifest-path skinny/Cargo.toml -p codegen
  tests::css_l4_generated_runtimes_reproducible_from_request -- --exact`,
  current result `FAILED: DifferentFile("generated.rs")`.
- P0-3b: `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, owner
  `skinny real-typed bench generation`, broad command `(cd skinny && cargo xtask
  check-real-typed)`, current result `FAILED: generated_real_typed.rs differs`.

Exact dirty generated manifest:

```text
M skinny/crates/bbnf-bench/src/generated_real_typed.rs
M skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs
M skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs
M skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs
M skinny/crates/runtime/src/grammars/css_l4_nested_layout/generated.rs
M skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs
M skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs
M skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs
```

### P0-4 Pattern H Collapse

Pattern H remains provenance-only. The root runtime count is still 67 files; the
close requirement is generator-owned grammar-id template collapse with
byte-equivalent round-trip proof. Header-only provenance is not enough.

S-P3 must split this into separate rows:

- template design and grammar-id parameterization;
- `css_l4` projection source binding;
- 67-file census and round-trip gate.

### P0-5 Generic Backend Grammar Markers

Shared backend output leaks JSON path markers into non-JSON generated output and
core lowering imports BBNF runtime shapes directly. S-P3 must either bind
grammar-neutral fixes or record intrinsic bootstrap blocks with executable
proof.

### P0-6 Decision Engine Hardening

The current Decision Engine proof remains valid, but SK-V16 should add an
adversarial CSP fixture for egraph-preferred invalid selection and require
non-SinkOnly lowerers to carry policy/union facts into emitted runtime-plan
artifacts before future admission depends on those paths.

### P0-7 FNV And x86 Guard

FNV remains bench/diagnostic metadata only. CSS typed proof must not use hash
sidecars as semantic proof. Native SIMD remains Apple M5 Max / aarch64 only;
SK-V16 SIMD gates must prove no x86/AVX code or evidence was touched.

## Required SK-V16 Report Consumers

The report flags below are not implemented proof. S-P3 must author or name the
consumers before any wave can use these report classes as close evidence:

```sh
(cd skinny && cargo xtask gate-json --check-results --skv16-css-typed-report <path>)
(cd skinny && cargo xtask gate-json --check-results --skv16-dirty-generated-report <path>)
(cd skinny && cargo xtask gate-json --check-results --skv16-pattern-h-roundtrip-report <path>)
(cd skinny && cargo xtask gate-json --check-results --skv16-native-simd-report <path>) # only if SIMD scoped
```

## Executable Evidence

Commands run locally during S-P0:

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
# 16

find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
# 67

rg -c '\| json/.+\| .*\| ADMITTED \|' restart/skinny/ROLLING-SOTA-DELTA.md
# 51

rg -c '\| css_l4/.+\| .*\| OPEN \|' restart/skinny/ROLLING-SOTA-DELTA.md
# 24

cargo test --manifest-path skinny/Cargo.toml -p codegen \
  tests::css_l4_generated_runtimes_reproducible_from_request -- --exact
# FAILED: DifferentFile("generated.rs")

(cd skinny && cargo xtask check-real-typed)
# FAILED: generated_real_typed.rs differs

cargo xtask check-runtime
# PASS

cargo xtask regen --check --grammar css_l4
# PASS: clean (9 of 9 grammars matched)

cargo test --manifest-path skinny/Cargo.toml -p passes decision_ -- --nocapture
# 3 passed

cargo test --manifest-path skinny/Cargo.toml -p codegen lower_ -- --nocapture
# 5 passed

cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench \
  fnv_quarantine::tests:: -- --nocapture
# 3 passed

git diff --cached --name-status
# empty

git diff -- . | rg -n -- 'x86|x86_64|AVX|avx|_mm|target_feature' || true
# no current S-P0 diff matches

rg -n -- 'x86|x86_64|AVX|avx|_mm|target_feature' restart/skinny/tranches/sk-v16/audit-overfit/*.md
# hits are guard/prune text only; no implementation/evidence path is newly touched
```

## Next

S-P0 is converged and feeds S-P1. S-P1 must profile the prune list and
S-P2/S-P3 must author a prune-first SK-V16 wave program. No CSS SOTA,
generated, Pattern H, or native SIMD behavior wave may open ahead of the prune
gates.
