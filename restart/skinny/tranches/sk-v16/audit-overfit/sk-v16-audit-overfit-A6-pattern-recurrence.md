# SK-V16 S-P0 A6 - Pattern Recurrence

Date: 2026-05-28.
HEAD: `fc16919d4`.
Axis: A6 pre-restart pattern recurrence.
Disposition: REVISE / BLOCK A6 CLEAN.

## Critical Findings

### A6-C1 - CSS legacy proof path remains live in codegen/runtime

`RequestFacts` emits `generated.rs` from `CSS_GENERATED_RS`, not from a typed
grammar-derived CSS provider. The template contains fact-stream output,
`CssFullParseSummary`, `parse_full`, delimiter summary parsing, and FNV metadata.
Seven CSS targets still route through this request-facts path.

Required prune: remove or quarantine `CSS_GENERATED_RS` from any SK-V16
close/admission path. `--skv16-css-typed-report` must reject evidence containing
`CSS_GENERATED_RS`, `emit_fact_stream`, `CssFullParseSummary`, `parse_full`,
`css_l4_*_fact_stream`, `input_fnv64`, `stream_fnv64`, wrong-plane comparator
admission, cross-plane comparator evidence, `sidecar-same-run`,
`same-plane-source-sidecar`, `sidecar_freshness`, `historical:*`, or `stale:*`
as live proof.

Representative scan:

```sh
rg -n 'CSS_GENERATED_RS|CssFullParseSummary|emit_fact_stream|parse_full|input_fnv64|stream_fnv64|wrong[-_]plane|cross-plane|sidecar-same-run|same-plane-source-sidecar|sidecar_freshness|historical:|stale' \
  skinny/crates/codegen/src/runtime_generator.rs \
  skinny/crates/runtime/src/grammars/css_l4_*/*.rs \
  skinny/crates/bbnf-bench/src/gate.rs \
  skinny/crates/bbnf-bench/src/css_l4_w8.rs \
  skinny/crates/bbnf-bench/src/bin/gate.rs \
  skinny/crates/bbnf-bench/src/report.rs \
  skinny/crates/bbnf-bench/src/nonjson_css_l4.rs
```

### A6-C2 - Pattern H remains provenance-only, not collapsed

The root runtime still has 67 grammar-specific files and top-level per-grammar
directories. PASS-IMPL V2 accepted provenance only and routed full collapse.
Header-only provenance or retaining 67 files without generator-owned grammar-id
template collapse is not A6 clean.

## High Findings

### A6-H1 - Shared backend emits JSON path marker into non-JSON output

Generic backend output still leaks `path::markers::Json` into generated
non-JSON grammars. S-P3 must parameterize the eager empty path marker by grammar
or make it grammar-neutral.

Representative scan:

```sh
rg -n 'path::markers::Json|TypedPath<markers::Json>' \
  crates/core/src/backend \
  crates/core/src/grammar/generated
```

### A6-H2 - Core lowering is BBNF-runtime-shaped

Core lowering imports `BbnfView`, `BbnfKind`, and `BbnfCompoundKind` directly in
shared lowering code. Either scope this as intrinsic bootstrap-only with proof or
route it through a grammar-neutral view contract before claiming A6 clean.

Representative scan:

```sh
rg -n 'BbnfView|BbnfKind|BbnfCompoundKind' crates/core/src/lower
```

### A6-H3 - Dirty generated state remains a close risk

Current `git status --short` still contains seven dirty skinny CSS generated
runtime files and `skinny/crates/bbnf-bench/src/generated_real_typed.rs`.
SK-V16 requires exact manifest, broad command result, and owner/disposition
before generated/codegen close proof can count.

## Medium Findings

- FNV is quarantined on the bench side, but CSS generated output still emits FNV
  metadata. This must remain diagnostic-only.
- x86/AVX paths exist under an SK-V16 aarch64-only pin. Native SIMD proof must
  prove `x86_touched=false` and cite no x86/AVX evidence.

Current S-P0 diff did not touch x86/AVX evidence:

```sh
git diff --cached --name-status
# empty

git diff -- . | rg -n -- 'x86|x86_64|AVX|avx|_mm|target_feature' || true
# no current S-P0 diff matches

rg -n -- 'x86|x86_64|AVX|avx|_mm|target_feature' restart/skinny/tranches/sk-v16/audit-overfit/*.md
# hits are guard/prune text only; no implementation/evidence path is newly touched
```

## Prune Candidates

1. Remove `CSS_GENERATED_RS` from live codegen admission surfaces or make it
   diagnostic-only with hard gate rejection.
2. Require CSS typed provider from `grammar/css/l4/*.bbnf`, typed summary
   equality vs cssparser, and same-workload speed before any CSS SOTA claim.
3. Collapse Pattern H with generator-owned grammar-id templates and
   byte-equivalent regen evidence.
4. Fix generic backend `Json` eager-path marker leakage.
5. Classify BBNF-shaped core lowering as intrinsic bootstrap-only or route it
   through a grammar-neutral view contract.
6. Enforce dirty generated manifest and broad-check disposition before any
   generated/codegen close claim.
7. Keep FNV and x86/AVX out of SK-V16 production/admission evidence.

CH4 split note: A6 prune work is split-prone. S-P3 must separate CSS legacy
proof removal, Pattern H template collapse, JSON marker leakage, BBNF-shaped
lowering classification, dirty generated disposition, FNV guard, and x86/AVX
guard into individually budgeted rows.
