# AZ-IV.W5.4 — Post-W5 Substrate Denominator

The permanent substrate-audit test (`crates/ir/tests/substrate_audit.rs`)
enumerates every `pub` item (`fn`, `struct`, `enum`, `trait`, `static`)
in the audited crate set and counts production callers across the
workspace. Items with zero production callers fail the build unless
explicitly sanctioned.

## Current Surface (W5.4 close)

- **Audited files**: 514 (under the five audited crate roots)
- **Caller files**: 594 (every workspace member's `src/` tree)
- **Enumerated `pub` substrates**: 886
- **Zero-caller substrates**: 32 (full list in `W5-substrate-audit-pass.txt`)
- **Test elapsed**: ~1.4s (well within the 60s budget)

The 32 zero-caller findings are real: each is a fully-`pub` item whose
identifier appears only at its declaration site across all workspace
production code (i.e. excluding `tests/`, `examples/`, `benches/`,
`#[cfg(test)]` blocks, and the audited surface itself). They route to
either deletion, sanctioned whitelist entry, or a real production
caller landed in a follow-on cleanup pass.

Per the W5.4 dispatch's empty-return rule (more than ~5 zero-caller
substrates surfacing routes to follow-on cleanup), the test
infrastructure lands here and the cleanup of the 32 substrates is
a separate dispatch that closes Hard Gate 13's residual condition
("zero zero-caller substrates remain"). The audit machinery itself
is permanent and CI-gated; it will fire on every PR until the
substrate count reaches zero.

## Audited Crate Set

| Crate (Cargo `[package].name`) | Path | Spec alias |
|---|---|---|
| `bbnf-ir` | `crates/ir/` | bbnf-ir |
| `bbnf` | `crates/core/` (excl. `src/grammar/generated/**`) | bbnf-core |
| `egraph` | `crates/egraph/` | bbnf-egraph |
| `csp-solver` | `crates/csp-solver/` | bbnf-csp-solver |
| `simd-scan` | `crates/simd-scan/` | bbnf-simd-scan |

Generated files under `crates/core/src/grammar/generated/**` are
excluded from `pub`-item enumeration (they emit re-exports of the
audited surface, not new surface). They ARE scanned as callers.

## Caller Universe

Every workspace member's `src/` tree feeds the caller pass:

- `bbnf` (incl. generated/), `bbnf-analysis`, `bbnf-bootstrap`,
  `bbnf-gorgeous`, `bbnf-ir`, `bbnf-lsp`, `bbnf-ser`, `bbnf-path`
- `csp-solver`, `egraph`, `egraph-derive`, `simd-scan`
- `xtask`

Per the wave spec, `tests/`, `examples/`, `benches/`, and code
inside `#[cfg(test)]` (or `#[cfg(any(test, ...))]`) attributes are
excluded from caller counting at the directory and AST levels.

## Sanctioned Whitelist

Source of truth: `SANCTIONED_SUBSTRATES` in `crates/ir/tests/substrate_audit.rs`.

| Substrate | Reason |
|---|---|
| (none yet) | Whitelist is empty at W5.4 close; cleanup-pass dispatch decides per-item disposition. |

If the audit reveals a substrate that cannot route to a consumer or a
deletion (per the W5 dispatch's empty-return rule), it lands here with
a one-line reason — never as a naked allowance. New entries require a
W6 close-honesty review.

## Follow-On Cleanup Dispatch (Hard Gate 13 residual)

The 32 zero-caller substrates surfaced by the W5.4 close split into
three disposition categories. Triage hints below; final disposition is
the cleanup dispatch's call.

### Test/bench-only consumers (sanction or downgrade visibility)

These items are referenced ONLY from `tests/`/`benches/`/`examples/`
in their own crate. Either sanction with a one-line reason or restrict
visibility to `pub(crate)` + a `#[cfg(any(test, feature = "_internal"))]`
re-export.

| Substrate | File:line | Test/bench callers |
|---|---|---|
| `pack_lut_byte_for_test` | `crates/core/src/backend/rust/emitter/precedence.rs:272` | `crates/core/tests/pratt_const_fold.rs` (×6) |
| `ensure_dag` | `crates/ir/src/dag/mod.rs:50` | `crates/ir/tests/**` (~20 callers), `crates/core/benches/**` (×2) |
| `parse_with_ir` | `crates/ir/src/vm/interpreter/mod.rs:373` | `crates/ir/tests/vm/interpreter.rs` (~30 callers) |
| `min_conflicts` | `crates/csp-solver/src/solver/local_search.rs:106` | `crates/csp-solver/tests/local_search.rs` (×3) |
| `propagate_gac_alldiff` | `crates/csp-solver/src/solver/gac_alldiff.rs:209` | `crates/csp-solver/tests/gac.rs` (×2) |
| `compact_stripe_synthetic` | `crates/simd-scan/src/compaction.rs:53` | feature-gated callers |
| `compact_stripe_pext` | `crates/simd-scan/src/compaction.rs:82` | feature-gated callers |

### Internal helpers misclassified as `pub`

These items are constructed/called via methods or inline-function paths
that the AST-walk doesn't credit. Triage either restricts visibility to
`pub(super)` / `pub(crate)` or routes a missing caller.

- `cursor_generic_clause`, `cursor_arg` (backend/shapes/cursor_param)
- `phf_dispatch_fn_ident`, `phf_kw_table_ident`, `try_build_shared_table`,
  `emit_shared_table`, `shared_vocab_ident` (backend/regex/phf)
- `type_desc_to_syn`, `type_desc_is_span`, `type_is_span`
  (backend/rust/ir_types) — likely only called from `type_desc_to_syn_raw`
  recursively; need follow-up resolution
- `make_alphabet` (passes/recognizers/pattern_alphabet)
- `compute_inside_string_bytes` (simd-scan/parity)
- `propagate_stratified` (csp-solver/solver/monotonic)
- `compute_excluded_bytes`, `classify_rule_alphabet` (regex emit/simd, byte_class)
- `charset_from_class_body`, `emit_call_with_escapes` (backend/kernels)

### Genuinely dead (delete)

These items have no consumer of any kind (production, test, bench, or
example). Delete in the cleanup dispatch.

- `calculate_acyclic_deps_scc`, `calculate_non_acyclic_deps_scc`
  (`crates/core/src/graph/scc.rs:209`/`:242`)
- `SeqResultStrategy` (`crates/core/src/backend/types/mod.rs:61`)
- `resolve_ref_strategy`, `classify_seq`, `NodeStrategy`,
  `classify_repeat` (`crates/core/src/backend/strategy/**`)
- `generate_serialize_methods` (`crates/core/src/generate/serialize/mod.rs:15`)

(Disposition above is preliminary; the cleanup dispatch confirms each.)

## Known Misses (per-test docstring)

1. **Trait-method substrates**: a trait method named `foo` is matched
   by identifier — any caller that types `.foo(` against ANY type
   counts. Same-named methods on different traits are conflated.
2. **Generic-over-T substrates**: common method names like `map`,
   `iter`, `collect` are over-credited because the audit cannot
   distinguish between the audited surface's `map` and `Iterator::map`.
3. **Macro-emitted callers**: identifiers materialised only after macro
   expansion are seen IF the substrate name appears at the macro
   invocation site (the `.rs` source, pre-expansion). Names visible
   only post-expansion are missed.

These misses bias the audit toward over-counting (false-negatives on
zero-caller). The audit is therefore a permissive lower-bound on
substrate consumption, not a precise count. Tightening to AST-aware
resolution is post-AZ-IV scope.

## Performance Budget

The test runs in `< 60s` on the reference host. Parsing is
parallelised with `rayon`; `cargo metadata --no-deps` is invoked once
per test run.

## Hard Gate Citation

`docs/tranches/AZ-IV/AZ-IV.md` §Hard Gate 13:

> Permanent substrate-audit test: `crates/ir/src/passes/tests/
> substrate_audit.rs` enumerates every `pub` substrate at compile time
> and fails the build if any has zero callers in production code
> (excluding `tests/`, `examples/`, `#[cfg(test)]`). CI-gated.

The test file lives at `crates/ir/tests/substrate_audit.rs` (not the
`src/passes/tests/` path written in the wave spec) per the standing
`feedback_no-inline-tests` rule that all tests live under `tests/`,
never inline `#[cfg(test)]` in `src/`. The Hard Gate condition is
unaffected — enumeration + zero-caller failure + CI-gating all hold
through the existing `cargo nextest run --workspace --profile ci`
step in `.github/workflows/ci.yml`.
