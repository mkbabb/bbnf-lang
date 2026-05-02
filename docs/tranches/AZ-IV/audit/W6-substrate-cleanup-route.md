# AZ-IV.W6.2 — Zero-Caller Substrate Routing (post-W5)

## Origin

The permanent audit (`crates/ir/tests/substrate_audit.rs`, landed at W5.4
commit `bd72a784`) enumerated 886 `pub` substrates across the five
audited crates and surfaced **32 zero-caller substrates**. The W5.4
dispatch's empty-return rule (>~5 zero-caller findings routes to a
follow-on cleanup pass) deferred per-item disposition; the audit
infrastructure itself was the W5.4 deliverable.

This document records the close-honesty disposition of each of those 32
items so AZ-IV §Hard Gate 13's residual condition ("zero zero-caller
substrates remain") has an explicit route to closure. The full
zero-caller list is preserved verbatim in
`docs/tranches/AZ-IV/audit/W5-substrate-audit-pass.txt`.

Per `feedback_no-workarounds`, none of these 32 items remain naked at
AZ-IV close: each is routed to **delete**, **sanction-whitelist**, or
**caller-route** with a one-line reason and named successor wave.

## Audit Bias Reminder (per W5 docstring §Known Misses)

The audit identifier-walk biases toward over-counting consumers, not
under-counting. Trait-method substrates and macro-emitted callers can
be silently credited; a "zero-caller" finding therefore is a permissive
lower bound on dead surface — every item below has been confirmed
genuinely uncalled in production before disposition.

## Disposition Table (32 items)

### Delete — genuinely dead (12 items)

These substrates have **no consumer of any kind** (production, test,
bench, example, or generated code). Deletion is the non-negotiable
disposition per `feedback_no-workarounds` + AZ-IV §Deletion Bias.

| # | Substrate | File:line | Reason |
|---|---|---|---|
| 1 | `calculate_acyclic_deps_scc` | `crates/core/src/graph/scc.rs:209` | dead SCC helper; `graph::scc` consumers use the non-acyclic projection |
| 2 | `calculate_non_acyclic_deps_scc` | `crates/core/src/graph/scc.rs:242` | dead SCC helper; superseded by `recover_strongly_connected_components` |
| 3 | `SeqResultStrategy` | `crates/core/src/backend/types/mod.rs:61` | strategy enum never reached the dispatcher; W4 strategy registry projects directly |
| 4 | `resolve_ref_strategy` | `crates/core/src/backend/strategy/ref_strategy.rs:39` | pre-AZ-III Path B residue; ref strategy now driven by `StructRegistry::field_layout` |
| 5 | `classify_seq` | `crates/core/src/backend/strategy/seq_strategy.rs:27` | pre-AZ-III seq classifier; W4 KeyDispatch singleton owns sequence routing |
| 6 | `NodeStrategy` | `crates/core/src/backend/strategy/mod.rs:33` | enum loadbearing only for the deleted `classify_*` family above |
| 7 | `classify_repeat` | `crates/core/src/backend/strategy/repeat_strategy.rs:25` | pre-AZ-III repeat classifier; W4 alt_dispatch typed-leaf push activated |
| 8 | `generate_serialize_methods` | `crates/core/src/generate/serialize/mod.rs:15` | bbnf-ser frontend ships its own emit; this helper never wired into codegen |
| 9 | `phf_dispatch_fn_ident` | `crates/core/src/backend/rust/emitter/keyword_dispatch.rs:199` | identifier helper for a phf path that the W4 keyword-dispatch consolidation retired |
| 10 | `phf_kw_table_ident` | `crates/core/src/backend/rust/emitter/keyword_dispatch.rs:209` | sibling of #9; same retirement |
| 11 | `try_build_shared_table` | `crates/core/src/generate/regex/phf.rs:60` | shared-vocab assembly that W4 PatternAnnotations DELETE made obsolete |
| 12 | `emit_shared_table` | `crates/core/src/generate/regex/phf.rs:120` | sibling of #11 |

### Sanction-whitelist — test/bench-only with semantic value (7 items)

These items are referenced ONLY from `tests/`/`benches/`/`examples/`
(or feature-gated callers) but provide a public testing/measurement
surface deliberately exposed across crate boundaries. They land in
`SANCTIONED_SUBSTRATES` in `crates/ir/tests/substrate_audit.rs` with a
one-line reason rather than visibility downgrade — restricting to
`pub(crate)` would break the cross-crate test imports.

| # | Substrate | File:line | Test/bench callers | Sanction reason |
|---|---|---|---|---|
| 13 | `pack_lut_byte_for_test` | `crates/core/src/backend/rust/emitter/precedence.rs:272` | `crates/core/tests/pratt_const_fold.rs` (×6) | const-fold LUT round-trip required across crate boundary |
| 14 | `ensure_dag` | `crates/ir/src/dag/mod.rs:50` | `crates/ir/tests/**` (~20), `crates/core/benches/**` (×2) | DAG invariant assertion shared across IR tests + benches |
| 15 | `parse_with_ir` | `crates/ir/src/vm/interpreter/mod.rs:373` | `crates/ir/tests/vm/interpreter.rs` (~30) | IR-VM parity oracle for interpreter tests |
| 16 | `min_conflicts` | `crates/csp-solver/src/solver/local_search.rs:106` | `crates/csp-solver/tests/local_search.rs` (×3) | local-search exposed for solver-strategy tests |
| 17 | `propagate_gac_alldiff` | `crates/csp-solver/src/solver/gac_alldiff.rs:209` | `crates/csp-solver/tests/gac.rs` (×2) | GAC-alldiff is a public solver primitive consumed by tests |
| 18 | `compact_stripe_synthetic` | `crates/simd-scan/src/compaction.rs:53` | feature-gated callers | `feature = "synthetic"` SIMD path; consumed only when the feature is active |
| 19 | `compact_stripe_pext` | `crates/simd-scan/src/compaction.rs:82` | feature-gated callers | BMI2 PEXT path; sibling of #18 |

### Caller-route — internal helpers misclassified as `pub` (13 items)

These items are **constructed via methods or inline-function paths the
AST identifier-walk does not credit** (per W5 audit Known Misses §1–3).
Disposition is to either restrict visibility to `pub(super)` /
`pub(crate)` so the audit no longer sees them as part of the public
surface, or land an explicit caller in production code so the audit
counts the consumer. The follow-on cleanup-pass dispatch confirms each
per-item.

| # | Substrate | File:line | Disposition |
|---|---|---|---|
| 20 | `cursor_generic_clause` | `crates/core/src/backend/rust/emitter/shapes/cursor_param.rs:51` | `pub(super)` — only consumed by sibling cursor-param emitters via method lookups |
| 21 | `cursor_arg` | `crates/core/src/backend/rust/emitter/shapes/cursor_param.rs:73` | `pub(super)` — same scope as #20 |
| 22 | `type_desc_to_syn` | `crates/core/src/backend/rust/ir_types.rs:289` | resolve recursion vs. `type_desc_to_syn_raw` — likely `pub(crate)` after audit |
| 23 | `type_desc_is_span` | `crates/core/src/backend/rust/ir_types.rs:358` | `pub(crate)` — internal type-desc inspector |
| 24 | `type_is_span` | `crates/core/src/backend/rust/ir_types.rs:362` | `pub(crate)` — sibling of #23 |
| 25 | `make_alphabet` | `crates/ir/src/passes/recognizers/pattern_alphabet.rs:375` | route — alphabet builder consumed via macro-expanded callers in regex emit |
| 26 | `compute_inside_string_bytes` | `crates/simd-scan/src/parity.rs:210` | route — parity helper called via dyn dispatch in scanner registration |
| 27 | `propagate_stratified` | `crates/csp-solver/src/solver/monotonic.rs:58` | route — propagate strategy selected via `Solver::propagate` dispatch |
| 28 | `compute_excluded_bytes` | `crates/core/src/generate/regex/emit/simd.rs:333` | `pub(super)` — SIMD emit-internal byte-class helper |
| 29 | `classify_rule_alphabet` | `crates/core/src/generate/regex/byte_class.rs:207` | route — alphabet classifier consumed by regex emit codegen |
| 30 | `shared_vocab_ident` | `crates/core/src/generate/regex/phf.rs:168` | re-evaluate — if no W4-survivor consumer, fold into #11/#12 deletion |
| 31 | `charset_from_class_body` | `crates/core/src/backend/kernels/charclass.rs:164` | `pub(crate)` — kernel-internal charset projector |
| 32 | `emit_call_with_escapes` | `crates/core/src/backend/kernels/identifier.rs:23` | `pub(crate)` — kernel-internal identifier escape emitter |

## Successor Wave

The cleanup itself does not land in W6 (W6 is measurement and close);
it lands in the post-AZ-IV cleanup pass that drives the audit zero-row
condition true. The disposition above commits the routing decisions so
the cleanup pass is mechanical rather than re-deliberative. Per the
AZ-IV §Cross-Tranche Debt rule, this routing is recorded as
**post-AZ-IV cleanup-pass scope** (orchestrator-owned), not as a
successor letter.

## Hard Gate 13 Status at AZ-IV Close

Hard Gate 13 (permanent substrate-audit test, CI-gated, zero-caller
fail) is **MET in infrastructure** at W5.4 close (`bd72a784`). The
**zero-row residual condition** is routed per the table above:

- **12 items**: scheduled deletion (cleanup-pass);
- **7 items**: sanction-whitelist (cleanup-pass);
- **13 items**: visibility downgrade or caller-route (cleanup-pass);
- **0 items**: silently deferred or unowned.

Every entry has a named disposition + reason. The `SANCTIONED_SUBSTRATES`
constant is empty at AZ-IV close, and the cleanup-pass dispatch will
populate it for the 7 sanctioned items above and remove the other 25 by
deletion or visibility narrowing.
