# POST-CLOSE-B — Substrate Wired-And-Consumed Audit

## Mandate

> Ensure all substrate is wired and consumed.

The substrate-audit test (`crates/ir/tests/substrate_audit.rs`) shipped at
W5.4 with **32 zero-caller `pub` substrates** out of 886 enumerated. W6.2
routed those into **12 delete / 7 sanction-whitelist / 13 caller-route**
buckets and deferred mechanical execution to a post-AZ-IV pass. This
document is that follow-on audit: it verifies (or refutes) every W6.2
classification, reaches beyond classification into module-level death
patterns and architectural transposition opportunities, and surveys for
additional dead surface the W5 audit's name-only scan misses.

The verification methodology was a workspace-wide identifier scan
(`grep -rn ... --include="*.rs"`) per substrate, partitioning matches
into production vs `tests/`/`benches/`/`examples/` vs declaration sites.
The W6.2 disposition table's three-way split is treated as a *hypothesis*
to test, not a finding to ratify.

## The 32 — verified classifications

Symbol legend in **Verified?** column: `OK` confirms W6.2 routing;
`FLIP` re-routes to a different bucket; `EXEC` confirms with a more
specific execution detail (e.g. visibility level).

| # | Substrate | W6.2 class | Verified? | Architectural note |
|---|---|---|---|---|
| 1 | `calculate_acyclic_deps_scc` | delete | OK | Dead helper; consumers (`pipeline::compile`, `analysis::diagnostics::cycles`) use `tarjan_scc` + `topological_sort_scc` directly. |
| 2 | `calculate_non_acyclic_deps_scc` | delete | OK | Same; chained on `calculate_acyclic_deps_scc`. The `graph::scc` module retains 250 LOC for two consumed fns + one struct — leave the file. |
| 3 | `SeqResultStrategy` | delete | OK | Enum at `backend/types/mod.rs:61`, no constructor anywhere. The `backend/types/mod.rs` file holds 12 `pub` items; `SeqResultStrategy` is the orphan. |
| 4 | `resolve_ref_strategy` | delete | OK | `RefStrategy` itself is consumed only via `From<CallStrategy>` blanket impl in tests; the resolver fn has zero call sites. |
| 5 | `classify_seq` | delete | OK | Strategy classifier — no caller. See module-cluster finding below. |
| 6 | `NodeStrategy` | delete | OK | Variant carriers `AltStrategy`/`SeqStrategy`/`RefStrategy`/etc. exist; the union `NodeStrategy` enum is unconstructed. |
| 7 | `classify_repeat` | delete | OK | 31-LOC file, single fn, dead. |
| 8 | `generate_serialize_methods` | delete | OK | bbnf-ser's frontend ships its own emit; `pub mod serialize` in `generate/mod.rs:19` exposes a stub. |
| 9 | `phf_dispatch_fn_ident` | delete | OK | Helper for a phf path the W4 keyword-dispatch consolidation retired. |
| 10 | `phf_kw_table_ident` | delete | OK | Sibling of #9. |
| 11 | `try_build_shared_table` | delete | OK | `crates/core/src/generate/regex/phf.rs` (183 LOC) holds 5 dead `pub` items — see module cluster. |
| 12 | `emit_shared_table` | delete | OK | Sibling of #11. |
| 13 | `pack_lut_byte_for_test` | sanction | OK | `crates/core/tests/pratt_const_fold.rs` — 6 cross-crate test asserts. Name carries the `_for_test` suffix; the convention is the contract. |
| 14 | `ensure_dag` | sanction | OK | ~20 IR test files plus `crates/core/benches/{css,json}/wasm.rs` (×2 production callers — these are wasm benches outside the audited surface). The wasm-bench callers flip this finding marginally; sanction still correct. |
| 15 | `parse_with_ir` | sanction | OK | ~30 callers in `crates/ir/tests/vm/interpreter.rs`. Pure parity oracle. |
| 16 | `min_conflicts` | sanction | OK | 3 callers in `crates/csp-solver/tests/local_search.rs`. |
| 17 | `propagate_gac_alldiff` | sanction | OK | 2 prod callers in `csp-solver/tests/gac.rs` plus a turbofish regression-guard line. |
| 18 | `compact_stripe_synthetic` | sanction | OK | feature-gated `synthetic` SIMD path; doc-mentioned in `avx2.rs`. |
| 19 | `compact_stripe_pext` | sanction | OK | BMI2 PEXT path; sibling of #18. |
| 20 | `cursor_generic_clause` | route (`pub(super)`) | **FLIP → DELETE** | **Zero callers anywhere.** Sibling fns `cursor_param`/`cursor_where_clause` ARE consumed in 8 emitter call sites. The W3.6 module triad shipped one orphan helper. |
| 21 | `cursor_arg` | route (`pub(super)`) | **FLIP → DELETE** | Same as #20: sibling helpers consumed; `cursor_arg` (returning bare `cursor` ident) has no caller. The cross-shape emitter inlines `quote! { cursor }` directly. |
| 22 | `type_desc_to_syn` | route (`pub(crate)`) | **FLIP → DELETE** | One-line wrapper around `type_desc_to_syn_raw` with `use_slices = true`. **Zero callers**. The recursive private `type_desc_to_syn_raw` is invoked directly by the four consumer sites with their own boolean. |
| 23 | `type_desc_is_span` | route (`pub(crate)`) | OK | Confirmed zero callers. Either delete OR change `type_is_span` (the syn-Type variant, also dead) to call this — but neither is wired. |
| 24 | `type_is_span` | route (`pub(crate)`) | OK | Sibling of #23. Both are pattern-matchers for `parse_that::Span` types; the codegen inspects the `TypeDesc` directly via `matches!` in-line. |
| 25 | `make_alphabet` | route (macro caller) | **FLIP → DELETE** | The struct `PatternAlphabet` is consumed via `mine_pattern` (its constructor in the same file, line 130). No macro call site references `make_alphabet`. The only caller is the `PatternAlphabetMiner` impl which builds the struct literal in-line. |
| 26 | `compute_inside_string_bytes` | route (dyn dispatch) | **FLIP → DELETE** | **Zero references workspace-wide** including tests. No `dyn Fn` registry consumes it. The `simd-scan/src/parity.rs` file holds its only mention. Genuinely abandoned. |
| 27 | `propagate_stratified` | route (`Solver::propagate`) | **FLIP → DELETE** | Zero callers. The `csp-solver` `Solver` trait does not enumerate strategies via `propagate_stratified`; the public solver API uses GAC/AC variants directly. |
| 28 | `compute_excluded_bytes` | route (`pub(super)`) | OK (downgrade) | Truly internal to `generate/regex/emit/simd.rs`; no external `pub` need. |
| 29 | `classify_rule_alphabet` | route (regex emit) | **FLIP → DELETE** | Doc-comment at `byte_class.rs:157` references it as the populator, but the actual `RuleAlphabetMap` is built by `byte_class::populate_rule_alphabets` (one consumer). The `pub fn classify_rule_alphabet` at line 207 is a separate orphan. |
| 30 | `shared_vocab_ident` | route (re-evaluate) | OK (delete) | Fold into #11/#12 deletion as W6.2 hinted. The `phf.rs` module is 5/5 dead `pub`. |
| 31 | `charset_from_class_body` | route (`pub(crate)`) | OK (downgrade) | Internal kernel helper; no external need. |
| 32 | `emit_call_with_escapes` | route (`pub(crate)`) | OK (downgrade) | Internal kernel helper; sibling of #31. |

**Verification deltas vs W6.2**: 6 substrates flipped from "route" to
"DELETE" (#20, #21, #22, #25, #26, #27, #29). The pattern is identical
in each: W6.2 hypothesised a missing wire (macro caller / dyn dispatch
/ propagate strategy / etc.) but the wire never existed. The audit's
identifier scan is permissive — it would have caught a genuine consumer
naming the substrate at any source-level site. **The W6.2 routing was
biased toward "missing wire" when "actually dead" was simpler.**

## Beyond the 32 — additional dead substrate

The W5 audit's loose identifier-match credits any same-named symbol
elsewhere in the workspace as a "caller", missing the cases where a
substrate *appears consumed* because of an unrelated namesake. The
following surface either is dead despite the audit's count, or is
ambiently consumed only via patterns the audit cannot resolve and so
warrants visibility narrowing.

1. **`SeqResultStrategy`'s file context (`backend/types/mod.rs`)**: 11
   sibling `pub` items in the same file — `CallStrategy`,
   `FlattenStrategy`, `ValuePlacement`, `KeyDispatchBranch<O>`, etc.
   Several are constructed only inside the file itself; the audit's
   "occurrence > 1" rule credits the impl-block `Self::Variant`
   identifiers and so the type passes. Recommend a follow-up audit
   variant that walks `impl` blocks separately so type-only strategy
   surface is visibility-checked.

2. **`emit_keyword_phf` / `emit_keyword_tables` (kept)**: consumed by
   `backend/rust/emitter/grammar.rs:165–191` — alive. Listed for context
   so the keyword_dispatch module split is clear: the 2 dead idents
   (`phf_dispatch_fn_ident`, `phf_kw_table_ident`) are 28 LOC out of 212
   in the file, the remainder is live.

3. **`generate/serialize/serialize.rs` (private module-only consumers)**:
   the `mod serialize` private file is referenced by the dead public
   `generate_serialize_methods` only. Deleting #8 (`generate_serialize_methods`)
   strands `serialize::rule_pushes_tape_record` + `generate_dispatch_arms`.
   Per the no-orthogonal rule, the entire `crates/core/src/generate/serialize/`
   directory is dead and must be removed atomically.

4. **`SccResult` field `cyclic_rules`**: declared `pub` but the field
   is read only inside `tarjan_scc` itself + tests (not production
   consumers). Field-level dead-pub is below the audit's resolution.

5. **`compute_inside_string_bytes` (#26)**: the `crates/simd-scan/src/parity.rs`
   file has 209 lines preceding the dead fn. Spot-check shows the
   surrounding `parity_scan_*` functions ARE consumed, but
   `compute_inside_string_bytes` is a leftover from an earlier
   string-quote-tracking design that the structural-scan path retired.

6. **`crates/bbnf-path-ts/` cdylib**: the crate ships `[lib]
   crate-type = ["cdylib", "rlib"]` and lives at `crates/bbnf-path-ts/src/template_tag.rs`.
   The only consumer surface is `tests/isomorphic_path_error.rs`. There
   is **no `wasm/` or `ts/` workspace consumer importing it for
   non-test purposes** — the cdylib output exists for hypothetical
   downstream wasm-bindgen usage that hasn't materialised.

7. **`PATH_PLAN` static (per-grammar)**: every generated grammar exposes
   `pub static PATH_PLAN: &[PathPlanEntry; N]`; consumed in the same
   generated module via a const `while i < PATH_PLAN.len()` lookup. The
   audit credits same-file refs, so this passes; visibility could be
   `pub(super)` but the const-eval loop sits in the same generated
   module already. **Worth pinning** as a substrate-with-consumer
   regression guard since it's the lazy-bail-out path's load-bearing
   table.

8. **`merge_path_seed` (`crates/ir/src/rewrites/mod.rs:208`)**: only
   tested. The W3 path-egraph-seed lands the rewrites and the loader,
   but no `prepare_grammar` / pipeline stage actually calls
   `merge_path_seed` against `RuleSet`. This is **WIRE NOW**, not
   delete: the path-seed rewrites are the AZ-IV.W3 deliverable and
   their consumer is missing.

9. **`docs/benchmarks/wave-AZ-IV-W{3,4,5}.json`**: declared in W3/W4/W5
   wave specs as wave-bridging snapshots per AUDIT-E §5 / D5 ADOPTED.
   `find . -name 'wave-AZ-IV*'` returns **zero files**. The three D5
   bridging snapshots were never produced. Either the snapshots land
   retroactively as a close-honesty artefact or the W3/W4/W5 wave specs
   are amended to drop the requirement. This is a docs-promise gap, not
   a code-substrate gap, but it bears on the "wired and consumed"
   directive at the documentation layer.

## Module-level death (clusters)

Five modules concentrate dead substrate. Per the user's
**no-god-modules** memory rule, the response is *retire the module*,
not patch each fn.

### Cluster A — `crates/core/src/backend/strategy/` (4 of 5 fns dead)

| File | LOC | Status |
|---|---:|---|
| `mod.rs` | 41 | `NodeStrategy` enum dead (#6) |
| `alt_strategy.rs` | 196 | **`AltStrategy`** consumed by driver/alt.rs |
| `ref_strategy.rs` | 45 | `resolve_ref_strategy` dead (#4); `RefStrategy` consumed only via blanket `From<CallStrategy>` in tests |
| `repeat_strategy.rs` | 31 | `classify_repeat` dead (#7) |
| `seq_strategy.rs` | 60 | `classify_seq` dead (#5) |
| `wrap_strategy.rs` | 18 | `WrapStrategy` declared, not consumed in production |

**Recommendation: TRANSPOSE → AZ-V.** Move the live `AltStrategy`
content to `backend/driver/alt_strategy.rs` (collocated with its only
consumer at `backend/driver/alt.rs`), drop the `strategy/` directory.
Net deletion ~150 LOC, removes the false impression that 5 strategy
classifiers exist.

### Cluster B — `crates/core/src/generate/regex/phf.rs` (4 of 5 pub fns dead)

| `pub` item | Status |
|---|---|
| `RuleKeywordSet` | survives via consumer chain in `keyword_dispatch.rs` (need verification) |
| `SharedKeywordTable<'a>` | dead — only refs are #11/#12 producers |
| `try_build_shared_table` | DEAD (#11) |
| `emit_shared_table` | DEAD (#12) |
| `shared_vocab_ident` | DEAD (#30) |

The file documents the W4 keyword-dispatch consolidation; the
`SharedKeywordTable` builder pipeline ships unwired. **Recommendation:
DELETE NOW** — fold whatever `RuleKeywordSet` consumers exist into
`keyword_dispatch.rs` directly, retire `phf.rs`.

### Cluster C — `crates/core/src/generate/serialize/` (entire dir dead)

| File | LOC | Status |
|---|---:|---|
| `mod.rs` | 105 | `generate_serialize_methods` dead (#8) |
| `serialize.rs` | 51 | only consumer is the dead `mod.rs` |

**Recommendation: DELETE NOW** — the directory is fully dead.
156 LOC. bbnf-ser frontend (`crates/bbnf-ser/`) ships the actual
serialize emit elsewhere; this is leftover scaffolding.

### Cluster D — `crates/core/src/backend/rust/ir_types.rs` lines 289–372

| `pub` item | Status |
|---|---|
| `type_desc_to_syn` (line 289) | DEAD (#22) |
| `type_desc_is_span` (line 358) | DEAD (#23) |
| `type_is_span` (line 362) | DEAD (#24) |

A 84-line block of three dead helpers around the live recursive
`type_desc_to_syn_raw`. **Recommendation: DELETE NOW** the three
wrappers; if any future caller wants the slice-mode default, it can
inline `type_desc_to_syn_raw(.., true)` or the file can re-add a
single wrapper at that point.

### Cluster E — `crates/core/src/backend/rust/emitter/shapes/cursor_param.rs`

| `pub fn` | Consumers |
|---|---|
| `cursor_generic_clause` | DEAD (#20) |
| `cursor_where_clause` | 8 emitter call sites |
| `cursor_param` | 8 emitter call sites |
| `cursor_arg` | DEAD (#21) |

2 of 4 helpers shipped without a consumer. **Recommendation: DELETE NOW**
the two dead helpers; the cross-shape sites that "should" use `cursor_arg`
inline `quote! { cursor }` instead — that inlining is fine, the helper
duplicated it.

## Substrate-audit test improvements

The current `crates/ir/tests/substrate_audit.rs` is a `#[test]` that
panics on zero-caller. Three concrete improvements would raise its
precision and accelerate fail-feedback.

### Improvement 1 — `pub(crate)` audit variant

The audit only enumerates bare `pub`. Many of the 13 "caller-route"
W6.2 items are misclassified-`pub` substrates that should be
`pub(crate)` or `pub(super)`. Adding a *second* test
`substrate_audit_visibility_minimization` that enumerates `pub` items
whose only callers are in the same crate, recommending a downgrade,
would catch class-D items (visibility too wide) the current zero-caller
audit cannot. This is `WIRE NOW` scope: same machinery, different
filter.

### Improvement 2 — populated whitelist with reasons

`SANCTIONED_SUBSTRATES` is currently `&[]`. Per W5-substrate-denominator.md
"Sanctioned Whitelist" the table is empty at AZ-IV close. The 7
items in W6.2's sanction bucket (#13–#19) **MUST** land in the array
with their one-line reasons before the zero-caller test can go
green — otherwise the audit reports them as failures forever even
after the 25 dead/route items are resolved. This is the immediate next
step.

### Improvement 3 — build.rs vs `#[test]`

The audit currently runs as a `cargo test`-time check (~1.4s). Moving
to `crates/ir/build.rs` would make it a compile-time check that fails
the build before tests run. Trade-off: `build.rs` slows every build by
~1.4s; `#[test]` only fires under `cargo nextest run`. **Recommendation:
keep as `#[test]`** — the CI gate via
`.github/workflows/ci.yml` runs nextest unconditionally, and the
test-time location keeps `cargo build` fast for inner-loop iteration.
This rejects the "improve to build.rs" idea explicitly.

### Improvement 4 — trait-method dispatch handling

The "Known Misses" docstring acknowledges trait-method substrates are
identifier-matched (`.foo()` against any type counts as a caller). For
the audited 32 there's no trait-method false-negative, but a future
addition of a trait method named `parse` or `compile` would silently
disappear into the workspace's namesake noise. **Recommendation: ROUTE
TO AZ-V** — extend the visitor to record the receiver type from
`syn::ExprMethodCall` and require the receiver type to match the
substrate's `impl Trait for Type` site. Bigger lift; not commit-ready
in one pass.

## Triage Summary

| Class | W6.2 count | Verified count | Notes |
|---|---:|---:|---|
| DELETE NOW | 12 | **18** | +6 from W6.2 "route" misclassification (#20, #21, #22, #25, #26, #27, #29). Includes 5 module-cluster deletions. |
| WIRE NOW | 0 | **2** | Newly surfaced: `merge_path_seed` consumer in `prepare_grammar`; populate `SANCTIONED_SUBSTRATES`. |
| TRANSPOSE (AZ-V) | 0 | **2** | strategy/ directory retirement; trait-method dispatch in audit. |
| WHITELIST | 7 | 7 | #13–#19 unchanged; lands as `SANCTIONED_SUBSTRATES` constant population. |
| Visibility downgrade | 13 | 5 | Only #28, #31, #32 + #23/#24 (both delete-or-downgrade) survive as genuine `pub→pub(crate)` candidates. |
| Routing wiring (real) | 0 | 0 | Every "route" candidate either resolves to delete (no consumer) or visibility downgrade (consumer in-crate). |

## Recommended Path Forward

A single **substrate-cleanup-pass** commit lands the mechanical
deletions and whitelist population. Visibility narrowing is a separate
commit because it touches different files and should not bundle with
`-D` deletions. A third commit handles the missing wire.

**Commit 1 — `chore(core/cleanup): retire dead substrate (clusters A–E)`**
- Delete `crates/core/src/generate/serialize/` (Cluster C, 156 LOC)
- Delete `crates/core/src/generate/regex/phf.rs` 4 dead fns (Cluster B)
- Delete `crates/core/src/backend/rust/ir_types.rs:289–372` (Cluster D)
- Delete `crates/core/src/backend/rust/emitter/shapes/cursor_param.rs`
  `cursor_generic_clause` + `cursor_arg` (Cluster E)
- Delete `crates/core/src/graph/scc.rs:209–250` (#1, #2)
- Delete `crates/core/src/backend/strategy/{ref,repeat,seq}_strategy.rs`
  classifier fns (#4, #5, #7); leave `AltStrategy` for Commit 2
- Delete `crates/core/src/backend/types/mod.rs:61` `SeqResultStrategy`
- Delete `crates/core/src/backend/strategy/mod.rs:33` `NodeStrategy`
- Delete `crates/core/src/backend/rust/emitter/keyword_dispatch.rs:199,209`
- Delete `crates/core/src/backend/kernels/identifier.rs:23` `emit_call_with_escapes`
- Delete `crates/ir/src/passes/recognizers/pattern_alphabet.rs:375`
  `make_alphabet`
- Delete `crates/simd-scan/src/parity.rs:210` `compute_inside_string_bytes`
- Delete `crates/csp-solver/src/solver/monotonic.rs:58` `propagate_stratified`
- Delete `crates/core/src/generate/regex/byte_class.rs:207`
  `classify_rule_alphabet`

**Commit 2 — `chore(core/strategy): collocate AltStrategy with driver`** (TRANSPOSE)
- Move `crates/core/src/backend/strategy/alt_strategy.rs` →
  `crates/core/src/backend/driver/alt_strategy.rs`
- Delete `crates/core/src/backend/strategy/` directory
- Update `crates/core/src/backend/mod.rs` re-exports
- Update consumers in `backend/driver/{alt,mod,analysis}.rs`

**Commit 3 — `chore(ir/audit): populate SANCTIONED_SUBSTRATES`** (WHITELIST)
- Add 7 entries to `crates/ir/tests/substrate_audit.rs:100–107` with
  one-line reasons per W6.2 § "Sanction-whitelist" (#13–#19)
- Add 5 visibility-downgrade items: change `pub fn` →
  `pub(crate) fn` for `compute_excluded_bytes`, `charset_from_class_body`,
  `type_desc_is_span`, `type_is_span`, `emit_call_with_escapes`. (Note:
  three of these are also delete candidates — pick whichever is
  smaller-touch in the cleanup commit, then this commit only narrows
  what remains.)

**Commit 4 — `feat(pipeline): wire path_seed rewrites into RuleSet`** (WIRE NOW)
- Identify the right pipeline stage (likely `prepare_grammar` or
  `compile_grammar`) and call `RuleSet::merge_path_seed()` on the
  per-grammar `RuleSet` once.
- Add a regression test asserting the 3 path-seed rewrites land in the
  grammar's saturation set after pipeline run.

**Commit 5 — `docs(az-iv/audit): mark substrate audit zero-row`** (CLOSE)
- Update `W5-substrate-denominator.md` "Current Surface" with new
  zero-caller count = 0.
- Update `W6-substrate-cleanup-route.md` "Hard Gate 13 Status" to
  "MET in infrastructure + zero-row".
- Stretch: produce the missing `docs/benchmarks/wave-AZ-IV-W{3,4,5}.json`
  bridging snapshots, OR amend the wave specs to drop the requirement
  with a one-line "post-close honesty: deferred to AZ-V" note.

After all five commits land, the substrate-audit test's 0 zero-caller
state holds permanently and Hard Gate 13's residual condition is fully
discharged.
