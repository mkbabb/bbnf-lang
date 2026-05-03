# Tranche BA — Direct-Projection Codegen

> **Letter status — un-recycled at master `40092b28` (post-AZ-IV close).**
> The previous BA tranche (typed pointer-path queries; absorbed into AZ-IV.W2) and the subsequently-recycled BA tranche (rule-discovery; absorbed into the un-subsumed BB) are **archived** at `docs/tranches/BA/historical/recycled-rule-discovery/`. Per `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md`, the canonical post-AZ-IV letter sequence is **AZ → BA (direct-projection) → BB (rule-discovery) → BC (cleanup) → BD+ (TS/WASM re-engineering)**. The AZ → BA → BB → BC → BD ordering is canonical per the user's directive; fictional AZ-V is removed from all close-state docs.
>
> BA opens after AZ-IV close. Phase 1 plan-surgery (this commit) precedes BA.W0 dispatch.

## Thesis

Direct-projection codegen restores the GESTALT §2 *direct-to-struct* invariant verbatim: every grammar rule's TypeDesc — annotated with `->` or inferred for `->`-less rules — reaches the emitter and produces a typed Rust struct/enum at codegen time. The parse fn writes directly to typed fields. The runtime arena/builder template registry indirection retires. The lazy parse path becomes canonical; eager is its degenerate case. `Document::get<T>(path)` mirrors sonic-rs's `pointer!` API with superior ergonomics: compile-time grammar-aware diagnostics, type-inferred return type, zero-allocation wildcard iterator.

## The architectural defect (per DEEP-A + DEEP-B)

Two altitudes, same defect:

- **DEEP-A (architectural)**: the struct registry is populated by `project_types` at codegen time but never consumed at parse time. Every emitted parse fn constructs `__layout: StructLayout { rule_type: TypeDesc::Span, fields: vec![] }` — type inference output is thrown away. `SimpleStructBuilder::push_leaf_with_*` for 5 grammars deposits `V::unit()`, discarding typed leaf payloads.
- **DEEP-B (samply, 25,963 samples, fat-LTO)**: 86.07% of inclusive samples are `Vec<OpenFrame>::clone` from `<JsonStructBuilder as StructBuilder>::checkpoint`. This single mechanism explains the 18/19 AU floor BELOW + 5.22× sonic_value gap + 4196× sonic_get gap (latter compounded by `bbnf_get_*` calling eager `parse` instead of routing to `parse_with`).

The parser does runtime checkpoint-and-rollback over an untyped slab because compile-time-resolved direct projection isn't emitted. Direct-projection codegen restores the invariant and retires the speculative-checkpoint clone discipline.

## Invariants (BA-scoped; AZ-IV invariants persist + are extended)

1. **Direct-to-struct.** Every compound-typed rule (annotated OR `->`-less) projects to a typed Rust struct/enum at codegen time. The emitter consumes `StructRegistry` output; runtime layout literals are deleted.
2. **One parse path.** Eager `parse(input)` collapses to `parse_with(input, &EMPTY_PATH)`. The cross-grammar `__EAGER_EMPTY_PATH<Json,_>` literal retires.
3. **Cheap checkpoints.** `Checkpoint` is a value (`(stack_depth, arena_count)`), not a clone. Speculative branches use predictive first-byte dispatch where alphabets are disjoint.
4. **Sonic-class `get`.** `Document::get<T>(path)` reroutes through `parse_with` for path-resolved leaves. The lazy lane is the value-API hot path, not eager-then-walk.
5. **Type inference is the source of truth.** `->`-less rules project the same as annotated rules; the annotation becomes a naming hint, not a typing hint.
6. **No legacy code.** Per the user: NO quick solutions, NO workarounds, idiomatic gestalt, KISS, ONE PATH.

## Wave Table

| Wave | Agents | Closes on evidence | Status |
|---|---:|---|---|
| BA.W0 — Truth, regen, cleanup absorption | 5 parallel | strict regen 9/9 green; 18 zero-caller substrates DELETED; 3 module clusters retired; `merge_path_seed` decision; worktree fixture symlink contract codified | planned |
| BA.W1 — Inverse-layout-audit IR pass | 5 parallel | every compound-typed rule has a non-empty `StructLayout`; `project_types` extension; build fails on un-inferred compound rule | planned |
| BA.W2 — Direct-projection codegen | 6 parallel | per-grammar `<Grammar>Document` typed struct + `<Grammar>Value` typed enum emitted from `StructRegistry`; `arena_template` + `builder_template` retired from value-API hot path; AU floor 18/19 BELOW closes | planned |
| BA.W3 — Speculative checkpoint redesign | 5 parallel | `Vec<OpenFrame>::clone` no longer in samply top-3; `Checkpoint = (stack_depth, arena_count)`; predictive first-byte dispatch in JSON; ≥80% inclusive-sample reduction per DEEP-B | planned |
| BA.W4 — `parse_with` as value-API hot path | 5 parallel | `Document::get<T>(path)` reroutes through `parse_with`; eager `parse` collapses to `parse_with(input, &EMPTY_PATH)`; sonic-class `get` API lands; `bbnf_get_twitter` ≤ 5× `sonic_get_twitter` | planned |
| BA.W5 — Cursor consult + LegacyPath retirement | 5 parallel | `cursor.match_field` + `cursor.match_index` + `cursor.decide` collapse into `cursor.consult(&ParsedSegment)`; `LegacyPath`/`LegacySegment` shim retires; `Document::get<T>` consumes `TypedPath` directly | planned |
| BA.W6 — Measurement & close | 3 parallel | AU floor 19/19 at-or-above; sonic-rs floor MET; samply 7-artefact contract per claim becomes canonical close discipline; FINAL.md cites resolving artefact for every Hard Gate | planned |

## Hard Gates (target ≥ 22 — final tally locks at W0 close)

1. `cargo xtask regen --check` 9/9 green at every wave close.
2. `cargo nextest run --workspace --cargo-profile ax-iter` 100% pass at BA close (closes the AZ-IV `ts_node_execute` and `substrate_audit` RED tests; the former via direct-projection projecting TS aggregates as iterables OR via explicit `#[ignore]` with named successor letter per TS/WASM punt).
3. **Speculative-checkpoint cost ≤ 14% of inclusive samples** on `bbnf_value_twitter` samply trace (DEEP-B's ≥80% reduction met or exceeded). Same-harness fat-LTO `[profile.bench]`. Saved samply 7-artefact contract under `.profiles/samply/post-BA/`.
4. **`bbnf_get_twitter` ≤ 5× `sonic_get_twitter`** same-harness, samply-attributed. Hard Gate 7 (AZ-IV) closes here.
5. **AU floor 19/19 at-or-above** on `docs/benchmarks/post-BA.json` `floors.post-AU.rows_at_or_above`. AF MASKED-DEFERRAL closes here.
6. `Vec<OpenFrame>::clone` site does not appear in samply top-3 hotspots on any production bench.
7. `__EAGER_EMPTY_PATH` cross-grammar literal absent from `crates/core/src/grammar/generated/**`.
8. `crates/core/src/runtime/{arena_template,builder_template}.rs` deleted.
9. `crates/core/src/runtime/{json,css_l4,sheets,bbnf,bnf,csv,ebnf,math,css_pretty}/{arena,builder}.rs` deleted (where direct-projection subsumes them).
10. `LegacyPath` and `LegacySegment` deleted from `crates/core/src/runtime/path/**` (the W3 shim).
11. `cursor.match_field`, `cursor.match_index`, `cursor.decide` deleted; `cursor.consult` is the only call.
12. Per-grammar `__path_plan { pub use crate::path::cursor::{Decision, SegmentKind}; }` re-exports deleted; single source from `crate::path::cursor`.
13. **Type inference covers every `->`-less compound rule** — BA.W1 inverse-layout audit pass green.
14. `Document::get<T>(path)` consumes `TypedPath<G, T>` directly; no `LegacyPath` lowering.
15. `JsonParser::get<T>(input, path)` (and per-grammar equivalents) — sonic-class API lands and routes through `parse_with`.
16. `path!` proc-macro return type is type-inferred from path's terminal TypeDesc (no turbofish required for the common case).
17. Wildcard `JsonParser::get_iter(input, path!(..., "*", ...))` returns a zero-allocation iterator.
18. Substrate-audit test (`crates/ir/tests/substrate_audit.rs`) GREEN. The 32 zero-caller substrates from W5.4 are deleted-or-whitelisted at W0.
19. **Samply 7-artefact contract per claim** per `docs/instructions/PROFILING.md`; environmental gating retires.
20. `docs/benchmarks/post-BA.json` lands per `docs/benchmarks/SPEC.md` schema; `floors` block compares row-by-row against `post-AU.json` and `post-AZ-IV.json`; `competitors` block carries same-harness sonic-rs / lightningcss / simdjson rows.
21. `cargo fmt --all -- --check` passes.
22. `git diff --check` passes; FINAL.md cites resolving artefact for every gate, miss, deletion, and handoff.

## Non-Routable Carries (every Audit-C MASKED-DEFERRAL closes inside BA)

| Carry | Audit-C class | BA wave | Close criterion |
|---|---|---|---|
| F2 sonic-rs ≤ 5× | MASKED-DEFERRAL | W4 | `bbnf_get_twitter` ≤ 5× `sonic_get_twitter` same-harness samply-attributed |
| AF AU floor 18/19 BELOW | MASKED-DEFERRAL | W2 + W3 | 19/19 at-or-above |
| F8 32 zero-caller substrates | CHRONIC-RISK | W0 | substrate_audit GREEN |
| F4 Tailwind regex_scan timeout | CHRONIC-RISK | W4 (direct-projection eliminates per-call overhead) OR routes to BB rule-discovery | named close criterion at W0 dispatch |
| F10 watchdog rows | CHRONIC-RISK | W6 | zero watchdog rows in fat-LTO + bench-iter matrices |
| F5 TS Node-execute (W1 backend-ts gap) | MASKED-DEFERRAL | routes to BD (TS/WASM) per user punt; OR BA.W2 if direct-projection's TS emit naturally fixes it | named successor letter at BA close |

## Cross-Tranche Ordering

- **BA opens after AZ-IV close**, after Phase 1 plan surgery completes (this commit cohort).
- **BB opens after BA closes** with the rule-discovery scope (the original BB scope, un-subsumed; identical to the recycled-BA plan that lived at `docs/tranches/BA/historical/recycled-rule-discovery/`).
- **BC opens after BB closes** as the cleanup pass (Audit-A's TRANSPOSE bucket + AUDIT-B's routed splits + worktree fixture symlink contract + samply 7-artefact contract canonicalization).
- **BD+ reserved** for TS/WASM re-engineering or shared-ABI tranche (per user punt).

## Deletion Bias

Per AZ-IV §Deletion Bias and DEEP-C's enumeration, BA deletes before adding. Forbidden patterns in the BA diff:

- no `_v2` modules, no compatibility feature flags
- no per-grammar `from_rule_name` arm-list (T1 transposition was AZ-IV.W4.4; BA inherits)
- no runtime `__layout: StructLayout` literal at parse-fn entry (replace with codegen-emitted typed projection)
- no `Vec<OpenFrame>::clone` on speculative branch entry (replace with `(stack_depth, arena_count)` value-typed checkpoint)
- no two parse codepaths (eager + lazy collapse to one)
- no `Option<&mut PathCursor>` parameter (cursor mandatory; eager passes `&mut PathCursor::eager()`)

If deletion is unsafe because a current consumer exists, the wave names the consumer and refactors the surface to match its real role.

## Brittleness Window

No tranche-wide brittleness window. A wave may declare a local brittleness window only in its wave spec, with suspended gates, restoration wave, and reason. BA cannot close while any brittleness window is open.

## TS / WASM Position (per user directive)

The user explicitly punted: *"Ignore our TS and WASM backends for now, these are not relevant and will likely need to be fully re-engineered at some point (or can we leverage a shared ABI?)."*

BA scopes to Rust only. TS and WASM backends are not load-bearing for direct-projection. Three options for the future tranche:

- **Option 1 — `wasm-bindgen-shared`**: works, but binds us to Wasm runtime semantics and pays the JS-bridge marshalling cost the W5.2 Node-execute test surfaced.
- **Option 2 — `abi_stable`**: stable Rust ABI for plugin-style cross-crate use; not a TS bridge per se.
- **Option 3 — Custom IR-based ABI**: emit a flat byte-encoding of the typed IR + per-grammar reader. Both Rust and TS read the same encoding. The encoding becomes the contract; no marshalling. Closes the W5.2 RED gate by mechanism.

Decision deferred to the post-BC tranche (BD candidate). The deep audits do not select an option; the user requested explicit punt and that is honored.
