## DEEP-SYNTHESIS — Direct-Projection Tranche, Canonical Ordering

**Audit cohort**: DEEP-A (assay) + DEEP-B (profile) + DEEP-C (path forward) + DEEP-D (re-ordering).
**Inputs**: commits `1e5f8c0e`, `68fb40cb`, `0f7b3446`, `34e4ebe9`.
**Base**: master `d4b8de18`.
**Mandate**: NO quick solutions, NO workarounds, idiomatic gestalt, KISS, ONE PATH, canonical letter ordering.

This synthesis supersedes `POST-CLOSE-SYNTHESIS.md`. The earlier draft proposed BD because it failed to consider the recycle/subsume reversibility. DEEP-D's archaeology corrects that.

## I — The Architectural Defect (precise)

The post-close audits converged on one defect with three measurable manifestations.

**DEEP-A's reading** — *the struct registry is populated by `project_types` at codegen time but never consumed at parse time*. Every emitted parse fn constructs `__layout: StructLayout { rule_type: TypeDesc::Span, fields: vec![] }` (nine emission sites) — type inference output is thrown away at the parse boundary. `SimpleStructBuilder::push_leaf_with_*` deposits `V::unit()` for 5 grammars, discarding typed leaf payloads.

**DEEP-B's reading** (samply, 25,963 samples, fat-LTO `[profile.bench]`) — *the dominant cost is `<JsonStructBuilder as StructBuilder>::checkpoint`, which deep-clones the in-flight `Vec<OpenFrame>` on every speculative branch*. **86.07% of inclusive samples** attribute to `Vec<OpenFrame>::clone`. This single mechanism explains:
- the 18/19 AU floor BELOW status (every typed builder requires checkpoint discipline; AU's flat per-grammar arenas had no equivalent),
- 5.22× of the `bbnf_value_twitter` sonic-rs gap (clone cost),
- the 4196× `bbnf_get_twitter` gap is *the same* + `bbnf_get_*` calling eager `JsonParser::parse` instead of routing to `parse_with`.

These two readings are the same defect viewed at two altitudes. DEEP-A names the architectural shape (typed projection unwired); DEEP-B names the runtime cost mechanism (clone-on-checkpoint). The defect is: **the parser does runtime checkpoint-and-rollback over an untyped slab because compile-time-resolved direct projection isn't emitted; the cost is dominated by a `Vec<OpenFrame>::clone` per speculative branch.**

**The single primary blocker for both semantic parity (TS Node-execute, substrate audit) and performance parity (sonic-rs, AU floor) is therefore one mechanism**: emit each grammar's typed records inline at codegen time so the parser writes directly to typed fields, eliminating the speculative-checkpoint slab discipline and the registry indirection that supports it.

## II — Canonical Lettering (Option A locked)

Per DEEP-D archaeology and the user's invariant that AZ → BA → BB → BC → BD must be canonical:

| Letter | Pre-correction | Post-correction | Disposition |
|---|---|---|---|
| **BA** | recycled for rule-discovery | **un-recycled** → direct-projection codegen | Old BA scope was already absorbed into AZ-IV; the recycle was historical. The new BA letter scope is direct-projection. |
| **BB** | SUBSUMED | **un-subsumed** → rule-discovery (the originally-planned BB scope; same scope as recycled-BA) | The original BB rule-discovery plan returns. |
| **BC** | closed (Shared Precepts Consumer Rollout — orchestration meta-tranche) | **archived** → cleanup pass / sweeping carries (post-direct-projection, post-rule-discovery) | Orchestration content moves to `docs/tranches/BC/orchestration-archive-2026-04-30/`; the BC letter is repurposed as the cleanup tranche. The orchestration close artefact is preserved unmodified in the archive directory. |
| **BD** | (synthesis-proposed earlier; retracted) | reserved for whatever post-cleanup tranche surfaces | Future. |

The trajectory becomes: **AZ-IV (closed) → BA (direct-projection) → BB (rule-discovery) → BC (cleanup) → BD+ (future)**. This honors the user's canonical AZ→BA→BB→BC→BD ordering verbatim, eliminates the fictional AZ-V, and binds every routed AZ-IV carry to a real letter.

## III — The Defect's Three Manifestations and the Single Fix

| Manifestation | Site | Today | Direct-projection mechanism |
|---|---|---|---|
| Speculative `Vec<OpenFrame>::clone` (86.07% inclusive) | `JsonStructBuilder::checkpoint` invoked across `parse_wrap_*` byte-dispatch towers | Deep-clones the in-flight stack on every speculative branch entry | Predictive first-byte dispatch (JSON's byte alphabets are disjoint at branch points); `Checkpoint` becomes `(stack_depth, arena_count)` — a value, not a clone. ≥ 80% reduction per DEEP-B. |
| Registry indirection on every `begin_compound` | `compound_kind_for_layout(&StructLayout)` runtime lookup; `__layout` constructed per-call | Type info that codegen knows is thrown away and re-derived at runtime | Emitter generates per-rule typed projection inline. Layout is a static, not a runtime value. |
| `Document::get<T>(path)` walks materialised AST | `parse(input)?.get(path)` is the value-API hot path; `parse_with` exists but is unused by the value API | Two orthogonal codepaths (`feedback_no_orthogonal_codepaths` violation) | `Document::get<T>(path)` reroutes through `parse_with(input, &path)`; eager `parse(input)` becomes `parse_with(input, &EMPTY_PATH)`. One codepath. Sonic-class get API mirrored with grammar-aware compile-time validation. |

These are three views of the same fix: **direct-projection codegen produces a typed `<Grammar>Document` per grammar; parse fns write to typed fields; speculative branches use predictive first-byte dispatch with cheap value-typed checkpoints; `Document::get<T>` short-circuits via `parse_with` for path-resolved leaves**. Sonic-rs reaches 332ns by lazy byte-walk; we reach the same architectural class by lazy-parse-with-cheap-checkpoint and direct-projection.

## IV — Type Inference for `->`-Less Rules (the user's specific question)

Per DEEP-A and DEEP-C: `project_types` already produces a TypeDesc for every rule, annotated or not. The gap is not the inference pass; it is the *consumption* of inference output by the emitter. For compound-typed `->`-less rules (e.g., a JSON `value` Alt over `string | number | object | array | null | bool`), the inference produces a `TypeDesc::Sum(branches)` with each branch fully typed — but the emitter currently lowers this to a `StructLayout` with `kind: Span, fields: vec![]` and the parser arena receives a heterogeneous slab.

The fix in BA.W1: an inverse-layout-audit IR pass guarantees every compound-typed rule has a `StructLayout` (annotated or not); the emitter consumes it and produces a typed Rust struct/enum per layout. `->`-less rules project the same as annotated rules — type inference IS the source of truth, and the codegen consumes it without any per-rule special case.

Concrete consequence: a grammar can be written without a single `->` annotation and the emitter still produces typed output. The annotation becomes a *naming hint* (the user wants `Length` not `LengthRule`), not a *typing hint* (the user wants this to have a type, please figure out which). This restores the GESTALT invariant verbatim.

## V — Sonic-Class `get` API with Superior Ergonomics

```rust
// Compile-time-typed (the typed-AST equivalent of sonic-rs `pointer!`):
let title: Option<&str> = doc.get(path!(Json, "statuses", 0, "text"));
// path! validates against the grammar's StructRegistry at proc-macro time;
// invalid paths fail to compile with a syn::Error::new pointing at the offending segment;
// the return type is INFERRED from the path's terminal TypeDesc — no turbofish required for the common case.

// Lazy bail-out (no document materialization):
let title: Option<&str> = JsonParser::get(input, path!(Json, "statuses", 0, "text"));
// This reroutes through parse_with internally; the parser short-circuits at first leaf match.
// On twitter.json: 332ns class (samply-attributed).

// Wildcard lazy iteration (already lands in W2; the iterator is the path expression itself):
for (anchor, name) in JsonParser::get_iter(input, path!(Json, "statuses", "*", "user", "name")) {
    // ...
}

// Variant-select on sums (already lands in W2):
let color: Option<&CssColor> = doc.get(path!(CssL4, "rules", 0, "declarations", 0, "value", "color"));

// Runtime-dynamic path (less common; flexible — discouraged for hot paths):
let v: Option<JsonValue<'_>> = doc.get_dyn(&runtime_path);
```

Superior to sonic-rs's `pointer!`: compile-time grammar-aware diagnostics; type-inferred return type; wildcard returns a zero-allocation iterator instead of a `Vec`. Superior to simdjson OnDemand: no manual iterator state; the path expression IS the iterator.

The `get` mechanism is per-grammar; each `<Grammar>Parser` exposes the same shape because the codegen template is grammar-general.

## VI — TS / WASM Punt and Shared-ABI Question

The user explicitly punted TS and WASM. Position: TS and WASM backends are not load-bearing for direct-projection. They will be re-engineered or routed to a shared ABI in a separate tranche letter — likely BD or later, after BC cleanup. The shared-ABI feasibility question is genuine:

- **Option 1 — `wasm-bindgen-shared`**: works, but binds us to Wasm runtime semantics and pays the JS-bridge marshalling cost the W5.2 Node-execute test surfaced.
- **Option 2 — `abi_stable`**: stable Rust ABI for plugin-style cross-crate use; not a TS bridge.
- **Option 3 — custom IR-based ABI**: emit a flat byte-encoding of the typed IR + per-grammar reader. Both Rust and TS read the same encoding. The encoding becomes the contract; no marshalling. Closes the W5.2 RED gate by mechanism (TS reads typed IR directly), not by patching the TS emitter.

**Decision deferred to a post-BC tranche** (BD candidate). The deep audits do not select an option; the user requested explicit punt and that is honored. AZ-IV.md §Hard Gate 4 (TS executable) routes to that future tranche with named destination.

## VII — BA Tranche Shape (direct-projection codegen)

**Letter**: BA (un-recycled).

**Thesis**: Direct-projection codegen. The lazy path is canonical; eager is its degenerate case; type inference output reaches every emit site; the speculative-checkpoint Vec-clone discipline retires; `Document::get<T>(path)` becomes the typed accessor mirroring sonic-rs's `pointer!` with superior ergonomics.

**Waves**:
- **BA.W0 — Truth, regen, cleanup absorption.** Mirrors AZ-IV.W0 pattern. Records `post-AZ-IV.json` rows as the floor; resolves the worktree fixture symlink contract per W6.2 known miss. Absorbs three cleanup commits: 18 verified-dead substrate DELETE + populate `SANCTIONED_SUBSTRATES`; 3 module-cluster retirements (`generate/serialize/`, `generate/regex/phf.rs` survivor fold, `backend/strategy/` collocate); `merge_path_seed` wire-or-delete (route W3.0 path-shape rewrites to the canonical egraph saturation site OR delete; deletion is preferred unless BB-rule-discovery prep needs them as seed bag).
- **BA.W1 — Inverse-layout-audit IR pass.** Every compound-typed rule (annotated OR `->`-less) produces a complete `StructLayout` consumable by the emitter. The pass fails the build when a rule's TypeDesc is non-trivial but its `StructLayout` is empty. `project_types` integration; runs after type inference; before emit.
- **BA.W2 — Direct-projection codegen.** The emitter generates per-grammar `<Grammar>Document` typed struct + `<Grammar>Value` typed enum from `StructRegistry`; parse fns write directly to typed fields. The `arena_template` + `builder_template` (W5.3 dedup) retires from the value-API hot path. The 18/19 AU floor BELOW closes here.
- **BA.W3 — Speculative checkpoint redesign.** Replace `Vec<OpenFrame>::clone` with `Checkpoint` as `(stack_depth, arena_count)` value-typed snapshot. Predictive first-byte dispatch in JSON (byte alphabets disjoint at branch points). Per DEEP-B: ≥ 80% inclusive-samples reduction expected.
- **BA.W4 — `parse_with` as value-API hot path.** `Document::get<T>(path)` reroutes through `parse_with(input, &path)`; eager `parse(input)` collapses to `parse_with(input, &EMPTY_PATH)`; `__EAGER_EMPTY_PATH<Json,_>` cross-grammar literal DELETED; one codepath. Sonic-class `get` API lands. The 4196× `bbnf_get_twitter` gap closes here.
- **BA.W5 — Cursor consult unification + LegacyPath retirement.** `cursor.match_field` + `cursor.match_index` + `cursor.decide` collapse into `cursor.consult(&ParsedSegment)`. `LegacyPath`/`LegacySegment` shim across `runtime/{json,css_l4,sheets,bbnf}/parse_with.rs` retires; `Document::get<T>` consumes `TypedPath` directly.
- **BA.W6 — Measurement & close.** Same-harness sonic-rs floor MET (`bbnf_get_*` ≤ 5× `sonic_get_*`); AU floor 19/19 at-or-above; tailwind WATCHDOG resolves through direct-projection (Pratt + unordered shape paths consume typed projection same as struct shapes); samply 7-artefact contract per claim becomes the canonical close discipline (no more environmental gating). FINAL.md cites resolving artefact for every Hard Gate.

**Hard Gates** (target ≥ 22):
- All 23 AZ-IV Hard Gates that closed `MET_WITH_MISSES` or `ROUTED` close `MET` here.
- `cargo nextest run --workspace` 100% pass (closes ts_node_execute and substrate_audit RED tests; the latter via W0 cleanup, the former via direct-projection projecting TS aggregates as iterables — though the user's TS/WASM punt may route this to a later letter, in which case `ts_node_execute` is `#[ignore]`-marked with named successor letter, not RED).
- Speculative checkpoint cost ≤ 14% of inclusive samples on `bbnf_value_twitter` (DEEP-B's ≥ 80% reduction target met or exceeded).
- `bbnf_get_twitter` ≤ 5× `sonic_get_twitter` same-harness, samply-attributed (Hard Gate 7).
- AU floor 19/19 at-or-above (closes the AF MASKED-DEFERRAL).
- Speculative-checkpoint `Vec<OpenFrame>::clone` site does not appear in samply top-3 hotspots.
- `__EAGER_EMPTY_PATH` cross-grammar literal absent from `crates/core/src/grammar/generated/**`.
- The `arena_template` + `builder_template` modules deleted (per `feedback_no_orthogonal_codepaths`).

**Non-Routable Carries**: every Audit-C MASKED-DEFERRAL closes inside BA. F2 (sonic gap) → W4. AF (AU floor) → W2 + W3. F8 (32 zero-caller substrates) → W0. F5 (TS Node-execute) → routes to the named TS/WASM tranche after BC; if that tranche is not BD, the route names the actual letter.

## VIII — BB Tranche Shape (rule-discovery, un-subsumed)

The original BB rule-discovery scope returns verbatim from `docs/tranches/BB/historical/BB.md` (after archive). Plan adapts to consume the BA-direct-projection IR (same StructRegistry, same TypedPath, same direct-projection emit) but the architectural skeleton — Ruler CVC enumerator, VM oracle on residue, ranker, Class-1/2/3 tiering, grammar-colocated `*.ron` rewrites — is unchanged from the recycled-BA plan. `merge_path_seed` consumer wiring is BB.W1 if BA.W0 deferred deletion.

## IX — BC Tranche Shape (cleanup pass, repurposed)

BC absorbs Audit-A's TRANSPOSE bucket (12 items), AUDIT-B's routed splits (`runtime/css_l4/builder.rs`, `passes/types/mod.rs`, `csp_strategy/mod.rs` further splits), the W6.2-known worktree fixture symlink contract codification, the samply 7-artefact contract as canonical close discipline. Each carry has named close criterion and bench/test evidence.

The pre-existing BC orchestration content (`Shared Precepts Consumer Rollout`) archives unmodified to `docs/tranches/BC/orchestration-archive-2026-04-30/` — the close artefact and the precepts submodule pin (`e490e8ed`) are preserved for archaeology. The BC letter itself is rewritten with the cleanup-tranche scope.

## X — Plan-Doc Surgery (deletion + archive + rewrite)

Per DEEP-D, with this synthesis as the precipitating commit:

| File | Disposition | Reason |
|---|---|---|
| `docs/tranches/REMAINING-TRAJECTORY.md` | **DELETE** | SUPERSEDED at `cb14970f`; references fictional AZ-V and stale post-AZ-III handoff. GESTALT.md is the canon. |
| `docs/tranches/AZ-IV/audit/POST-CLOSE-SYNTHESIS.md` | **DELETE** | Earlier synthesis named "BD" then "BC" then back; superseded by this DEEP-SYNTHESIS. |
| `docs/tranches/BA/BA.md` | **ARCHIVE → `BA/historical/BA-rule-discovery.md`** + new BA.md for direct-projection | Old BA scope is preserved for archaeology; new BA scope is direct-projection. |
| `docs/tranches/BA/PROGRESS.md` | **ARCHIVE → `BA/historical/`** + new PROGRESS.md when BA opens | Old scope's progress doc moves; new doc tracks new scope. |
| `docs/tranches/BB/BB.md` | **un-archive: `BB/historical/BB-recycled.md` and rewrite top-level `BB.md`** | The SUBSUMED banner was correct under the recycle scheme; under the canonical scheme BB takes rule-discovery (its original scope; ironic and clean). |
| `docs/tranches/BC/{BC.md,FINAL.md,...}` | **ARCHIVE → `BC/orchestration-archive-2026-04-30/`** + new BC.md for cleanup | The orchestration close artefact is preserved unmodified; the letter is repurposed. |
| `docs/tranches/AZ-IV/FINAL.md` row F12 (bbnf-buddy as routed follow-on) | **EDIT** | bbnf-buddy is a separate subproject (per memory `project_bbnf_buddy.md`); it does not consume a tranche letter. Remove the conflation. |
| `docs/GESTALT.md` | **REFRESH** | Update to reflect canonical ordering AZ → BA (direct-projection) → BB (rule-discovery) → BC (cleanup) → BD+; remove fictional AZ-V; cross-reference DEEP-SYNTHESIS for the active mechanism plan. |
| `docs/codegen-paths.md` | **REFRESH** | The arena/builder template path retires; direct-projection becomes the canonical codegen path. Update the pipeline diagram. |

The earlier `POST-CLOSE-A/B/C/D` audit docs and DEEP-A/B/C/D audit docs remain in `docs/tranches/AZ-IV/audit/` as the inputs to this synthesis. They are not deleted; they are the citations.

## XI — Implementation Order

The user said "this must be implemented." Execution sequence:

**Phase 1 — Plan surgery (this commit + 3 follow-ups, all docs-only):**
1. This synthesis (`DEEP-SYNTHESIS.md`) lands.
2. Delete `REMAINING-TRAJECTORY.md` and `POST-CLOSE-SYNTHESIS.md`.
3. Archive BA, BB, BC content; write new top-level `{BA,BB,BC}/{BA,BB,BC}.md` for the new scopes.
4. Refresh GESTALT.md and codegen-paths.md.

**Phase 2 — BA.W0 cleanup absorption (3 commits):**
1. Delete 18 verified-dead substrates + populate `SANCTIONED_SUBSTRATES` whitelist (closes substrate_audit RED test).
2. Retire 3 module clusters (`generate/serialize/`, `generate/regex/phf.rs` survivors fold into `keyword_dispatch`, `backend/strategy/` collocate live `AltStrategy` with `backend/driver/alt.rs`).
3. `merge_path_seed` decision (delete unless BB.W1 wants it as seed bag).

**Phase 3 — BA waves W1-W6** (parallel + sequenced per the wave-shape above; orchestrator dispatches in the AZ-IV pattern).

**Phase 4 — BB then BC** (sequenced).

## XII — Closing

The AZ-IV close was honest about its misses and routed them to follow-on. The post-close audits surfaced that the routes were partially fictional (no AZ-V exists) and that the architectural mechanism behind the chronic perf gap was not "registry indirection" handwave but `Vec<OpenFrame>::clone` per speculative branch (DEEP-B, samply-attributed). This synthesis names the defect, names the fix, names the canonical letter sequence, and sequences implementation. The trajectory is one path: **AZ-IV (closed) → BA (direct-projection codegen) → BB (rule-discovery) → BC (cleanup) → BD+ (TS/WASM re-engineering or shared ABI; future)**.

ONE path forward.
