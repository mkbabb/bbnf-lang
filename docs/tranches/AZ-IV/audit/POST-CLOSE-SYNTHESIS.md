## POST-AZ-IV Synthesis — One Path

**Audit cohort**: A (legacy excision) + B (substrate consumption) + C (chronic deferrals) + D (KISS path forward).
**Base**: master `50d25ba1` (AZ-IV closed at `6de6ac0c`; 4 audit docs landed).
**Mandate**: NO quick solutions, NO workarounds, idiomatic gestalt, KISS, ONE PATH.

## I — The Thesis

**DIRECT-PROJECTION CODEGEN.** The lazy path is the canonical parse path; eager is its degenerate case (`parse_with(input, &EMPTY_PATH)`); the value-API and `Document::get<T>(path)` reroute through it; the W5 arena/builder template registry indirection retires from the value-API hot path. One parse path. One projection path. One mechanism for grammar-derived semantics.

This subsumes Audit-D's candidates A/B/C/D into a single direction. It is also the only mechanism that closes the three MASKED-DEFERRAL carries Audit-C identified (F2 sonic-rs ≤5x, F5 TS Node-execute, AF AU-floor regression) without inventing a new tranche letter.

## II — What The Audit Cohort Surfaced

### Audit-A — 24 legacy findings (11 DELETE / 12 TRANSPOSE / 1 JUSTIFIED)

The three highest-leverage:

1. **`LegacyPath` / `LegacySegment` shim across 4 W3 grammars** (`crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/parse_with.rs`) — every lazy parse drops the W2 typed path back to the pre-W2 borrowed alphabet via 4 byte-identical `lower()` helpers. The `JsonPathQuery::get<T>` trait family was never widened to consume `TypedPath`. Single transposition retires ~80 LOC + per-parse `Vec<LegacySegment>` allocation.

2. **`__EAGER_EMPTY_PATH` hardcodes `markers::Json` for ALL 9 grammars** (`crates/core/src/backend/rust/emitter/grammar.rs:414-425`). math, csv, css_pretty, css_l4 all reference `markers::Json` in their generated parse bodies. Survives W1 grammar-overfit static scan because the scan inspects `runtime/**` + `shapes/**` only, not `generated/`. Cross-grammar literal that AZ-IV §Invariant 2 explicitly forbids.

3. **`AscentStrategy` trait + 3 impls + `DefaultAscent` typedef** (`crates/core/src/path/ascent.rs`, 277 LOC) — wired into `PathCursor` as `Option<&dyn AscentStrategy>` field with a `with_ascent()` setter never called outside its own definition. Only consumer is the W2 micro-bench. Substrate-wired-without-consumer pattern AZ-IV §Invariant 3 forbids.

### Audit-B — substrate verification (deletion bucket: 12 → 18)

W6.2's "caller-route" routing was over-optimistic: 6 of 13 routed substrates have zero callers anywhere (`cursor_generic_clause`, `cursor_arg`, `type_desc_to_syn`, `make_alphabet`, `compute_inside_string_bytes`, `propagate_stratified`, `classify_rule_alphabet`). They flip to DELETE.

Three module-level death clusters require directory-level retirement:
- `crates/core/src/generate/serialize/` (156 LOC, fully dead)
- `crates/core/src/generate/regex/phf.rs` (4 of 5 pub fns dead; fold survivor into `keyword_dispatch`)
- `crates/core/src/backend/strategy/` (4 of 5 classifiers dead; collocate live `AltStrategy` with `backend/driver/alt.rs`)

Two real missing wires:
- `merge_path_seed` is registered but no pipeline stage invokes it. The W3.0 path-egraph-seed substrate is inert. Either WIRE NOW or DELETE the rewrites.
- `SANCTIONED_SUBSTRATES` array is empty. Any sanctioned `pub` items will trip the audit forever after the deletes land.

### Audit-C — 12 carries audited; 3 MASKED-DEFERRALs

| # | Carry | Class | Reasoning |
|---|---|---|---|
| F2 | `bbnf_get_twitter` ≤5x sonic | **MASKED-DEFERRAL** | Routed to "AZ-V optimization tranche". AZ-V is invoked 4× in close-state docs but 0× in trajectory/plan/BA/BB. Fictional successor. |
| F5 | TS Node-execute (W1 backend-ts gap) | **MASKED-DEFERRAL** | W1 closed `complete` while Hard Gate 4 promised TS executable parity. W5.2 surfaced the gap. "Post-AZ-IV TS triumvirate" route is W1 scope re-labelled. |
| AF | AU floor 18/19 BELOW | **MASKED-DEFERRAL** | W5 arena/builder template caused regression within AZ-IV's own authoring window. Routing to phantom AZ-V is shim-hell per `feedback_execute_planned_architecture`. Must unwind in redress, not in fictional successor. |
| F4 | Tailwind regex_scan timeout | CHRONIC-RISK (5 tranches) | Routes to BB rule-discovery. BB hasn't opened. |
| F8 | 32 zero-caller substrates | CHRONIC-RISK | W6.2 routed; Audit-B verifies + flips 6 to DELETE. |
| F10 | Watchdog rows | CHRONIC-RISK | Routed to phantom measurement cohort. |
| F1, F12 | Sheets Flat-shape; BC bbnf-buddy | GENUINE | distinct mechanisms. |
| F3, F6, F7, F9 | AUDIT-B splits + outlier dedup + module-split | GENUINE-with-defect | Destination unnamed; cleanup tranche needed. |

The cohort's single architectural insight: **"AZ-V is fictional"**. The chronic-perf gap, the AU floor regression, and the TS-incompleteness all route to a successor letter that does not exist in any plan document. This is precisely the chronic-deferral pattern AZ-IV's non-routable framing was designed to forbid (per `AZ-IV.md §Cross-Tranche Debt`: "If a non-routable item cannot land inside AZ-IV without changing the AZ-IV thesis, the response is a triumvirate review of the thesis — not a new tranche letter.").

### Audit-D — 5 transpositions; single thesis adopted

Audit-D commits to **DIRECT-PROJECTION CODEGEN** as the single thesis and enumerates 5 transpositions with file:line + LOC delta. The top-3:

- **T1 — Eager-as-degenerate-lazy collapse**: kills the `__EAGER_EMPTY_PATH` lie. **−820 LOC** (Audit-D estimate).
- **T4 — Arena/builder template DELETION on value-API path**: closes the 14 BELOW-AU rows (28-65× on bbnf_self/sheets) by inlining `compound_kind_for_layout` at codegen. **−194 LOC**.
- **T5 — `Document::get<T>(path)` reroutes through `parse_with`**: closes the 4196× `bbnf_get_twitter` sonic gap by mechanism. **+30 LOC, −1ms per get_twitter**.

## III — The Decision

**The next tranche is BC, not BA, and not the fictional AZ-V.**

Per `AZ-IV.md §Cross-Tranche Debt`, the recycled BA was earmarked for rule-discovery (Ruler CVC enumerator, VM oracle, ranker). Audit-C confirms the rule-discovery scope is genuinely deferrable — the 4196× perf gap is NOT a rule-discovery problem. It is a **codegen-direct-projection** problem, and that is what BC must close.

**BC's thesis**: direct-projection codegen. The arena/builder template indirection retires; the lazy path becomes canonical; the value-API reroutes through `parse_with`; eager is `parse_with(input, &EMPTY_PATH)`; the AU floor and the sonic-rs gap close through the same mechanism, not two.

**BB (rule-discovery)** opens AFTER BC. The recycled-BA scope (Ruler CVC, VM oracle, ranker, grammar-colocated rewrite dirs, Tranche-H rediscovery ≥80%) routes to BB.

**BD = BC+1**: the cleanup pass that absorbs Audit-A's TRANSPOSE bucket (12 items), Audit-B's module-cluster retirements (3 modules), and Audit-C's CHRONIC-RISK carries (F4 Tailwind, F8 32-substrate cleanup, F10 watchdog rows). BD opens after BC closes.

The trajectory becomes: **AZ-IV (closed) → BC (direct-projection codegen) → BB (rule-discovery) → BD (cleanup pass)**. This eliminates the fictional AZ-V and binds every routed AZ-IV carry to a named, real, tranche-letter destination.

## IV — Immediate Dispatch (before BC opens)

Three concrete commits land BEFORE BC's W0 dispatches, none of which depend on BC's mechanism:

1. **DELETE the 18 verified zero-caller substrates** + populate `SANCTIONED_SUBSTRATES` whitelist (Audit-B's deletion bucket). Closes the F8 carry. Estimated −400 LOC.

2. **Retire the 3 module clusters** (`generate/serialize/`, `generate/regex/phf.rs`, `backend/strategy/` survivors collocated). Closes Audit-B's module-level death cluster. Estimated −350 LOC.

3. **Wire OR delete `merge_path_seed`** — the W3.0 path-egraph-seed rewrites are inert. Either invoke them at the canonical egraph saturation site OR delete `path_seed.rs` + the `merge_path_seed` loader. Closes Audit-B's first real missing wire. The decision goes with whichever the egraph saturation surface naturally accepts; if neither is clean, the rewrites belong in the BB rule-discovery tranche (they were always seed candidates for that scope).

Total: ≈ −750 LOC, three commits. None create a new mechanism. All close existing carries through DELETION or honest WIRING.

After these land, BC opens.

## V — BC Tranche Shape

**Letter**: BC (recycled — see `AZ-IV.md §Cross-Tranche Debt` for letter-recycling discipline; BC was previously bbnf-buddy per memory `project_bbnf_buddy.md`. Either bbnf-buddy moves to BD or BE, or this letter routes to a new letter; the orchestrator picks at BC open.)

**Thesis**: Direct-projection codegen. The lazy path is canonical; eager is its degenerate case.

**Waves** (6 + close):
- BC.W0 — Truth & regen baseline (mirrors AZ-IV.W0 pattern; record current `post-AZ-IV.json` rows as floor; resolve worktree fixture symlink contract per W6.2 known miss).
- BC.W1 — Eager-as-degenerate-lazy collapse (Audit-D T1). The `parse()` entry is rewritten as `parse_with(input, &EMPTY_PATH)`. Per-grammar phantom marker types replace the hardcoded `markers::Json` in eager bodies. The HRTB `__P: for<'__c> PathSchema<'__c>` propagates everywhere or simplifies; resolve at codegen-time.
- BC.W2 — Cursor consult unification (Audit-D T2): `cursor.match_field` + `match_index` + `decide` collapse into one `cursor.consult(&ParsedSegment)` polymorphic on the path's segment alphabet. Eliminates one orthogonal call shape (`feedback_no_orthogonal_codepaths`).
- BC.W3 — Arena/builder template retirement on value-API path (Audit-D T4). The W5 dedup template stays for the 5 simple grammars (it works there); the 4 outlier grammars (JSON, CSS L4, Sheets, BBNF) get direct-projection codegen. The AU floor 18/19 BELOW closes here.
- BC.W4 — `Document::get<T>(path)` reroutes through `parse_with` (Audit-D T5). The 4196× `bbnf_get_twitter` sonic gap closes here. Same-harness `bbnf_get_*` ≤ 5× `sonic_get_*` becomes a hard-met gate, not a routed carry.
- BC.W5 — TS aggregate-as-iterable fix (W1 backend-ts gap). `runtime::ts::emit_value` for object/array shapes emits an iterable JS array of pairs/elements, not a span over input bytes. The W5.2 `ts_node_execute` test goes GREEN. F5 closes through MECHANISM.
- BC.W6 — Measurement & close. Same-harness sonic-rs floor MET; AU floor MET (or named per-row blocker with profile evidence); tailwind WATCHDOG resolves through direct-projection (Pratt + unordered shape paths) or routes EXPLICITLY to BD with named close criterion.

**Hard Gates** (target ≥ 20):
- Direct-projection lazy lane: `bbnf_get_twitter ≤ 5× sonic_get_twitter` MET on same-harness comparison.
- AU floor: 19/19 rows at-or-better than `post-AU.json`.
- Cursor consult unification: `cursor.match_field` + `match_index` deleted; `cursor.consult` is the only call.
- Eager-as-degenerate: `crate::backend::rust::emitter::grammar::emit_parse_body_struct_direct` calls into the lazy emit body with an empty path; the `__EAGER_EMPTY_PATH` LazyLock dies.
- Phantom-marker per grammar: `markers::Json` no longer appears in 9 generated bodies; per-grammar phantom marker is correct.
- Per-grammar `__path_plan` modules retain `PATH_PLAN` static but the local `Decision` / `SegmentKind` re-export goes (single source from `crate::path::cursor`).
- TS aggregate-as-iterable: `ts_node_execute` GREEN on twitter.json.
- Substrate-audit test: GREEN (zero zero-caller substrates after deletion-bucket-18 + sanction-whitelist).
- Workspace nextest: 100% pass; no LSP integration timeout (resolve worktree fixture contention from W6.2's known miss).
- Per-grammar arena/builder: 4 outlier grammars retire dedicated modules in favor of direct-projection codegen.

**Non-Routable Carries** (the disciplined backbone — none route to a successor):
- Every Audit-C MASKED-DEFERRAL closes inside BC. F2 (sonic gap) → W4. F5 (TS Node) → W5. AF (AU floor) → W3.
- Every Audit-A LegacyPath / `__EAGER_EMPTY_PATH` / AscentStrategy finding closes inside BC.
- Every Audit-D T1-T5 transposition lands inside BC.

## VI — BB Tranche Shape (after BC)

**Letter**: BB (recycled — rule-discovery scope from AZ-IV's recycled-BA-routing; AZ-IV §Cross-Tranche Debt names this exact reroute).

**Thesis**: Rule discovery. Ruler CVC enumerator + VM oracle on residue + ranker + Class-1/2/3 tiering. Grammar-colocated rewrite dirs. Synthetic-grammar extensibility.

**Hard Gates** (target ≥ 15):
- Tranche-H rediscovery ≥ 80%.
- ≥ 5 accepted rules per production grammar (JSON, CSS L4, Sheets, BBNF).
- `merge_path_seed` consumed at the canonical egraph saturation site (the W3.0 path-shape rewrites become BB's seed bag).
- Tailwind regex_scan timeout closes via emitted scanner path (this is the F4 carry; routes here because rule-discovery generates the regex rewrites that close it).

## VII — BD Tranche Shape (cleanup pass after BB)

**Thesis**: Architectural cleanup absorbing Audit-A TRANSPOSE bucket + Audit-B module-cluster retirements + Audit-C residual CHRONIC-RISK.

**Hard Gates**:
- AUDIT-B routed splits land (`runtime/css_l4/builder.rs`, `passes/types/mod.rs`, `recognizers/dta.rs` → `grammar_facts.rs` further split, `csp_strategy/mod.rs`).
- Worktree fixture symlink contract codified (W6.2 known miss).
- The 4 outlier grammars (JSON/CSS L4/Sheets/BBNF) retire any remaining template indirection BC didn't subsume.
- Samply 7-artefact contract per `docs/instructions/PROFILING.md` lands as the canonical close-artefact discipline (rather than environmentally-gated as in AZ-IV).

## VIII — One-Path Discipline

For every "two ways to do X" surviving AZ-IV, BC commits to ONE WAY:

| Currently two | One way |
|---|---|
| Eager `parse(input)` + lazy `parse_with(input, &path)` | `parse_with(input, &path)`; `parse(input) = parse_with(input, &EMPTY_PATH)` |
| Cursor `match_field` + `match_index` + `decide` | `cursor.consult(&ParsedSegment)` |
| Per-grammar `__path_plan` re-exports `Decision`/`SegmentKind` | Single source at `crate::path::cursor`; per-grammar holds only `PATH_PLAN` static |
| `arena_template` + per-grammar `arena.rs` | Direct-projection codegen for outliers; template for simple grammars (proven; keep) |
| Path-typed `TypedPath<G,T>` + lower to `LegacyPath` for `Document::get` | `Document::get<T>(typed_path)`; `LegacyPath` retires |
| `__EAGER_EMPTY_PATH: TypedPath<Json, &str>` for all grammars | Per-grammar phantom marker constructed at codegen time; honest types |
| `Option<&mut PathCursor>` parameter | `&mut PathCursor` mandatory; eager passes `&mut PathCursor::eager()` |

## IX — Performance Closure

The 4196× `bbnf_get_twitter` gap is not "make AST descent faster". sonic-rs scans bytes; we descend the AST. The gap closes through **mechanism inversion**: when the path projects to a single leaf, the lazy parser does NOT materialize a document at all — `parse_with(input, &path)` short-circuits at the first segment match, yields the leaf, and discards the in-flight builder state without ever calling `finalise`. The current implementation builds the document then walks the path. Inverting this — leaf-extraction-before-finalise — is the BC.W4 mechanism. Sonic-rs's 332ns is the architectural target; ≤ 5× routes the gap and BC.W4 closes it.

## X — Recommended Next Step

Dispatch the three immediate commits (Section IV). After they land, the orchestrator opens BC.W0 with the wave-shape locked above. Recycled-BA's pre-existing scope routes to BB after BC closes. The fictional AZ-V is removed from all close-state docs.

This synthesis closes the post-AZ-IV audit cohort. Every chronic-deferral surfaced has a named, real destination; every legacy survivor has a deletion plan; every architectural transposition has a wave-and-mechanism binding. ONE path forward.
