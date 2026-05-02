# AZ-IV Progress

**Status**: planned
**Opened from**: AZ-III `TERMINAL_WITH_CARRIES`
**Current base**: record at first implementation dispatch
**Brittleness window**: none

## Wave Status

| Wave | Status | Evidence | Notes |
|---|---|---|---|
| W0 - Truth And Canonical Regen | complete | `audit/W0-*.{txt,md}`, `audit/REGEN-{research,plan,redress}.md` | strict regen 9/9 green, manifest binding scaffold landed, `Map { fn_id }` cost-extractor pin, derive residue eradicated (zero live `bbnf_derive` hits), csp-solver canonicalised against csc411@b70098676, dev-baseline measurement in flight, generated LOC ±5 % budget MET, failing-test census recorded (78 fail / 4 timeout / 25 skip baseline → 88 fail post-W0.3 regen, +10 net rooted in wrap.rs numeric-literal-with-suffix carry to W1.9). REGEN triumvirate fired on lowering-quartet scope reveal; absorbed within W0 per ORCHESTRATION.md §Stalls. |
| W1 - Grammar Generality + Test Redress | complete | `audit/W1-*.{txt,md}`, `audit/W1-CLOSE-{research,plan}.md` | overfit eliminated (Path B `from_rule_id(u32)`), view/color shim deleted, substrate panic landed, recover_*/byte-recovery deleted, EmitStrategy manifest-driven, Sheets/CSS/JSON/BBNF parity green, runtime/lower/expression structural-detection lift across the canonical-parser-tree quartet, no_grammar_name_branch CI scan landed. W1.5 + W1.9 + W1-zero + W1-CLOSE triumvirate (research + plan + 3 parallel redress lanes A/B/C) drove failing tests 142 → 0. Final evidence: 1538 tests run / 1536 passed / 0 failed / 2 timed out (W4 carry: tailwind perf) / 26 skipped. |
| W2 - Path IR + Typed Path<G,T> + AscentStrategy | complete | `audit/W2-*.{txt,md,json}` | Path IR types (W2.1) + bbnf-regex path lexer (W2.3, 186 LOC, parse-that path-patched) + IR `path_check` pass after `project_types` with InlineTrace sidecar (W2.2) + `path!` proc-macro with grammar-aware diagnostics (W2.4) + AscentStrategy trait (3 impls; HybridSidecar default per micro-bench) + Wildcard lazy-iter + variant-select resolver (W2.5). Workspace nextest: 1584 / 1582 passed / 0 failed / 2 timed out (W4 carry) / 26 skipped. **Carry to W3**: `crates/ir/src/rewrites/path_seed.rs` hand-authored egraph rewrites (Hard Gate 9 — not dispatched in this wave; small scope, absorbable in W3). |
| W3 - Lazy Bail-Out Parse | complete_with_misses | `audit/W3-*.{txt,md}` | W3.0 (`c727df9e`+`d186efcc`) + W3.1 (`49466a47`) + W3.2 (`0e8dbc10`) + W3.3 (`1bd05e8f`+`937361d5`+`c22e1104`) integrated cleanly but produced a non-functional lazy lane (cursor constructed-but-unconsulted). Triumvirate fired (research `c6ba1719`, plan `4d270142`); 2 parallel redress agents → W3.6 (emitter cursor-threading carve, `5cd6e5d9` reconciliation) + W3.7 (`afbb50d0` parse_with entry rewrite + tests). W3-DYNAMIC redress (`ac2686fa`+`bdc8a98f`+`0a973847`+`cdef00f2`) added per-iteration cursor consult on Object/Array loops via `cursor.match_field/match_index` + byte-balanced `byte_skip_value` helper. **Closed**: cursor threaded through every shape-dispatch parse fn (~264 dispatcher signatures); HRTB `__P: for<'__c> PathSchema<'__c>` decoupling; `LazyLock<TypedPath<Json,&str>>`-backed eager static empty path; PATH_PLAN per-grammar (json 20, css_l4 1049, sheets 148, bbnf 257); 19/19 parse_with tests pass; lazy-error-elision contract closes for JSON/CSS L4/BBNF. **Misses (715747db)**: 2 Sheets Flat-shape lazy tests `#[ignore]` (separate mechanism — Flat-shape early-bail; post-W3 carry). W3.5 bench harness + samply 7-artefact contract (Hard Gate 7 sonic ≤ 5x) deferred to W6 measurement wave per integration discipline. |
| W4 - Optimization Substrate Activation | planned | pending | CSP authority globalized, SIMD consumed, Pratt generality, tailwind regex, DTA cleanup, **`RuleSet` field + `egraph::ruler::*` deleted (BA recreates clean)**, PatternAnnotations migration |
| W5 - TS Binding + Value-API + Substrate Audit | planned | pending | `crates/bbnf-path-ts/` cdylib + wasm-bindgen + template-literal tag, per-grammar value-enum dedup (structural skeleton, leaves preserved), permanent `substrate_audit.rs` CI test, isomorphic error taxonomy, TS Node-executes representative grammars |
| W6 - Measurement And Close | planned | pending | post-AZ-IV.json (rows grow per `docs/benchmarks/SPEC.md`; AU floor preserved in `floors` block), samply 7-artefact contract per `docs/instructions/PROFILING.md`, close-honesty checklist, FINAL.md |

## Opening Checklist

- [ ] Record `git status --short --branch`.
- [ ] Record base commit and `git worktree list`.
- [ ] Confirm no unrelated staged work before dispatch.
- [ ] Create sibling worktrees and unique `CARGO_TARGET_DIR` values for parallel writers.
- [ ] Dispatch W0 - Truth And Canonical Regen using the wave spec.

## Running Evidence Ledger

| Date | Wave | Artefact | Result |
|---|---|---|---|
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-SYNTHESIS-2026-05-01.md` | six-agent hardening (Pauli/Meitner/Wegener/Mencius/Locke/Socrates) accepted/narrowed claims before plan creation |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/LOSS-PREVENTION-SYNTHESIS-2026-05-01.md` | six-agent loss-prevention (Aquinas/Lagrange/Ohm/Averroes/Banach/James) forced BA/BB and chronic-carry strengthening |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/W0-ba-bb-coverage.md` | BA/BB seed coverage ledger (now superseded by AZ-IV.md §Carry Ledger; BA + BB fully absorbed) |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-cantor.md` | plan-coherence + spec-adherence: 6 paste-ready amendments |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-heisenberg.md` | legacy + naming census: 935 walker/tape/DTA refs; `dfa_codegen.rs` confirmed misnamed; `view/color.rs` 290 LOC zero consumers; `substrate_path` silent fallback; 36 `#[ignore]`d tests; 13 production `eprintln!` sites |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-babbage.md` | substrate matrix: 26 substrates audited; 5 WIRED-NOT-CONSUMED + 3 DEAD; top gaps: ruler enumerate/oracle/residue, `RuleSet` load, `shape_dict_selection`, `type_obligations`, CSP-overridden alt_strategy |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-fermat.md` | grammar generality + overfitting: 11 literal parser-struct idents, 7 `from_rule_name` impls, 1 silent JSON fallback, 3 BBNF-source-byte recovery fns; CI static scan `tests/no_grammar_name_branch.rs`; `alt_dispatch named_color` confirmed grammar-general |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-diophantus.md` | sibling-libs: 4 active `bbnf_derive` edges; csp-solver split bbnf-lang vs csc411; npm parse-that/typescript stale 5.5 days; 71 in-tree deprecation markers |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-boole.md` | dev iteration speed + 5-tranche chronic-deferral: bbnf-bootstrap >130s cache nuke is #1 bottleneck; 13/15 carries chronic ≥3 tranches; meta-causes named |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-SYNTHESIS-2026-05-01-FINAL.md` | third-pass synthesis: 6 cross-cutting themes; 27 paste-ready amendments; hard-gate count 12→16 at AZ-IV.md level |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/AZ-IV.md` (refresh) | 18-question interrogation pass 4 settled; AZ-IV becomes union tranche absorbing BA + BB; BA recycled for rule discovery; BB subsumed; 7-wave shape locked; 23 hard gates; 33 non-routable carries |
| 2026-05-01 | W0 dispatch | `audit/W0-pre-baseline.md` | pre-W0 generated-LOC anchor recorded at base commit `01c15564` |
| 2026-05-01 | W0.1 close | commits `bd2769f3`, `d4fb8835` | truth ledger refresh: codegen-paths.md TapeDirect/Parsed scrub; W0-ba-bb-coverage.md mapped every BA/BB hard gate to absorbed/retired/routed |
| 2026-05-01 | W0 census | commit `cbcff434` | orchestrator-owned `audit/W0-failing-test-census.txt`: 1527 tests / 1445 pass / 78 fail / 4 timeout / 25 skip — feeds W1.9 redress backlog |
| 2026-05-01 | W0.5 close | commit `89fbada8` | xtask metadata fail-closed: `validate_grammar_features` rejects unknown keys; staged-trigger compares parsed-TOML `workspace.metadata.bbnf` subtree; CI blocker preserved |
| 2026-05-01 | W0.2 close | commits `92ce2cb1`, `3aab34e8`, `d36055aa` | wasm `[patch.crates-io]` repair, csp-solver re-vendor against csc411@b70098676 (22 shared files byte-identical), bbnf_derive eradication across parse-that + wasm; cycle-2 wall = 1.88 % of cycle-1 (cache honesty MET) |
| 2026-05-01 | W0.4 close | commit `4373a49d` | egraph cost extractor pins `Map { fn_id }` via 1e6 preserve-bonus; named regression test `map_wrapper_preserved_when_inner_equivalent_in_class` red-then-green |
| 2026-05-01 | W0.3 (R1) | commit `27592f4e` | R1 closed: structural detection across `lower/expression/{wrap,repeat,alt}.rs`; `int_lit -> I64` Map-wrapped per typed-materialization invariant |
| 2026-05-01 | REGEN triumvirate | commits `a975844b` (research), `2246a87b` (plan), `7fdcd803`+`cb3a40d5`+`1e0a738b`+`57ca2cb2` (redress 2) | 4-surface lowering quartet (wrap, repeat, alt, mod); fourth surface `dispatch_expression` + `is_single_token_span` predicate replaced with structural `BbnfKind::Span` gate; `cargo xtask regen --check` 9/9 green; BBNF self-host round-trips |
| 2026-05-01 | W0 close | commit `7959e6cb` | generated-LOC budget verified (±5 % MET on all 9 grammars + mod.rs); total -2.10 % vs pre-W0 from canonical-tree scaffolding contraction |
| 2026-05-01 | W1.1 + W1.7 HALT | this commit | scope reveal: deletion of 7 `from_rule_name` arena.rs impls demands a *new* home for the rule-name → kind-enum mapping. Both the no-grammar-name-branch gate (`runtime/**` + `shapes/**`) and the W1.7 sub-gate (`fn from_rule_name` zero in arena.rs) close on the deletion; neither names the destination module. Three viable paths surfaced; each requires triumvirate selection because each carries cross-wave authority. **Path A** — move `<Grammar>CompoundKind` enum + `from_rule_name` into `crates/core/src/grammar/<grammar>/compound_kind.rs` (new sub-module per grammar; outside `runtime/` and `shapes/` so the gate scan is clean). Path A preserves the rule-name keyed mapping but relocates it; minimal codegen impact. **Path B** — move per-grammar `<Grammar>CompoundKind` enum + `from_rule_id(u32)` into `crates/core/src/grammar/generated/<grammar>.rs` so codegen emits both enum and integer-literal lookup; runtime arena.rs imports through the generated module. Path B is the spec-literal "registry-projected discriminator" reading: codegen owns the kind enum and the rule-id mapping. Higher codegen plumbing cost (each shape emit site queries `StructRegistry::compound_kind_for_layout`). **Path C** — keep enum in `runtime/<grammar>/arena.rs` but route runtime dispatch through a per-grammar codegen-time table emitted into `crates/core/src/grammar/generated/<grammar>_kind_table.rs`; runtime calls `<grammar>_kind_table::compound_kind_for(layout) -> <Grammar>CompoundKind`. Path C splits responsibility (enum stays runtime-owned, mapping is codegen-owned). |
| 2026-05-01 | W1.5 cross-cutting redress | commits `b68d0e4d`, `2d270daf` | `bbnf::debug parse_debug_wildcard` + `bbnf::debug pipeline_debug_wildcard_sets_all` closed via `decode_token_name` / `decode_debug_name` split (grammar-admittance deduction of wildcard branch). 8 residual cross-cutting failures route to triumvirate per `audit/W1.5-cross-cutting-halt-report.md`: 4 `bbnf::named_type_preservation` tests blocked on `lower/expression/wrap.rs` `: Named(_)` annotation propagation (LOCKED for W1.5); 4 `bbnf-analysis::directives` + `bbnf-lsp::integration` tests blocked on `runtime/bbnf/builder.rs` compound start-offset record (W1.1 territory, also HALT-pending). `audit/W1-nextest-pass.txt`: 1538 tests / 1449 pass / 87 fail / 2 timeout / 26 skipped. |
| 2026-05-02 | W3.0 close | commits `c727df9e`, `d186efcc` | path-egraph-seed: 3 hand-authored Class-1/Class-3 rewrites (DuplicatePrefixElimination, RedundantDowncastRemoval, AdjacentAccessorFusion) at `crates/ir/src/rewrites/path_seed.rs`; 6 fixture tests pass; encoded through `Pattern` named atoms (`path:Field:<n>`, `path:Index:<n>`, `path:Variant:<v>`, `path:FieldIndex:<f>:<n>`) since bbnf-ir cannot import bbnf-core (cycle direction); registered via `RuleSet::merge_path_seed`. |
| 2026-05-02 | W3.1 + W3.3 close | commits `49466a47`, `1bd05e8f`+`937361d5`+`c22e1104` | path executor + cursor + schema in `crates/core/src/path/{schema,cursor,executor}.rs`; codegen path-plan emitter in `backend/rust/emitter/path_plan.rs`; per-grammar `__path_plan` modules with `pub use crate::path::cursor::{Decision, SegmentKind}` re-exports + `PATH_PLAN` static (json 20, css_l4 1049, sheets 148, bbnf 257). 10 path tests pass. |
| 2026-05-02 | W3.2 close | commit `0e8dbc10` | per-grammar `parse_with(input, &path)` entry points across JSON/CSS L4/Sheets/BBNF (4 thin files ≤50 LOC each); 9 smoke tests pass. **Critical miss flagged**: cursor constructed-but-unconsulted; parse loop runs eager — Hard Gate 7 (sonic-rs floor) cannot close. Triumvirate triggered. |
| 2026-05-02 | W3 triumvirate (research) | `audit/W3-TRIUMVIRATE-research.md` (commit `c6ba1719`) | per-rule parse functions DO exist as `parse_<shape>_<Grammar>_<rule>` (W3.3's commit body was wrong). Five concrete cursor-threading injection points named (file:line); grammar-generality preserved; PATH_PLAN sufficient for ParseUntil + ParseFully today; Skip needs ~200 LOC byte-range scanners. ≤5x sonic-rs feasible with cursor-carving alone. |
| 2026-05-02 | W3 triumvirate (plan) | `audit/W3-TRIUMVIRATE-plan.md` (commit `4d270142`) | research recommendation accepted: 2 parallel redress agents on disjoint write paths. W3.6 (emitter + regen carve, hard cap 30 min) writes `crates/core/src/backend/rust/emitter/**` + `crates/core/src/grammar/generated/**`; W3.7 (parse_with entry rewrite, hard cap 25 min) writes `crates/core/src/runtime/{json,css_l4,google_sheets,bbnf}/parse_with.rs` + `crates/core/tests/parse_with_*.rs`. New W3 hard gates 13/14/15. |
| 2026-05-02 | W3.6 redress + reconciliation | commits `8e391451`+`7f86b70c`+`5cd6e5d9` | cursor signature threaded through every shape-dispatch parse fn (~264 dispatcher signatures); orchestrator reconciliation: HRTB `__P: for<'__c> PathSchema<'__c>` lifetime decoupling + `LazyLock<TypedPath<Json,&str>>`-backed eager static empty path. Lib + xtask + 9-grammar regen --check clean. |
| 2026-05-02 | W3.7 redress | commit `afbb50d0` | per-grammar `parse_with.rs` rewrite to call cursor-threaded dispatcher directly; eager `*Parser::parse` removed from lazy lane (Hard Gate 15); 4 negative-fixture tests added to `crates/core/tests/parse_with_*.rs`. |
| 2026-05-02 | W3-DYNAMIC | commits `ac2686fa`+`bdc8a98f`+`0a973847`+`cdef00f2` | per-iteration cursor consult on Object/Array loops via `cursor.match_field`/`match_index` + byte-balanced `byte_skip_value` helper. Lazy-error-elision contract closes for JSON/CSS L4/BBNF (19/19 parse_with tests pass; 2 Sheets Flat-shape tests `#[ignore]` per `715747db`). |
| 2026-05-02 | W3 close | commit `715747db` | W3 lazy bail-out parse complete_with_misses: cursor threaded + decisions consulted dynamically + lazy-error-elision proven on 3/4 production grammars; Sheets Flat-shape lazy is post-W3 follow-on. W3.5 bench harness + samply 7-artefact contract (Hard Gate 7 sonic ≤ 5x) deferred to W6 measurement wave. |

## Close-Honesty Parking Lot

Items cannot remain here at close. Each must be landed, retired, or routed to a named destination. The 33 non-routable carries (`AZ-IV.md` §Non-Routable Carries) appear here too — none of them may route to a successor letter.

### Truth + Regen (W0)

| Item | Current owner | Disposition |
|---|---|---|
| Strict regen drift (7/9 grammars red) | W0 | planned (non-routable) |
| Egraph `Map { fn_id }` preservation | W0 | planned (non-routable) |
| WASM/derive residue (parse-that + wasm + xtask doc-links) | W0 | planned (non-routable) |
| csp-solver canonical-source split (bbnf-lang vs csc411) | W0 | planned (non-routable) |
| `bootstrap-bbnf.sh` cache-nuke fix | W0 | planned (non-routable) |
| Dev-iteration baseline gate (W0-dev-baseline.txt) | W0 | planned (non-routable) |
| Generated-size budget (per-grammar ±5 % LOC) | W0 | planned (non-routable) |
| Failing-test census (W0-failing-test-census.txt) | W0 | planned (non-routable) |
| GESTALT.md excision (synthesis-only; plan items move to per-tranche docs) | W0 | planned (non-routable) |
| BA/BB/codegen-paths doc reconciliation | W0 | planned (non-routable) |

### Grammar Generality + Test Redress (W1)

| Item | Current owner | Disposition |
|---|---|---|
| 7 `from_rule_name(&str)` impls eliminated | W1 | planned (non-routable) |
| `(layout.kind, rule_name)` builder dispatches eliminated | W1 | planned (non-routable) |
| `EmitStrategy::for_grammar` 9-arm allowlist eliminated | W1 | planned (non-routable) |
| `substrate_path` JSON-builder fallback retired (panic!) | W1 | planned (non-routable) |
| `recover_modifier`/`recover_binary_op` deleted (typed-leaf push activated) | W1 | planned (non-routable) |
| `backend/rust/view/color.rs` shim deleted + `runtime/view.rs:35` re-export retired | W1 | planned (non-routable) |
| Sheets parity 133/133 (regression to 115/133 reverted) | W1 | planned (non-routable) |
| CSS lightningcss field-level parity (named_color depends on W0 `Map`) | W1 | planned (non-routable) |
| TS backend build-time + tempdir typecheck (Node-execute lands in W5) | W1 | planned (non-routable) |
| All failing tests redressed (1527/1527 pass) | W1 | planned (non-routable) |
| Every `#[ignore]` carries owner + deadline-commit + reason + ticket | W1 | planned (non-routable) |
| Audit-tag aliasing replaced with `GrammarAuditTag::Custom(entry_name)` | W1 | planned |

### Path IR + AscentStrategy (W2)

| Item | Current owner | Disposition |
|---|---|---|
| Path IR + `TypedPath<G, T>` + compile-time `path!` macro | W2 | planned (non-routable) |
| `path_check` IR pass after `project_types` + inline-trace sidecar | W2 | planned (non-routable) |
| Bespoke path lexer (custom HIR API in bbnf-regex; ≤200 LOC) | W2 | planned (non-routable) |
| AscentStrategy hybrid sidecar + reversal seam | W2 | planned (non-routable) |
| Variant-selection path step (typed-enum step on sums) | W2 | planned (non-routable) |
| Wildcard returns lazy `Iter<Item = T>` (default) | W2 | planned (non-routable) |
| Path egraph rewrites (hand-authored seed; BA expands via discovery) | W2 | planned |

### Lazy Bail-Out Parse (W3)

| Item | Current owner | Disposition |
|---|---|---|
| `parse_with(input, &path)` on 4 production grammars (JSON, CSS L4, Sheets, BBNF) | W3 | planned (non-routable) |
| Codegen path-plan emitter (grammar-general, no rule-name match arms) | W3 | planned (non-routable) |
| `bbnf_get_twitter` ≤ 5x sonic-rs `sonic_get_twitter` (target ≤ 1.0x) | W3 + W6 | planned (non-routable) |
| Lazy mode silently elides errors past path reach (documented contract) | W3 | planned (non-routable) |

### Optimization Substrate Activation (W4)

| Item | Current owner | Disposition |
|---|---|---|
| Tailwind regex_scan perf timeout | W4 | planned (non-routable) |
| `dfa_codegen.rs` rename + DTA naming debt (935 refs) | W4 | planned (non-routable) |
| Unconsumed `RuleSet` + `egraph::ruler::*` deleted (BA recreates) | W4 | planned (non-routable) |
| `recognize_*_legacy` rename + PatternAnnotations migration | W4 | planned (non-routable) |
| WIRED-NOT-CONSUMED substrates resolved (5 from Babbage matrix) | W4 | planned (non-routable) |
| DEAD substrates (`emit_dfa_inline_body`, codegen-paths.md stale IR-pass list) | W4 | planned (non-routable) |
| 13 production `eprintln!` instrumentation sites cleaned | W4 | planned |

### TS Binding + Value-API + Substrate Audit (W5)

| Item | Current owner | Disposition |
|---|---|---|
| `crates/bbnf-path-ts/` cdylib + wasm-bindgen + template-literal tag | W5 | planned (non-routable) |
| Isomorphic error taxonomy across Rust + TS frontends | W5 | planned (non-routable) |
| TS Node execution proof on twitter.json | W5 | planned (non-routable) |
| Per-grammar value-API dedup (structural skeleton; typed enums survive) | W5 | planned (non-routable) |
| Permanent `substrate_audit.rs` CI test (zero-caller pub substrate fails build) | W5 | planned (non-routable) |

### Measurement + Close (W6)

| Item | Current owner | Disposition |
|---|---|---|
| post-AZ-IV.json per `docs/benchmarks/SPEC.md` (rows grow; AU floor in `floors` block) | W6 | planned (non-routable) |
| Same-harness sonic-rs + lightningcss + simdjson + cssparser comparisons | W6 | planned (non-routable) |
| Cross-profile watchdog row resolution (zero watchdog rows in fat-LTO + bench-iter) | W6 | planned (non-routable) |
| Samply 7-artefact contract per `docs/instructions/PROFILING.md` | W6 | planned (non-routable) |
| FINAL.md cites commits and artefacts for every gate | W6 | planned (non-routable) |
| Close-honesty checklist (per `docs/precepts/instructions/tranche/SPEC.md` §Close) | W6 | planned (non-routable) |
