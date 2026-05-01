# AZ-IV Gestalt Plan

AZ-IV is the union tranche. It is the consolidation pass that makes the post-AZ-III architecture executable as one system **and** absorbs the typed compile-time `path!` macro + the lazy bail-out parse + the per-grammar value-API consolidation + the TS binding into one tranche. The BA letter is recycled for the post-AZ-IV residual: pure rule discovery (Ruler / VM oracle / ranker). BB is subsumed.

## Current Architecture

The desired architecture is already chosen:

1. Grammar files and manifest metadata define the parser family.
2. The compiler lowers grammar into IR facts, type obligations, recognizer decisions, CSP decisions, and egraph nodes.
3. Rust generation emits direct struct materialization.
4. Runtime views and formatters consume those structs.
5. Benchmarks and parity tests compare generated parsers against external oracles.

The problem is not absence of substrate. The problem is divergence between selected substrate and consumed substrate, plus two forward plans whose requirements were at risk of being dropped when their stale mechanisms were rejected.

## Active Contradictions

1. BA's typed `path!` macro and lazy bail-out parse were planned but never landed; the current `Path<'a>` is an untyped slice. Eager-then-walk lazy lane is 2953x slower than sonic-rs `get`. `parse()` materializes the full tree before path resolution.
2. `xtask regen` is canonical, yet strict regeneration is red for most manifest grammars.
3. CSP decisions exist, but selected engines/layout/dispatch choices still pass through sidecars or independent emitter ladders.
4. Rewrite/ruler storage exists, but loaded rules do not drive production egraph saturation/codegen — `RuleSet` is loaded into an `eprintln`-only sink.
5. DTA is live as a fact source, but comments still describe walker/tape runtime emission. `dfa_codegen.rs` is the regex-scan adapter, not a DFA codegen module.
6. Runtime CSS color has a current typed model, but `backend/rust/view/color.rs` (290 LOC, zero production consumers) still ships as a shim.
7. TS backend builds, but tests prove only string-presence; no Node-execute proof; no TS template-tag binding for `path!`.
8. Sibling repositories still carry derive (parse-that bootstrap + 2 Cargo.lock entries), csp-solver canonical-source split (bbnf-lang vs csc411 with 22 shared files diverging), npm staleness, and docs-sync drift.
9. Per-grammar arena/builder pairs (9 of them) re-encode the rule vocabulary the `StructRegistry` already knows; `from_rule_name(&str) -> Kind` impls + `(layout.kind, rule_name)` builder dispatches violate grammar generality.
10. Substrate-with-consumer rule is declared but unenforceable; 5 WIRED-NOT-CONSUMED + 3 DEAD substrates surfaced by Babbage's third-pass audit.
11. 118 failing tests at AZ-III close (workspace nextest 92.1 % pass); 36 `#[ignore]`d tests; the failure backlog has compounded across 4 tranches without close-discipline enforcement.

## One-Path Target

AZ-IV accepts only one route:

```text
grammar + manifest
  -> IR facts and obligations
  -> CSP/egraph/recognizer decisions
  -> generated StructDirect parser
  -> typed runtime document/view
  -> oracle parity and benchmark evidence
```

Any surface outside that route is either:

- used by this route;
- internal diagnostic/dev tooling with explicit docs and tests;
- deleted or frozen as historical.

The route is grammar-derived end-to-end. Production runtime never branches on a literal grammar parser-struct ident or a literal rule-name string outside `#[cfg(test)]`; discriminators come from `StructRegistry`, `TypeDesc`, `FactAuthority`, manifest metadata, or generated projection tables. Per-grammar runtime arena/builder duplication is the one-pass migration target inside W1 — `from_rule_name(&str) -> Kind` impls retire in favour of registry-projected `compound_kind` reads, and the substrate.rs JSON-builder fallback retires in favour of a hard panic at construction time.

## BA/BB Absorption Rule

BA's pre-recycle scope (typed compile-time `path!`, lazy bail-out parse, host bindings) and BB's perf/value/struct-projection scope (per-grammar value-enum dedup, sonic-rs same-harness floor) are absorbed into AZ-IV. The BA letter is recycled for the post-AZ-IV residual: pure rule discovery. BB is subsumed.

Concrete absorption:

- **W2 - Path IR + Typed Path<G,T> + AscentStrategy** lands BA's pre-recycle W0+W1 path-IR scope: `TypedPath<G, T>`, `path!` proc-macro at compile time, `path_check` IR pass after `project_types`, inline-trace sidecar (source rule names always resolve), bespoke path lexer in `bbnf-regex`, hybrid-sidecar `AscentStrategy` with reversal seam, variant-selection step on typed enums, wildcard returns lazy `Iter<Item = T>` (`.with_anchors()` + `.collect()` adapters).
- **W3 - Lazy Bail-Out Parse** lands BA's pre-recycle W1 stretch (sonic-class lazy parse): `parse_with(input, &path)` is a parse mode that consumes `TypedPath<G, T>` and skips subtrees the path does not visit. Floor: JSON + CSS L4 + Sheets + BBNF. Lazy + eager share generated code; entry-point dispatch is the only divergence. Lazy mode silently elides errors past the path's reach (documented contract).
- **W5 - TS Binding + Value-API + Substrate Audit** lands BA W2's TS template-literal tag (`crates/bbnf-path-ts/` cdylib + wasm-bindgen) **and** BB's per-grammar value-enum dedup (structural skeleton only — typed `*Value` enums survive untouched per `feedback_preserve-rich-ast`) **and** the permanent `substrate_audit.rs` CI test that prevents future "substrate without consumer" regressions.
- **W6 - Measurement + Close** lands the same-harness sonic-rs / lightningcss / simdjson / cssparser comparisons; the AU 17-row floor stays in the `floors` block of `post-AZ-IV.json` per `docs/benchmarks/SPEC.md`. Lazy lane (`bbnf_get_twitter`) closes ≤ 5x sonic-rs same-harness; eager parity-or-better.

The post-AZ-IV residual letter is **BA (recycled)**: rule discovery (Ruler CVC enumerator, VM oracle on residue, ranker, Class-1/2/3 tiering, `crates/ir/src/rewrites/`, grammar-colocated `rewrites/*.ron` per grammar, Tranche H rediscovery ≥ 80 %, ≥ 5 accepted rules per production grammar). BB's old wave structure absorbs into recycled BA's wave structure.

Old BA wave specs preserve at `docs/tranches/BA/historical/` for archaeology; the new BA is the rule-discovery successor.

Product expansion outside the AZ-IV scope waits until AZ-IV closes cleanly. The semantics themselves are AZ-IV scope. The chronic-deferral pattern (substrate-with-consumer declared MET on substrate alone; sibling-repo work has no gate; stale-but-reachable code lacks deletion deadline) ends here.

## Deletion Bias

AZ-IV deletes before adding:

- no `*_v2` modules;
- no compatibility feature flags;
- no restored derive pipeline;
- no DTA walker fallback;
- no second path-query crate;
- no generated tape/view bridge;
- no "consumer later" hooks;
- no `from_rule_name(&str) -> Kind` arm-list per grammar (registry projects);
- no `(layout.kind, rule_name)` builder dispatch (registry projects);
- no `dfa_codegen` misnomer (it's the regex-scan adapter; rename or fold);
- no `backend/rust/view/color` shim (CSS uses `runtime::css_l4::CssColor`);
- no `recognize_*_legacy` patterns (rename or migrate Pratt then delete);
- no `substrate_path` JSON-builder fallback (panic on invalid binding).

If deletion is unsafe because a current consumer exists, the wave must name the consumer and refactor the surface to match its real role.

## Close Shape

AZ-IV closes only when:

1. docs and code agree on the active architecture;
2. regen and parity are coupled and green;
3. semantic parity is derived from type facts and generated projections, not normalizer escape hatches or rule-name tables;
4. existing optimization substrates produce emitted/runtime effects across their full denominator (machine-checkable via the permanent `substrate_audit.rs` CI test);
5. legacy claims are deleted or renamed;
6. benchmark rows beat post-AU/post-AZ floors and are measured, not watchdog-routed; the AU 17-row floor stays in the `floors` block of `post-AZ-IV.json`; row count grows as lazy/path/TS lanes add new bench rows;
7. BA's pre-recycle scope (typed `path!`, lazy bail-out, TS binding, AscentStrategy) and BB's perf/value/struct-projection scope are absorbed and landed inside AZ-IV — none route to a successor letter; old BA preserves at `docs/tranches/BA/historical/`; new BA opens after AZ-IV close for pure rule discovery;
8. every non-routable carry (per `AZ-IV.md` §Non-Routable Carries) closes inside AZ-IV with cited evidence — none route to a successor letter;
9. dev-iteration baseline gate passes: cold and warm walls for `cargo iter-check`, `cargo iter-test-leaf`, `cargo bench-iter-json --no-run`, `cargo nextest run --workspace --cargo-profile ax-iter`, and `cargo xtask regen --check` are recorded in `W0-dev-baseline.txt` with no regression vs the AZ-III baseline;
10. workspace nextest is 100 % pass — fail-count zero, every `#[ignore]` carries owner + deadline-commit + reason + ticket, every deleted test carries per-test commit-body justification per W1.9 format.

## Wave Sequence (canonical)

The 7-wave shape is the canonical execution sequence:

1. **W0 - Truth + Canonical Regen** (5 parallel + sub-units for failing-test census + GESTALT excision).
2. **W1 - Grammar Generality + Test Redress** (5 parallel; merges overfit elimination with full test redress per B1).
3. **W2 - Path IR + Typed Path<G,T> + AscentStrategy** (5 parallel; absorbs BA pre-recycle W0+W1 path-IR scope).
4. **W3 - Lazy Bail-Out Parse** (5 parallel; absorbs BA pre-recycle W1 stretch + BB perf items; floor: 4 production grammars).
5. **W4 - Optimization Substrate Activation** (5 parallel; deletes unconsumed `RuleSet` + `egraph::ruler::*`; BA recreates clean).
6. **W5 - TS Binding + Value-API + Substrate Audit** (5 parallel; absorbs BA W2 + per-grammar arena/builder dedup + permanent substrate-audit test).
7. **W6 - Measurement + Close** (3 parallel; post-AZ-IV.json per SPEC.md; samply 7-artefact contract; close-honesty checklist; FINAL.md).

Six-agent ceiling preserved (max 5 writer slots per wave); sub-units sequence inside owner worktrees per the AZ-IV.md Disjointness rules. HARD CAP defaults research 20 / plan 15 / redress 30 / audit 25; expand on overrun (no pre-allocated triumvirate budget); triumvirate auto-triggers fire on JSONL >15min quiet, first-pass-no-commit, 3-loop-iter, scope-reveal.
