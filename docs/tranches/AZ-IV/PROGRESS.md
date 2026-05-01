# AZ-IV Progress

**Status**: planned
**Opened from**: AZ-III `TERMINAL_WITH_CARRIES`
**Current base**: record at first implementation dispatch
**Brittleness window**: none

## Wave Status

| Wave | Status | Evidence | Notes |
|---|---|---|---|
| W0 - Truth And Canonical Regen | planned | pending | strict regen, manifest binding, `Map { fn_id }` preservation, sibling derive eradication, dev-iteration baseline, **failing-test census + GESTALT.md excision** |
| W1 - Grammar Generality + Test Redress | planned | pending | overfit elimination, view/color delete, substrate panic, recover_* delete, EmitStrategy manifest, Sheets/CSS/JSON/BBNF parity green, **all failing tests redressed (fix-with-spec or delete-with-justification)** |
| W2 - Path IR + Typed Path<G,T> + AscentStrategy | planned | pending | source-rule-resolved path checker, `path_check` IR pass after `project_types`, inline-trace sidecar, bespoke path lexer via bbnf-regex HIR, hybrid sidecar AscentStrategy, `path!` proc-macro, compile-time variant-selection step, wildcard `Iter<Item = T>` with `.with_anchors()` |
| W3 - Lazy Bail-Out Parse | planned | pending | path-driven recognizer; floor: JSON + CSS L4 + Sheets + BBNF; lazy + eager same `Option<T>` semantics; lazy mode silently elides errors past path reach (documented contract) |
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
