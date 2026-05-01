# AZ-IV Progress

**Status**: planned
**Opened from**: AZ-III `TERMINAL_WITH_CARRIES`
**Current base**: record at first implementation dispatch
**Brittleness window**: none

## Wave Status

| Wave | Status | Evidence | Notes |
|---|---|---|---|
| W0 - Truth And Canonical Regen | planned | pending | strict regen, workspace truth, doc truth, egraph `Map`, metadata gates |
| W1 - Runtime Surface And Semantic Parity | planned | pending | BA typed path/query semantics, type-inference parity, CSS/Sheets/JSON/BBNF/TS parity, shape generality |
| W2 - Optimization Substrate Activation | planned | pending | BB rewrite/ruler denominator, CSP/regex authority, shape_dict/SIMD, Pratt/view, legacy deletion |
| W3 - Measurement And Close | planned | pending | post-AU/post-AZ/sonic-rs performance floors, substrate denominator ledger, workspace gates, close-honesty |

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
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/W0-ba-bb-coverage.md` | BA/BB seed coverage ledger created so W0 cannot lose path/query or rewrite/ruler requirements |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-cantor.md` | plan-coherence + spec-adherence: 6 paste-ready amendments (W2 within-wave overlap split, cross-wave Disjointness notes, empty-return uniformity, three-loop trigger, HARD CAP block, generated-size-budget, hard-gate evidence paths) |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-heisenberg.md` | legacy + naming census: 935 walker/tape/DTA refs; `dfa_codegen.rs` confirmed misnamed (rename to `regex_scan_adapter.rs`); `view/color.rs` 290 LOC zero consumers (delete); `substrate_path` silent fallback (panic); `recognize_*_legacy` rename; 36 `#[ignore]`d tests; 13 production `eprintln!` sites |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-babbage.md` | substrate matrix: 26 substrates audited (9 CONSUMED, 6 PARTIAL, 5 WIRED-NOT-CONSUMED, 3 DEAD, 4 UNDERUTILIZED); top 5 gaps include ruler enumerate/oracle/residue, `RuleSet` load, `shape_dict_selection`, `type_obligations`, CSP-overridden alt_strategy |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-fermat.md` | grammar generality + overfitting: 11 literal parser-struct idents, 7 `from_rule_name` impls, 1 silent JSON fallback, 3 BBNF-source-byte recovery fns; 3 new W1 hard gates; 6 new W1 sub-units; CI static scan `tests/no_grammar_name_branch.rs`; `alt_dispatch named_color` confirmed grammar-general |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-diophantus.md` | sibling-libs: 4 active `bbnf_derive` edges (parse-that bootstrap + wasm/parse-that Cargo.lock); csp-solver split bbnf-lang vs csc411 with 22 shared files diverging; npm parse-that/typescript stale 5.5 days; 71 in-tree deprecation markers; W0.2.b csp-solver canonical + W0.2.c bbnf_derive eradication |
| 2026-05-01 | planning | `docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-boole.md` | dev iteration speed + 5-tranche chronic-deferral: bbnf-bootstrap >130s cache nuke is #1 bottleneck; 13 of 15 carries chronic ≥3 tranches (PatternAnnotations 5+, view/color 5+, Sheets 4, regen drift 4); meta-causes "substrate-with-consumer declared MET on substrate alone", "sibling-repo work has no gate", "stale-but-reachable lacks deletion deadline"; AZ-IV.md §Non-Routable Carries section; W0 dev-baseline gate; W3 watchdog-discipline tighten |

## Close-Honesty Parking Lot

Items cannot remain here at close. Each must be landed, retired, or routed to a named destination. The 13 non-routable carries (`AZ-IV.md` §Non-Routable Carries) appear here too — none of them may route to a successor letter.

| Item | Current owner | Disposition |
|---|---|---|
| AZ-III strict regen drift (4 substrate divergences) | W0 - Truth And Canonical Regen | planned (non-routable) |
| Egraph `Map` wrapper preservation | W0 - Truth And Canonical Regen | planned (non-routable) |
| Sheets parity current count mismatch (115/133 regression) | W1 - Runtime Surface And Semantic Parity | planned (non-routable) |
| TS backend executable parity | W1 - Runtime Surface And Semantic Parity | planned (non-routable) |
| Tailwind regex timeout | W2 - Optimization Substrate Activation | planned (non-routable) |
| Watchdog benchmark rows (cross-profile) | W3 - Measurement And Close | planned (non-routable) |
| BA/BB/GESTALT/codegen doc drift | W0 - Truth And Canonical Regen, W3 - Measurement And Close | planned |
| BA typed path/query product commitments | W1 - Runtime Surface And Semantic Parity | planned |
| BB Ruler/VM-oracle/ranker full program | W2 - Optimization Substrate Activation | planned (non-routable) |
| post-AU/post-AZ and sonic-rs performance floors | W3 - Measurement And Close | planned (non-routable) |
| full substrate denominator ledger | W2 - Optimization Substrate Activation, W3 - Measurement And Close | planned (non-routable) |
| `dfa_codegen.rs` rename + DTA naming debt (935 refs) | W2 - Optimization Substrate Activation | planned (non-routable) |
| `backend/rust/view/color.rs` shim deletion + `runtime/view.rs:35` re-export retire | W1 - Runtime Surface And Semantic Parity | planned (non-routable) |
| `substrate_path` silent JSON-builder fallback | W1 - Runtime Surface And Semantic Parity | planned (non-routable) |
| `from_rule_name(&str)` impls (7 grammars) | W1 - Runtime Surface And Semantic Parity | planned (non-routable) |
| `(layout.kind, rule_name)` builder dispatch (JSON + CSS L4) | W1 - Runtime Surface And Semantic Parity | planned (non-routable) |
| `EmitStrategy::for_grammar` 9-arm allowlist | W0 - Truth And Canonical Regen | planned |
| `recover_modifier`/`recover_binary_op` BBNF source-byte scanners | W1 - Runtime Surface And Semantic Parity | planned |
| `recognize_*_legacy` + PatternAnnotations migration | W2 - Optimization Substrate Activation | planned (non-routable) |
| `bbnf_derive` residue (parse-that + wasm + xtask doc-links) | W0 - Truth And Canonical Regen | planned (non-routable) |
| csp-solver canonical-source split (bbnf-lang vs csc411, 22 shared files diverge) | W0 - Truth And Canonical Regen | planned |
| `bootstrap-bbnf.sh` cache-nuke (>130s recurring wall) | W0 - Truth And Canonical Regen | planned |
| Workspace 1527-test 290s cycle + 118 known-failing tests | W0 / W3 | planned |
| 36 `#[ignore]`d tests | W1 - Runtime Surface And Semantic Parity / W2 - Optimization Substrate Activation | planned |
| 13 production `eprintln!` instrumentation sites | W2 - Optimization Substrate Activation | planned |
| WIRED-NOT-CONSUMED substrates (ruler, RuleSet, shape_dict_selection, type_obligations, CSP override) | W2 - Optimization Substrate Activation | planned (non-routable) |
| DEAD substrates (`emit_dfa_inline_body`, codegen-paths.md stale IR-pass list) | W2 - Optimization Substrate Activation | planned (non-routable) |
