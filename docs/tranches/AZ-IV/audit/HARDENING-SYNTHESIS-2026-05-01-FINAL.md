# AZ-IV Hardening Synthesis - Pass 3 - Final (2026-05-01)

Final synthesis of the third 6-agent hardening cohort, dispatched after AZ-III closed `TERMINAL_WITH_CARRIES` at HEAD `5211b953` and the AZ-IV planning seed landed at `db8b00ad`. The two prior passes (HARDENING-SYNTHESIS-2026-05-01.md, LOSS-PREVENTION-SYNTHESIS-2026-05-01.md) accepted the plan with narrowed claims; this third pass surfaced structural defects and chronic-deferral root causes the prior cohorts did not.

## Cohort

| Agent | Worktree | Audit doc | Commit | Lines |
|---|---|---|---:|---:|
| Cantor | `bbnf-wt-aziv-w0-harden-cantor` | `HARDENING-2026-05-01-cantor.md` | `c60697fe` (cherry-picked as `fbc05508`) | 280 |
| Heisenberg | `bbnf-wt-aziv-w0-harden-heisenberg` | `HARDENING-2026-05-01-heisenberg.md` | `d7083dd2` (`c4b5f666`) | 543 |
| Babbage | `bbnf-wt-aziv-w0-harden-babbage` | `HARDENING-2026-05-01-babbage.md` | `b36baf6e` (`a60189d6`) | 149 |
| Fermat | `bbnf-wt-aziv-w0-harden-fermat` | `HARDENING-2026-05-01-fermat.md` | `1d47b646` (`e4068969`) | 405 |
| Diophantus | `bbnf-wt-aziv-w0-harden-diophantus` | `HARDENING-2026-05-01-diophantus.md` | `0347ead8` (`67ea4e98`) | 298 |
| Boole | `bbnf-wt-aziv-w0-harden-boole` | `HARDENING-2026-05-01-boole.md` | `ac3e9ed1` (`bf533042`) | 686 |
| **Total** | 6 sibling worktrees | 6 audit docs | 6 cherry-picks | **2361 LOC** |

Each agent ran with HARD CAP 25 min (audit lane default per `ORCHESTRATION.md` §Triumvirate). Read-only briefs except for the single audit-doc creation. Sibling worktrees + per-agent `CARGO_TARGET_DIR` honoured the lock-contention discipline. Cantor returned synchronously at the dispatch tool boundary; the other five returned via task-completion notifications.

## Disposition By Lane

| Lane | Findings | ACCEPT | NARROW | REJECT/HARD-FAIL | Plan amendments produced |
|---|---:|---:|---:|---:|---:|
| Cantor (plan coherence) | 19 | 7 | 6 | 6 | 6 paste-ready blocks |
| Heisenberg (legacy/naming) | 16 | 1 | 13 | 2 | 4 paste-ready blocks |
| Babbage (substrate matrix) | 26 substrates + 10 numbered | 9 CONSUMED | 6 PARTIAL + 4 UNDERUTILIZED | 5 WIRED-NOT-CONSUMED + 3 DEAD | 3 paste-ready blocks |
| Fermat (grammar generality) | ~35 census hits | 5 GRAMMAR-DERIVED clean | 6 OVERFIT-CARRY + 3 OVERFIT-BUT-NEEDED | 4 OVERFIT-HARD | 7 paste-ready blocks |
| Diophantus (sibling libs) | 8 hard findings + 14-row sibling table | 0 ACCEPT | 4 NARROW | 4 ROUTE-NEW-SUB-UNIT | 3 paste-ready blocks |
| Boole (dev speed + 5-tranche genealogy) | 3 bottlenecks + 13/15 chronic | 2 IMPROVE | 0 NARROW | 13 NON-ROUTABLE | 4 paste-ready blocks |
| **Total** | **~100 findings + 26 substrates** | | | | **27 paste-ready amendment blocks** |

## Cross-Cutting Themes

### Theme 1 — Substrate-with-consumer was declared MET on substrate alone

Babbage's matrix proves the systemic pattern: `egraph::ruler::{enumerate, oracle, residue}` has zero production callers (only test/example consumers); `CompileOptions::rewrites` is loaded into an `eprintln`-only sink at `pipeline/compile.rs:560-573`; `GrammarIR::shape_dict_templates` and `GrammarIR::shape_dict_selection` are NEVER read by any backend emitter; `GrammarIR::type_obligations` (the Vec produced by W3a.3) is drained only by tests. AZ-IV.W2.9 hard gate now enumerates these explicitly and the substrate denominator artefact MUST list a disposition (consumer, deletion, or scope-reveal) for each before close.

This pattern recurs across at least four prior tranches (B5 `FusedBuilder` weld, AZ-I `StructRegistry` partial closure, AZ-II `ShapeTag` 9/9 declared on substrate, AZ-III W3b CSP installers). The plan-level fix is `AZ-IV.md` Hard Gate 9 (every active substrate has consumer evidence or deletion proof) backed by Babbage's substrate-matrix template.

### Theme 2 — Grammar-overfit lives in the runtime/builder seam

Fermat's census found 7 `from_rule_name(&str) -> Kind` impls (one per non-JSON grammar), 3 `(layout.kind, layout.rule_name.as_str())` builder dispatches (JSON + CSS L4 ×2), 1 `EmitStrategy::for_grammar` 9-arm allowlist, and 1 silent `JsonStructBuilder` fallback at `shapes/substrate.rs:70-78`. The substrate to displace these (StructRegistry, TypeDesc, FactAuthority, alt_dispatch typed-leaf push at `shapes/alt_dispatch/branches.rs:227-298`) ALREADY EXISTS — the hardening proved this. AZ-IV.W1 hard gates 10-15 + new sub-units W1.6/W1.7/W1.8 + new tests `crates/core/tests/no_grammar_name_branch.rs` and `crates/core/tests/synthetic_grammar_strategy.rs` enforce that closure. Substrate.rs fallback becomes a `panic!`. `EmitStrategy::for_grammar` becomes manifest-driven.

The critical insight is that the architecture's right bones (registry, type-desc, fact-authority) are ALREADY in place. AZ-IV is not building new abstractions — it is activating them and deleting the parallel hand-coded path.

### Theme 3 — Sibling-repo work has no gate

Diophantus found 4 active `bbnf_derive` edges (parse-that bootstrap source + 2 Cargo.lock entries + 4 stale xtask doc-link references), the csp-solver canonical-source split (22 shared files diverging between in-tree and csc411 sibling), and 5.5-day npm staleness in parse-that/typescript. The pre-existing AZ-III `cargo metadata --locked` workspace gate was insufficient because it stopped at `wasm/`. New W0 sub-units AZ-IV.W0.2.b csp-solver Canonicalisation Enforcement and AZ-IV.W0.2.c bbnf_derive Eradication close this with `diff -rq` discipline, `rg`-based zero-hit enforcement, and explicit lock regeneration for parse-that's Cargo + npm trees.

### Theme 4 — Dev-iteration debt compounds silently across waves

Boole's measurement found `bbnf-bootstrap` recurring >130s walls (because `scripts/bootstrap-bbnf.sh:28` deletes `target/.bbnf-cache/` before each cycle, defeating the content-keyed cache at `crates/derive/src/lib.rs:300-358`); fat-LTO bench harnesses cannot iterate without 10-min-per-harness compile walls (3 WATCHDOG_HALT rows in `post-AZ-III.json`); 1527-test workspace cycle is 290s with 118 known-failing tests polluting pass-rate signal. AZ-IV.W0 hard gates 7-8 (dev-iteration baseline + bbnf-bootstrap cache honesty) + scope item 8 close the cache-nuke; W0 verification artefacts add `W0-dev-baseline.txt` with row-by-row deltas vs the AZ-III baseline at `docs/benchmarks/AZ-III/W0p-bench-iter-walls.txt`; W3 hard gate 9 now requires named-function/named-regex hotspot evidence per watchdog row, not just `profile.json.gz`.

### Theme 5 — Legacy naming masks structural truth

Heisenberg's census tallied 935 walker/tape/DTA references across `crates/`, with 233 in generated `css_l4.rs` alone and a `dta_walker_table` variable still binding `lift_dta` output in production emitter `grammar.rs:145`. The "DTA" name (deterministic tape automaton) survived the runtime/walker deletion; it now describes a fact-extraction surface that has nothing to do with tapes or walkers. `dfa_codegen.rs` is the regex-scan adapter, not a DFA codegen module. `backend/rust/view/color.rs` (290 LOC, zero production consumers) is the "total hack" the user flagged. AZ-IV.W2.4 mechanism + sub-gate now name every rename (`dfa_codegen.rs` → `regex_scan_adapter.rs`, `dta.rs` → `grammar_facts.rs`, `recognize_*_legacy` → `recognize_*_pattern`), every deletion (`emit_dfa_inline_body`, `view/color.rs`, `view/peel.rs`, `runtime/view.rs:35` re-export, substrate.rs JSON fallback), and every comment scrub (named line numbers across `types/grammar.rs`, `keyword_dispatch.rs`, `shape_dispatch/mod.rs`, `shape_dict.rs`).

### Theme 6 — Chronic deferrals reveal architectural root causes, not "wrong successor letter"

Boole's genealogy found 13 of 15 carries deferred ≥3 tranches across B5/AZ-I/AZ-II/AZ-III/AZ-IV-planned. The top 5 by tranche count: PatternAnnotations migration (5+), `backend/rust/view/color` hack (5+), DTA/dfa naming (5), Sheets parity gap (4 — and regressed from 122/133 to 115/133 between AZ-III claim and AZ-IV-hardening rerun), strict regen drift (4). The architectural meta-causes the audit named: (a) substrate-with-consumer declared MET on substrate alone; (b) sibling-repo work has no gate; (c) stale-but-reachable code lacks deletion deadline. The plan-level fix is `AZ-IV.md` §Non-Routable Carries declaring those 13 items cannot route to a successor letter — if they cannot land inside AZ-IV without changing the AZ-IV thesis, the response is a triumvirate review of the thesis itself.

## Plan-Edit Landing Record

| Commit | File(s) | Headline |
|---|---|---|
| `db8b00ad` | 10 files (AZ-IV planning seed) | initial doc creation after prior 2 hardening passes |
| `fbc05508` | `audit/HARDENING-2026-05-01-cantor.md` | plan-coherence + spec-adherence audit |
| `c4b5f666` | `audit/HARDENING-2026-05-01-heisenberg.md` | legacy + naming surface census |
| `bf533042` | `audit/HARDENING-2026-05-01-boole.md` | dev speed + 5-tranche chronic-deferral genealogy |
| `67ea4e98` | `audit/HARDENING-2026-05-01-diophantus.md` | sibling-lib deep audit |
| `a60189d6` | `audit/HARDENING-2026-05-01-babbage.md` | substrate activation matrix |
| `e4068969` | `audit/HARDENING-2026-05-01-fermat.md` | grammar generality + overfitting audit |
| `f338a2e7` | `AZ-IV.md` + `GESTALT.md` + `PROGRESS.md` | parent-doc refinement: Non-Routable Carries, Cross-Tranche Debt rewrite, invariants 9-10, orchestration rules 10-12, hard gates 13-16, evidence-ledger expansion, parking-lot 27-row expansion |
| `fb5d1f0f` | `waves/W0.md` | W0 amendments: 5 new hard gates, 3 new scope items, 2 new sub-units (W0.2.b csp-solver, W0.2.c bbnf_derive), generated-size-budget table, HARD CAP block |
| `2d58a6d3` | `waves/W1.md` | W1 amendments: 5 new scope items, 6 new hard gates, 3 new sub-units (W1.6/W1.7/W1.8), W1.1 view/color sub-gate replacement, HARD CAP block, evidence artefacts |
| `c468b108` | `waves/W2.md` | W2 amendments: within-wave file-bound split (W2.2/W2.3/W2.5), W2.4 DTA cleanup mechanism + Files row + sub-gate replacement, hard gate 9 WIRED-NOT-CONSUMED enumeration, file bounds expansion, Disjointness cross-wave note, HARD CAP block |
| `3c725692` | `waves/W3.md` | W3 amendments: 2 new hard gates (non-routable closure + close-honesty checklist), tightened gate 9 (named-consumer hotspot), 3 new triumvirate triggers, HARD CAP block |
| (this commit) | `audit/HARDENING-SYNTHESIS-2026-05-01-FINAL.md` | this synthesis |

Total LOC delta (planning-only, no source touched): +84 (parent docs) + 48 (W0) + 53 (W1) + 37 (W2) + 10 (W3) = **+232 LOC of refinement** atop the 1034-LOC seed and 2361-LOC audit cohort.

## Hard-Gate Count Comparison

| Doc | Pre-pass-3 | Post-pass-3 | Net |
|---|---:|---:|---:|
| `AZ-IV.md` Hard Gates | 12 | 16 | +4 (overfit static scan, manifest strategy, substrate panic, non-routable blockers) |
| `AZ-IV.md` Orchestration Rules | 9 | 12 | +3 (HARD CAP defaults, triumvirate auto-triggers, ~700-word ceiling) |
| `AZ-IV.md` Invariants | 8 | 10 | +2 (no grammar overfit, no silent fallback) |
| `W0.md` Hard Gates | 6 | 11 | +5 (dev baseline, bbnf-bootstrap cache, generated-size budget, bbnf_derive zero-hit, csp-solver canonical) |
| `W0.md` Scope items | 7 | 10 | +3 (cache-nuke fix, derive eradication, csp-solver canonical) |
| `W0.md` Sub-units | 5 | 7 | +2 (W0.2.b, W0.2.c) |
| `W1.md` Hard Gates | 9 | 15 | +6 (no grammar-name branch, manifest strategy, substrate panic, from_rule_name eliminated, view/color deleted, recover_* deleted) |
| `W1.md` Scope items | 9 | 15 | +6 (10-15 mapping to substrate activations) |
| `W1.md` Sub-units | 5 | 8 | +3 (W1.6 typed-leaf push, W1.7 runtime dedup, W1.8 substrate panic + manifest strategy) |
| `W2.md` Hard Gates | 9 | 9 (gate 9 enumerated to a-f) | +0 row count, +6 sub-rows |
| `W2.md` File Bounds rows | 18 | 27 | +9 (obligation, shape_dict, node_facts, shape_dispatch, types/grammar, emitter/grammar, keyword_dispatch, shapes/substrate, codegen-paths) |
| `W3.md` Hard Gates | 13 | 15 | +2 (non-routable closure, close-honesty checklist) |
| `W3.md` Triumvirate triggers | 7 | 10 | +3 (three-loop iteration, non-routable thesis-reveal, JSONL >15min) |

## Substrate Matrix Summary (Babbage)

| Status | Count | Examples |
|---|---:|---|
| CONSUMED | 9 | StructRegistry, TypeDesc, FactAuthority, alt_dispatch typed-leaf push, runtime path/document/view, regex HIR (parse-that side), shape variants (production-active subset), 17 IR passes (active subset), span eligibility/FIRST/SCC |
| PARTIAL | 6 | CSP installers (consumed but not authoritative — alt_strategy.rs override), regex-engine selection (consulted not always emitted), egraph (Map preservation gap), simd-scan (gated narrowly), Pratt (grammar-specific paths remain), generated-output paths |
| WIRED-NOT-CONSUMED | 5 | egraph::ruler::{enumerate,oracle,residue}, RuleSet load, shape_dict_templates/selection, type_obligations Vec, alt_strategy CSP override (anti-pattern) |
| UNDERUTILIZED | 4 | Pratt for 5/9 grammars, ruler-alphabet single-Bool sources, view module (color shim), recognize_*_legacy producers |
| DEAD | 3 | emit_dfa_inline_body, codegen-paths.md merge_regex_alts entry, codegen-paths.md force_inline entry |
| **Total** | **26** | |

## Non-Routable Carries Reaffirmed

Per `AZ-IV.md` §Non-Routable Carries (Boole Amendment 1):

13 items have been deferred ≥3 tranches and are now non-routable in AZ-IV. AZ-IV cannot close by routing them to a successor letter. They land inside AZ-IV with cited evidence, or AZ-IV does not close. A non-routable item that cannot land without changing the AZ-IV thesis triggers a triumvirate scope-reveal review of the thesis itself, not a new tranche letter.

The 13: strict regen drift; egraph `Map` wrapper preservation; Sheets parity gap; tailwind regex_scan perf timeout; TS backend executable parity; watchdog rows under cross-profile; JSON value/path vs sonic-rs perf; CSS named_color runtime activation; PatternAnnotations migration; bootstrap/derive residue (sibling); DTA/dfa naming and cleanup; `backend/rust/view/color` hack; substrate denominator (CSP/regex/SIMD/Pratt/view); rewrite/ruler production wiring; WASM/sibling derive residue.

## Open Questions / Known Unknowns

1. **Generated-size budget pre-W0 LOC values are TBD**. The W0 verification artefact `W0-generated-size.txt` requires the orchestrator to record per-grammar LOC at base commit before W0 dispatch. The `+/- 5 %` ceiling is per-grammar, not aggregate.
2. **csp-solver canonicalisation choice (rebase vs `[patch.crates-io]`) is left to W0.2.b dispatch**. The diff-driven gate is robust to either decision; the wave amendment leaves the choice to the agent given the live state.
3. **Sub-unit cardinality (W1 = 8 sub-units in 5 writer slots) vs six-agent ceiling**. The plan honours the ceiling because parallel agent count is 5 in any wave; W1.6/W1.7/W1.8 sequence inside the W1.1/W1.5 owner worktrees. If dispatch reveals that a W1.X owner cannot complete its sub-units within the HARD CAP, the orchestrator splits via triumvirate, not by exceeding the parallel ceiling.
4. **`PatternAnnotations` migration target**. Heisenberg cited the sole consumer (`shape_dispatch/pratt.rs:101`); `dta.rs` rename closes the producer-side; the W2.5 sub-unit must name the migration shape (typed BBNF compound kind; FactAuthority projection; `node_facts`-direct reads) before deletion.
5. **bbnf-bootstrap retire vs cache-fix tradeoff**. Boole's W0 scope item 8 leaves the choice between (a) fixing `bootstrap-bbnf.sh` to stop nuking the cache and (b) removing bbnf-bootstrap from `iter-check-full` to the W0 dispatch. The cycle-2 ≤ 10 % gate enforces correctness regardless.

## Closing Posture

AZ-IV is now a **deeply-specified consolidation tranche** with 16 evidence-backed AZ-IV-level hard gates, 9 sub-unit hard-gate clusters, 27 paste-ready amendment blocks already integrated, 13 non-routable carries declared incompatible with successor routing, and 6 explicit cross-cutting themes the cohort surfaced. The plan stays 4 waves; the six-agent ceiling stays intact (max parallel = 5 writer slots in any wave); the close-honesty checklist is now wave-level and tranche-level enforceable.

The plan is now **ready for W0 dispatch**.

Open the W0 - Truth And Canonical Regen wave with `AZ-IV.md` and the refined `waves/W0.md` as the dispatch packet. The orchestrator records pre-W0 LOC per generated grammar, base commit, `git status --porcelain`, and `git worktree list` before opening sibling worktrees for the 5 parallel W0 writer slots. W0.2.b and W0.2.c sequence inside the W0.2 owner worktree.

If at any wave a non-routable carry cannot close with available evidence, the response is triumvirate review of the AZ-IV thesis. There is no AZ-V.

## Verification Commands

```text
git log --oneline 5211b953..HEAD
git diff db8b00ad..HEAD -- 'docs/tranches/AZ-IV/**' | wc -l
ls docs/tranches/AZ-IV/audit/
git worktree list
wc -l docs/tranches/AZ-IV/{AZ-IV,GESTALT,PROGRESS}.md docs/tranches/AZ-IV/waves/*.md docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-*.md
```

## Cohort Names

Hardening pass 3 cohort names (after mathematical foundations and substrate-of-thought figures): Cantor (set theory; plan-coherence audit), Heisenberg (uncertainty principle; legacy/naming surface census), Babbage (computational engines; substrate activation matrix), Fermat (last theorem; grammar generality and overfitting), Diophantus (Diophantine equations; sibling-lib deep audit), Boole (algebraic logic; dev speed and chronic-deferral genealogy).

The two prior cohorts: pass 1 (Pauli/Meitner/Wegener/Mencius/Locke/Socrates) and pass 2 (Aquinas/Lagrange/Ohm/Averroes/Banach/James). Three cohorts of 6 = 18 hardening agents across pass 1 (initial), pass 2 (loss-prevention), pass 3 (deep refinement). The next dispatch is implementation, not hardening.
