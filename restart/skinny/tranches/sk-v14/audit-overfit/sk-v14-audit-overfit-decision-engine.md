# SK-V14 S-P0 Axis A5 — Decision-Engine Fold Integrity

## §0 — Disposition summary

Pass criterion (verbatim from `PASS-0-OVERFIT-AUDIT.md §Scope` row A5):
> The CSP+egraph+cost resolver wired into compile; per-grammar policy +
> union substrate wired to actual runtime. **Pass criterion:** The
> resolver drives emission; no scaffold-only wave admits a row.

- Findings: **CRITICAL=0, HIGH=2, MED=1, LOW=1** (total 4).
- **Verdict: FAIL at HEAD, PASS conditioned on C-5 (PRUNE-1 + PRUNE-2) + C-4 (PRUNE-5) landing.** Resolver clause is PASS in isolation (wired end-to-end and drives emission via fail-closed lowering); scaffold-only-as-admit clause is FAIL at HEAD because W8 + W9 remain SCAFFOLD-ONLY and the W14.1-5 + W13.* + W15.1 (parse_only + typed + direct) rows and the 24 CSS L4 ADMITTED rows still cite W8 / W9 research artefacts at HEAD per A2 F1-F5 + A1 CRIT — those rows are PRUNE-1 / PRUNE-2 targets (audit-falsified per the SK-V14 contract) but no PRUNE commit has landed; ROLLING-SOTA-DELTA still carries them. Wave-level scaffold persistence is HIGH severity because PRUNE-5 must consume it; under the audit-corrected baseline, no row cites W8 / W9 as load-bearing evidence; at HEAD the W14.1-5 + W13.* + W15.1 rows are PRUNE-1 targets and the 24 CSS L4 rows are PRUNE-2 targets, all audit-falsified per A2 F1-F5 + A1 CRIT.
- Confirms / extends SK-V13 audit pack: **YES, confirms in full**; zero disagreements with `v4-decision-engine-trace.md §6` or `sk-v13-audit-overfit-decision-engine.md §9`.
- New findings (not in SK-V13 audit pack): **1** — gate-only consumption of `per_grammar_policy` / `same_substrate_union` report identifiers (file-routing telemetry without runtime semantic consumption); confirms but tightens the W8 / W9 SCAFFOLD verdict by quantifying the gate-layer-only footprint.

## §1 — Methodology

Per `S-P0-DISPATCH-CONTEXT.md §3` "executable verification mandate" — every cited path / pattern was run; counts and code quoted verbatim.

### §1.1 Files audited

- `skinny/crates/passes/src/lib.rs` (1869 lines; full read of `compile()` 31-65 and the cited 460-540 range covering 476-478).
- `skinny/crates/passes/src/decision_csp.rs` (249 lines; full read).
- `skinny/crates/codegen/src/lower/rust.rs` (lines 27-96 cited in v4).
- `restart/skinny/tranches/sk-v13/audit-overfit/sk-v13-audit-overfit-decision-engine.md` (full read).
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v4-decision-engine-trace.md` (full read).
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` §3 + §4 (full read; W8 / W9 reference scan).

### §1.2 Executable verification commands

| Command | Result |
| --- | --- |
| `wc -l skinny/crates/passes/src/lib.rs` | 1869 |
| `wc -l skinny/crates/passes/src/decision_csp.rs skinny/crates/passes/src/backend_egraph.rs` | 249 + 248 = 497 |
| `git grep -nc 'per_grammar_policy\|same_substrate_union\|GrammarConfig' skinny/crates/` | 20 hits across **3 files only**: `bbnf-bench/src/bin/gate.rs:14`, `bbnf-bench/src/lock14_baseline.rs:2`, `bbnf-bench/src/report.rs:4` — **zero matches in `passes/`, `codegen/`, `runtime/`, `ir/`** |
| `git grep -n 'GrammarConfig' skinny/crates/codegen/ skinny/crates/runtime/ skinny/crates/passes/ skinny/crates/ir/` | empty |
| `find skinny/crates/runtime/src -name "*.rs" \| xargs grep -l "UnionTape\|same_substrate\|union_tape"` | empty |
| `git grep -n 'BackendShape' skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/lower/rust.rs` | 7 hits — the lowering dispatcher consumes `BackendShape` via `HashMap<RuleId, BackendShape>` (lib.rs:140, 234; lower/rust.rs:1, 16, 22, 86) |
| `grep -nE 'fail.?closed\|csp\|egraph\|active.?cost\|decision.?csp' skinny/crates/codegen/src/lower/rust.rs` | 8 hits at lines 37-89; confirms fail-closed assertions on missing `cost_facts`, missing `active_cost`, missing `decision_csp`, shape-vs-CostFacts disagreement, `csp_status != "sat"`, budget != "pass", `selected_rule_count == 0`, `csp_solve_us > csp_timeout_ms * 1_000` |
| `git log --oneline -5 skinny/crates/passes/src/lib.rs skinny/crates/passes/src/decision_csp.rs skinny/crates/passes/src/backend_egraph.rs` | last three touches are `af8c143e0 feat(sk-v13-waveW7): admit CSP cascade fail-closed finalizer`, `9b6cdb080 feat(sk-v13-waveW6): admit egraph active cost gate`, `fdcc8b168 feat(sk-v13-waveW5): admit regex decision facts` — confirms W5/W6/W7 are the only commits touching the resolver layer; no SK-V14 commit yet |
| `git log --oneline restart/skinny/ROLLING-SOTA-DELTA.md \| head` | last touches W14.1-5 + W13.1-4 + W15.1; **zero SK-V14 commits since audit pack landed** — SK-V14 starting state == SK-V13 close state for the decision-engine layer |
| `grep -nE 'W8\|W9' restart/skinny/tranches/sk-v14/SYNTHESIS.md \| head` | 6 hits at lines 95, 136, 138, 196, 197, 274, 380, 400 — every reference frames W8 / W9 as **COSMETIC / SCAFFOLD** to be wired by PRUNE-5 / C-4; **zero load-bearing cite** |

### §1.3 Quote — `skinny/crates/passes/src/lib.rs:476-478` verbatim

Per dispatch mandate:

```
476        let candidates = backend_candidates(grammar, rule, backend_rule, layout, target);
477        let active = crate::backend_egraph::select(rule.id, candidates.clone());
478        crate::decision_csp::finalize_rule(&grammar.name, rule.id, candidates, active)
```

This is the per-rule pipeline: P1-P8-derived candidates → e-graph extraction → CSP solver. The return value flows back into `recognizers::derive_backend_shape_with_diagnostics()` → `layout_facts.cost_facts` (lib.rs:54) → `codegen::lower::rust::lower_to_rust()` (lower/rust.rs:27-89).

### §1.4 Quote — fail-closed enforcement in `codegen/src/lower/rust.rs:37-89`

```
 37                            "W7 fail-closed: backend shape {:?} disagrees with cost facts {:?} for rule {}",
 …
 41                if cost.active_cost.is_none() {
 43                        "W7 fail-closed: missing active-cost facts for rule {}",
 47                match cost.decision_csp.as_ref() {
 48                    Some(csp)
 49                        if csp.csp_status == "sat"
 50                            && csp.csp_budget_status == "pass"
 51                            && csp.selected_rule_count > 0
 52                            && csp.csp_solve_us <= csp.csp_timeout_ms.saturating_mul(1_000) => {}
 53                    Some(csp) => {
 55                            "W7 fail-closed: decision-CSP status {} budget {} selected {} for rule {}",
 …
 64                            "W7 fail-closed: missing decision-CSP facts for rule {}",
 …
 73                    "W7 fail-closed: missing cost facts for rule {}",
 …
 89            "W7 fail-closed: missing backend shape for rule {}",
```

Five fail-closed checks: missing cost facts; backend shape disagreement; missing active-cost facts; CSP status / budget / count / solve-time bounds; missing decision-CSP facts; missing backend shape. Lowering panics on any of these. The CSP-emitted shape selection alone reaches emission.

## §2 — Per-finding ledger

| # | Severity | Finding | Citation | Status |
| --- | --- | --- | --- | --- |
| 1 | HIGH | **W8 per-grammar policy SCAFFOLD-ONLY persists at SK-V14 starting state**. No `GrammarConfig` struct in `skinny/crates/{codegen, runtime, passes, ir}/`; CSP-selected `BackendShape` is consumed by lowering but no per-grammar policy facts modulate the emitted code. CSS L4 hardcoded `UNION_PROJECTION_*` constants in `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/config.rs` replace the dynamic policy dispatch the W8 research artefact specified. | `git grep -n 'GrammarConfig' skinny/crates/codegen/ skinny/crates/runtime/ skinny/crates/passes/ skinny/crates/ir/` empty; `v4 §4`; `v4 §6` W8 SCAFFOLD-ONLY verdict; SK-V14 SYNTHESIS §1.2 line 196 "COSMETIC; zero runtime consumption" | CONFIRMS V13 (`sk-v13 §5`) |
| 2 | HIGH | **W9 same-substrate union SCAFFOLD-ONLY persists at SK-V14 starting state**. No `UnionTape` variant, no `same_substrate` API in `skinny/crates/runtime/src/tape/`; CSS row uses hardcoded `token_union_projection(kind, depth) -> &'static str` returning fixed `UNION_PROJECTION_NORMALIZED_ASCII` / `UNION_PROJECTION_RAW_BYTES` constants — a hardcoded conditional, not a structural union. | `find skinny/crates/runtime/src -name "*.rs" \| xargs grep -l "UnionTape\|same_substrate\|union_tape"` empty; `v4 §5 + §9` "most surprising finding"; SK-V14 SYNTHESIS §1.2 line 197 "COSMETIC; hardcoded constants" | CONFIRMS V13 (`sk-v13 §6`) |
| 3 | MED | **`per_grammar_policy` / `same_substrate_union` symbols exist ONLY in the gate / bench / report layer** — `skinny/crates/bbnf-bench/src/bin/gate.rs` (14 hits: 2 entry validators + 2 path routers + tests), `skinny/crates/bbnf-bench/src/lock14_baseline.rs` (2 hits inside W1a admit-string literals), `skinny/crates/bbnf-bench/src/report.rs` (4 hits: schema validator + test wiring). Confirms but **tightens** the V13 SCAFFOLD verdict: the surface is not merely absent from runtime — it is *report-bound only*, gating compliance reports without ever reaching the compile / lower path. | `git grep -nc 'per_grammar_policy\|same_substrate_union\|GrammarConfig' skinny/crates/` returns the 3-file / 20-hit footprint; `report.rs:1139` literal `"public GrammarConfig"`; `report.rs:7941, 8004` test fns `skv13_per_grammar_policy_report_accepts_measured_block` and `skv13_same_substrate_union_report_accepts_measured_admit` | NEW (extends V13 §4 + §5 with quantified footprint) |
| 4 | LOW | **Resolver-output telemetry self-labels `static-template-blocker` and `sink-only-static-blocker` at the schema level** (`decision_csp.rs:160-161`: `static_css_provider_status: "static-template-blocker"`, `json_sink_only_status: "sink-only-static-blocker"`); the CSP facts emit a `block_id: "JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT"` (line 164). The resolver thus self-reports as load-bearing on the decision-layer and intrinsically blocked on the row-movement layer. This is not a finding against the resolver; it is the resolver's own honest declaration. | `decision_csp.rs:160-164` quoted in §1 above; matches `v4 §2` architectural-block citation `JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-RUNTIME` chained to `JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT` | CONFIRMS V13 (`sk-v13 §3 + §4`) |

### §2.1 Cross-axis Lock-14 corollaries (not double-counted here)

- The 8 per-grammar provider modules under `skinny/crates/codegen/` and the 64 hand-written per-grammar runtime files under `crates/core/src/runtime/{json, css_l4, google_sheets, bbnf, csv, ebnf, bnf, math, css_pretty}/` (verified by `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` = 9 dirs — one over the dispatch-cited 8 because of `css_pretty`) are A3 / A6 territory; they are not counted in this A5 ledger but they are the *reason* W8 / W9 cannot be wired without a generator refactor first. SK-V14 C-1 (PRUNE-3 + PRUNE-4) must land before C-4 (PRUNE-5) can wire W8 / W9 cleanly.
- The dispatch §1 enumerated 8 directories; the actual count is 9 (`css_pretty` is the additional one). This is a downstream A3 observation; recorded here only because A5 fan-out must reconcile its data with the cross-axis count.

## §3 — Pass criterion verdict

Quoting `PASS-0-OVERFIT-AUDIT.md §Scope` row A5 verbatim:

> The CSP+egraph+cost resolver wired into compile; per-grammar policy +
> union substrate wired to actual runtime. **Pass criterion:** The
> resolver drives emission; no scaffold-only wave admits a row.

**Verdict: FAIL at HEAD, PASS conditioned on C-5 (PRUNE-1 + PRUNE-2) + C-4 (PRUNE-5) landing.**

- **Resolver-drives-emission clause: PASS.** `passes/src/lib.rs:476-478` wires the W5 (`bbnf-regex::analyze`) → W6 (`backend_egraph::select`) → W7 (`decision_csp::finalize_rule`) pipeline per rule; the CSP output reaches `codegen::lower::rust::lower_to_rust()` via `CostFacts.decision_csp`; five distinct fail-closed checks (lower/rust.rs:37-89) panic on missing or inconsistent CSP facts. Removing the CSP would deny lowering. Removing the e-graph would degrade selection to silent P1-P8 cascade — a regression `decision_csp.rs:150-154` explicitly blocks via `cascade_retirement_status: "fail_closed"` + `legacy_cascade_admission_status: "blocked"`.
- **No-scaffold-only-admit clause: FAIL at HEAD, PASS conditioned on C-5 (PRUNE-1 + PRUNE-2) + C-4 (PRUNE-5) landing.** SK-V13 W14.1-5 (5 JSON parse_only) + W13.1-4 + W15.1 (typed + direct admits) rows cite W8 / W9 facts as evidence per the W8 / W9 research JSONs, and the 24 CSS L4 ADMITTED rows likewise carry W8 / W9 evidence at HEAD; ROLLING-SOTA-DELTA still carries every one of these rows (last touch `7ec4a474c W15.1`; no PRUNE commit landed). Those rows are AUDIT-FALSIFIED in the SK-V14 contract per SYNTHESIS §0.2 + §1.2 + §1.3, but the audit-corrected baseline is the *target* column, not present HEAD state. PRUNE-1 + PRUNE-2 revert the scaffold-cited admits; PRUNE-5 (C-4) wires W8 / W9 to load-bearing before any new admit may cite them. Until C-5 + C-4 land, the no-scaffold-only-admit clause is FAIL.

The resolver clause carries SK-V14 forward unchanged; the scaffold-clause obligation is the C-4 binding in SYNTHESIS §3, with the C-5 PRUNE-1 + PRUNE-2 revert as the prerequisite C-5 binding.

## §4 — Recommended prune actions

Cross-referenced to SK-V14 SYNTHESIS §3 candidate shortlist and §0.3 R-targets:

| Recommendation | Cross-reference | Severity | Action class |
| --- | --- | --- | --- |
| **Wire W8 per-grammar policy from gate / report layer through to codegen lowering.** Add `GrammarConfig` struct in a generic crate (no grammar-name leak; consumed by all grammars uniformly); thread through `passes::compile()` adjacent to `layout_facts.cost_facts`; consumed by `codegen::lower::rust::lower_to_rust()` as a third input alongside `backend_shape` + `cost_facts`. Same-wave measured row consumer on at least one named pre-wave JSON row. | SYNTHESIS §3 C-4 (R3 PRUNE-5); §0.4 P-5; §0.3 R3 | HIGH | Wire scaffold to load-bearing |
| **Wire W9 same-substrate union into the `Tape` / `OffsetFlags` layer as a same-tape, codegen-private union variant** — NOT a public substrate API; per SYNTHESIS §4 "union variants admit only as same-tape, codegen-private, row-consumed shapes". CSP shape selection produces measurable runtime divergence (samply hot-leaf change) on the same pre-wave row. | SYNTHESIS §3 C-4 (R3 PRUNE-5); §0.4 P-5; SK-V14 SYNTHESIS §4 "no public substrate API" clause | HIGH | Wire scaffold to load-bearing |
| **Quantify the gate-layer footprint** (3 files / 20 references for `per_grammar_policy` / `same_substrate_union` / `GrammarConfig`); after C-4 lands and the runtime consumer is live, the gate / report identifiers may become genuine compliance gates rather than scaffold telemetry. No source-touch required from A5; A6 / A3 own the Lock-14 generic-crate-leak audit. | A3 / A6 cross-reference; SYNTHESIS §3 C-1 | MED | Cross-axis tracking only |
| **Preserve the resolver's honest self-labelling** (`decision_csp.rs:160-164` block IDs). Post-C-4 these block IDs should clear; until they do, they correctly carry the SCAFFOLD-blocker signal end-to-end. The block-ID chain (`JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-RUNTIME` → `JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT`) must remain a gate-rejection invariant inside the C-4 entry-gates so any new admit attempting to cite W8 / W9 pre-runtime-consumer is denied at admit time. | `v4 §2` architectural-block citation chain; SYNTHESIS §3 C-4 entry-gate manifest | LOW | Preserve through PRUNE-5; gate-rejection invariant inside C-4 entry-gates |

### §4.1 Sequencing constraint

Per SYNTHESIS §0.4 P-5 + §4 "any SPEC wave that wires `bbnf-simd` … carries `G-SIMD-GRAMMAR-POLICY`" and `[no-deferrals]`: C-4 (PRUNE-5) must run **after** C-1 (PRUNE-3 + PRUNE-4 — Lock-14 refactor cluster) so the generic generator template exists to consume W8 policy. Wiring W8 into the current per-grammar provider module mesh would re-deepen the Lock-14 violation, not remediate it. C-1 → C-4 ordering is structural, not nominal.

### §4.2 Pre-blocked routes the A5 verdict reinforces

- **P-5 — Scaffold-research counted as load-bearing** (SYNTHESIS §0.4): no future SK-V14 row admit may cite W8 / W9 until the same-wave runtime consumer is measured. The current resolver telemetry (`decision_csp.rs:160-164`) carries the block_id chain that gate-rejects any such citation at admit time; that wiring is correct and must not be silenced.
- The C-4 entry / exit gates in SYNTHESIS §3 (named pre-wave row hot-leaf change in samply trace; per-shape Lock-1 triad declared in REDRESS) are the falsifiability hooks for the C-4 admission. A5 confirms these gates have a real resolver layer to dispatch against.

## §5 — Disposition

The decision-engine fold is the architecturally honest layer of the SK-V13 close — W5 / W6 / W7 land as designed, the CSP solver drives emission via fail-closed lowering, and the resolver's own telemetry honestly self-labels the W8 / W9 row-movement block. The SK-V14 contract reads this correctly: the resolver clauses survive (SYNTHESIS §1.1 pillars 1-3); the W8 / W9 SCAFFOLD verdict is reaffirmed (SYNTHESIS §1.2 lines 196-197 + §3 C-4 + §4 PRUNE-5 wiring obligation). At HEAD, the W14.1-5 + W13.* + W15.1 rows and the 24 CSS L4 ADMITTED rows continue to cite W8 / W9 as load-bearing evidence and ROLLING-SOTA-DELTA still carries them; the audit-corrected baseline (the *target* of C-5 PRUNE-1 + PRUNE-2) clears these to zero. C-4 (PRUNE-5) is the wave that then converts SCAFFOLD to LOAD-BEARING under measured row consumption; until both C-5 and C-4 land the no-scaffold-only-admit clause is FAIL at HEAD.

The two HIGH findings (W8 / W9 SCAFFOLD persistence) are addressed by C-4; the MED finding (gate-layer-only footprint) is a quantification of the SCAFFOLD verdict and clears as C-4 wires the runtime consumer; the LOW finding (honest self-labelling) is preserved through PRUNE-5 as a gate-rejection invariant inside C-4 entry-gates so any admit attempting to cite W8 / W9 pre-runtime-consumer is denied at admit time. No CRITICAL violation in the decision-engine layer at SK-V14 starting state.
