---
agent: 1F
pass: T-P1-excavation
cycle: V1
generated_at: 2026-05-27T23:05:32-04:00
spec_surfaces_audited:
  - restart/ARCHITECTURE.md
  - restart/MASTER-PLAN.md
  - restart/locks/LOCKS.md
  - restart/HANDOFF.md
  - restart/MIGRATION.md
  - restart/skinny/INDEX.md
  - restart/skinny/SUBSTRATE.md
  - restart/skinny/COMPILER.md
  - restart/skinny/BENCH.md
  - restart/skinny/WORKSPACE.md
  - restart/skinny/HARDENING.md
  - restart/skinny/ROLLING-SOTA-DELTA.md
  - restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md
  - restart/skinny/tranches/sk-v15/SYNTHESIS.md
  - restart/skinny/tranches/sk-v15/HANDOFF.md
  - restart/skinny/tranches/sk-v15/audit-overfit/*.md
  - restart/skinny/tranches/sk-v15/research/**/*.md
  - restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md
files_audited_count: 3031
live_truth_method: "sed/nl path:line reads over V1 spec surfaces; rg scans over restart/skinny/tranches/sk-v15 and prior totality reports; find counts: skinny/crates *.rs/*.toml=665, root crates/core/src/runtime *.rs=75, SK-V15 markdown=46, prior tranche research markdown=2227; wc -l largest-file census; no cargo/build mutation"
prior_cycle_dispositions_folded:
  accepted:
    - SKV14-PATTERN-H-67
    - SKV14-LOCK14-ROOT-LEAKS
    - SKV14-CSS-AUDIT-ZERO-AND-PRUNE-CHAIN
  rejected: []
  revised:
    - SKV14-W2-W4-W5-W6-W7-WAVE-GRAPH-CYCLES -> SKV15-NEW-CH3-V5-01
    - SKV14-CSS-ADMIT-CLOSURE -> SKV15-CSS-AUDIT-DEMOTED
    - SKV14-GATE-CLEANNESS -> SKV15-GATE-EXCLUSION-REQUIRED
  first_cycle_additions:
    - COH-001-SKV15-authority-drift
    - COH-002-CSS-broadcast-admission-drift
    - COH-003-wave-graph-cycle-detection-gap
    - COH-004-gate-exclusion-detection-gap
    - COH-005-Lock14-root-token-scan-gap
    - COH-006-Pattern-H-provenance-gap
    - COH-007-Decision-Engine-scaffold-gap
    - COH-008-BackendShape-depth-gap
    - COH-009-CSS_GENERATED_RS-contrivance
    - COH-010-past-corpora-preblock-ledger
    - COH-011-JSON-guard-baseline
    - COH-012-G-Alpha-G-Omega-conflict
divergence_count:
  spec_claims_implemented: 2
  spec_claims_unimplemented: 9
  impl_exceeds_spec: 0
  unknown: 1
locks_amendment_candidates: 0
---

## Executive Summary

SK-V15 starts from a split truth: JSON is a validated guard baseline, while CSS L4 is audit-demoted and Pattern H, Lock 14/16 gates, codegen neutrality, and the Decision Engine remain prune/rebuild obligations. The current top-level V1 surfaces still mostly narrate SK-V14 W5B-FRONTENDR and the SK-V14 audit-corrected baseline (`restart/HANDOFF.md:5`, `restart/HANDOFF.md:70`, `restart/skinny/INDEX.md:5`), while SK-V15 now declares a PRUNE-then-REBUILD tranche (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:5`, `restart/skinny/tranches/sk-v15/HANDOFF.md:8`). The most material drift is not a new architecture disagreement; it is stale authority routing and evidence-gate incompleteness. Live scans confirm the known risks: 24 CSS admits are one broadcast measurement; `CSS_GENERATED_RS` is a relocated hand-curated parser body; Lock 14/16 gates omit or self-exempt known leak roots; Pattern H remains 67 root runtime files with 0 generated headers; the Decision Engine has zero e-graph rewrites, non-driving CSP, and four stub lowerers. Past-corpora findings that SK-V15 must not re-derive are already named in Alpha-C, especially REDRESS-183/184/209..213 and the seven pre-blocked route patterns.

## Spec-Claim ↔ Implementation Table

| ID | Spec / corpus claim (path:line) | Live / counter-surface evidence (path:line) | Verdict | Divergence count class | Note |
|---|---|---|---|---|---|
| COH-001 | Main handoff says current authority is SK-V14 W5B-FRONTENDR / audit-zero baseline (`restart/HANDOFF.md:5`, `restart/HANDOFF.md:70`). INDEX likewise names SK-V14 as live dispatch authority (`restart/skinny/INDEX.md:5`, `restart/skinny/INDEX.md:13`). | SK-V15 handoff says PASS-IMPL V1 reclassified JSON honest / CSS contrived / Pattern H not collapsed / Decision Engine scaffold (`restart/skinny/tranches/sk-v15/HANDOFF.md:8`-`11`) and SK-V15 is open (`restart/skinny/tranches/sk-v15/HANDOFF.md:13`). | spec-surface drift | unimplemented | Update authority surfaces before downstream agents route from stale SK-V14 text. |
| COH-002 | SK-V14 INDEX still routes W8/W9/W10 as blocked behind PRUNE-1..5 (`restart/skinny/INDEX.md:35`-`40`). | SK-V15 close requires no 24-row CSS broadcast, typed CSS value output, and no CSS contrivance (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:39`-`42`); CSS is explicitly audit-demoted (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:62`-`68`). | spec-surface drift | unimplemented | The older block is directionally right but lacks the SK-V15 anti-broadcast and typed-value conditions. |
| COH-003 | MASTER and MIGRATION encode the Omega V8 W5B sequence and downstream blocks (`restart/MASTER-PLAN.md:800`-`823`, `restart/MIGRATION.md:138`-`144`). | SK-V15 adds NEW-CH3-V5-01 requiring delete/rebuild dependency tables for every retirement (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:100`-`106`) and Alpha-F repeats the dependency-table requirement (`restart/skinny/tranches/sk-v15/research/alpha/alpha-F-contract-draft.md:64`-`69`). | spec-surface drift | unimplemented | This is the V3/V4 wave-graph-cycle miss: deletion is blocked unless rebuild provider proof lands no later than delete wave. |
| COH-004 | Lock 14 mandates per-wave gate enforcement over generic roots and decision-engine facts (`restart/locks/LOCKS.md:377`-`390`); Lock 16 requires strict checkasm and source-present primitive manifest closure (`restart/locks/LOCKS.md:480`-`506`). | SK-V15 close adds self-exempting gate rejection and exclusion reporting (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:44`, `restart/skinny/tranches/sk-v15/SYNTHESIS.md:109`-`110`); A3 finds omitted scan roots and self-exempting legacy gates (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A3-lock14-lock16-generic-scan.md:26`-`31`). | spec-to-live drift | unimplemented | Gate-exclusion misses are live, not hypothetical. |
| COH-005 | Lock 14 forbids grammar switches, grammar-named public APIs, feature flags, and hand-written per-grammar runtime files (`restart/locks/LOCKS.md:349`). | A3 shows the live Lock 14 roots omit leak-bearing codegen roots (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370`-`2379`) and the forbidden token universe is JSON-shaped only (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:2381`-`2395`). | spec-to-live drift | unimplemented | Lock text is strong; executable scan is too narrow. |
| COH-006 | Lock 14 v+1 requires a Pattern H per-tranche count and treats the 67-file recurrence as category-scale failure (`restart/locks/LOCKS.md:402`-`424`); MASTER repeats the 67 baseline and 9-sub-wave PRUNE-4 (`restart/MASTER-PLAN.md:311`-`322`). | A4 reports Pattern H is 67 files with 0 generated headers (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A4-generator-roundtrip.md:17`-`18`, `:29`); A6 repeats 9 dirs / 67 files / 0 headers (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A6-pattern-recurrence.md:23`-`27`, `:35`). | spec-to-live drift | unimplemented | Pattern H is counted but not collapsed; provenance is the missing close primitive. |
| COH-007 | ARCH describes active cost facts, DecisionCspFacts, and `lower_to_rust` admission only with satisfying CSP (`restart/ARCHITECTURE.md:1081`); SK-V15 close requires rewrite count >=1 and real lowerers (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:47`). | A5 finds zero e-graph rewrites (`skinny/crates/passes/src/backend_egraph.rs:65`-`67`), selected-index CSP preservation (`skinny/crates/passes/src/decision_csp.rs:35`-`54`), grammar-named decision facts (`skinny/crates/passes/src/decision_csp.rs:162`-`166`), and four stub lowerers (`skinny/crates/codegen/src/lower/eager_tape.rs:15`-`17`). | spec-to-live drift | unimplemented | Decision Engine is scaffold, not load-bearing. |
| COH-008 | ARCH and HANDOFF preserve the five `BackendShape` variants and per-rule derivation (`restart/ARCHITECTURE.md:1088`-`1115`, `restart/HANDOFF.md:352`-`371`). | The enum/dispatcher covers all five (`skinny/crates/codegen/src/lower/mod.rs:17`-`24`), but four lowerers return label strings (`skinny/crates/codegen/src/lower/offset_tape.rs:15`-`17`, `skinny/crates/codegen/src/lower/event_tape.rs:15`-`17`, `skinny/crates/codegen/src/lower/collapsed_stage.rs:15`-`17`). | partially implemented | unimplemented | Five-shape canon is intact; implementation depth is not. |
| COH-009 | Generated-output allowance survives only when emitted from grammar source + workspace metadata (`restart/locks/LOCKS.md:351`-`366`). | CSS generation emits `normalize(CSS_GENERATED_RS)` (`skinny/crates/codegen/src/runtime_generator.rs:81`-`104`) where `CSS_GENERATED_RS` is the embedded full parser body (`skinny/crates/codegen/src/runtime_generator.rs:713`-`830`); A4 says all seven CSS generated files share one hash and are generated from a relocated hand-curated string (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A4-generator-roundtrip.md:16`, `:24`). | spec-to-live drift | unimplemented | CSS contrivance remains live. |
| COH-010 | Alpha-C says SK-V15 waves must not reopen broadcast CSS admits, mismatched CSSOM comparisons, moved string-literal parsers, silent exclusions, Pattern H without generated ownership, scaffold Decision Engine, or FNV closed-enum production migration (`restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:50`-`62`). | The pre-blocks are also routed to PRUNE/REBUILD receivers (`restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:64`-`69`). | implemented ledger | implemented | Past-corpora guard exists; main surfaces need cross-links. |
| COH-011 | SK-V15 validates JSON as guard baseline: 51/51 remains admitted (`restart/skinny/tranches/sk-v15/research/alpha/alpha-D-validated-invalidated.md:10`-`12`). | A2 finds JSON parse/direct/typed guard rows clean except FNV bench-only quarantine (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A2-admit-mechanism-integrity.md:11`-`16`, `:47`-`48`). | implemented | implemented | Do not re-derive JSON guard as if CSS invalidation invalidated JSON. |
| COH-012 | PASS-ALPHA / ORCHESTRATOR still describe G-Alpha as mandatory per Alpha-F (`restart/skinny/tranches/sk-v15/research/alpha/alpha-F-contract-draft.md:71`-`80`). | SK-V15 handoff says only G-Omega is mandatory under the active user pin (`restart/skinny/tranches/sk-v15/HANDOFF.md:46`-`50`). | open conflict | unknown | Governance conflict is documented but not reconciled in V1 surfaces. |

## Divergences Catalogued

| ID | Divergence | Evidence | LOC / risk |
|---|---|---|---|
| COH-001 | Main authority pages are one tranche stale: they still route from SK-V14 after SK-V15 opened. | `restart/HANDOFF.md:5`-`13`; `restart/skinny/INDEX.md:5`-`23`; `restart/skinny/tranches/sk-v15/HANDOFF.md:8`-`18` | 80-160 LOC docs; high routing risk |
| COH-002 | CSS 24-row broadcast remains invalidated while older surfaces still carry SK-V14 dispatch posture. | `restart/skinny/tranches/sk-v15/SYNTHESIS.md:39`-`42`; A1 broadcast row at `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A1-measurement-integrity.md:11` | 100-250 LOC ledger/gate; critical |
| COH-003 | Wave-graph cycle detection is SK-V15-local, not folded into MASTER/MIGRATION. | `restart/MASTER-PLAN.md:800`-`823`; `restart/MIGRATION.md:138`-`144`; `restart/skinny/tranches/sk-v15/SYNTHESIS.md:100`-`106` | 60-140 LOC docs/gates; high |
| COH-004 | Gate-exclusion and self-exempting-gate misses are live. | `restart/skinny/tranches/sk-v15/SYNTHESIS.md:109`-`110`; `skinny/crates/bbnf-bench/src/bin/gate.rs:63`-`75`; `skinny/xtask/src/main.rs:285`-`292` | 80-220 LOC gate hardening; high |
| COH-005 | Lock 14 root/token scan misses codegen and non-JSON grammar leaks. | `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370`-`2395`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A3-lock14-lock16-generic-scan.md:44`-`52` | 120-300 LOC scan expansion; high |
| COH-006 | Pattern H count is stable at 67, but generated ownership is 0/67. | `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A4-generator-roundtrip.md:17`-`18`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A6-pattern-recurrence.md:35` | 1.5k-3k LOC PRUNE-D; critical |
| COH-007 | Decision Engine is ceremonial: zero rewrite rules, non-driving CSP, stub lowerers. | `skinny/crates/passes/src/backend_egraph.rs:65`-`67`; `skinny/crates/passes/src/decision_csp.rs:53`-`83`; `skinny/crates/codegen/src/lower/eager_tape.rs:15`-`17` | 600-1.4k LOC split REBUILD-F; critical |
| COH-008 | BackendShape exists but only `SinkOnly` is materially lowered. | `skinny/crates/codegen/src/lower/mod.rs:17`-`24`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A5-decision-engine-fold.md:14` | 400-1.2k LOC lowerer work; high |
| COH-009 | CSS "generated" path is a grammar-header facade over a hand-curated string body. | `skinny/crates/codegen/src/runtime_generator.rs:97`; `skinny/crates/codegen/src/runtime_generator.rs:713`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A4-generator-roundtrip.md:49`-`53` | 300-900 LOC PRUNE/REBUILD; critical |
| COH-010 | Prior route pre-blocks are captured and must be treated as ledger constraints. | `restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:26`-`33`, `:50`-`62` | 0 LOC if respected; high if reopened |
| COH-011 | JSON guard baseline remains valid and should not be re-derived. | `restart/skinny/tranches/sk-v15/research/alpha/alpha-D-validated-invalidated.md:8`-`21`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A2-admit-mechanism-integrity.md:47`-`48` | 0 LOC guard; low |
| COH-012 | G-Alpha vs G-Omega gate posture is documented but not reconciled in V1 governance surfaces. | `restart/skinny/tranches/sk-v15/research/alpha/alpha-F-contract-draft.md:71`-`80`; `restart/skinny/tranches/sk-v15/HANDOFF.md:46`-`50` | 20-60 LOC governance note; medium |

## Anti-Pattern Scan

| Anti-pattern | Live evidence | Verdict | verify_action |
|---|---|---|---|
| God modules | Live `wc -l` over skinny Rust files reports `bbnf-bench/src/report.rs=10564`, `bbnf-bench/src/bin/gate.rs=5949`, `bbnf-bench/src/generated_real_typed.rs=4941`, `bbnf-bench/src/lock14_baseline.rs=4796`, `bbnf-bench/src/nonjson_css_l4.rs=3644`; Lock 13 permits bench/report/gate over 500 LOC only under explicit gate-surface budget (`restart/locks/LOCKS.md:340`-`344`). | open; bench may be exempt, production-like reports need budget transcript | Emit a Lock 13 transcript naming each >500 LOC file and its exemption or split receiver. |
| Parallel substrates / sidecars | Lock 1/ARCH treat mask streams as transient producers, not retained sidecars (`restart/ARCHITECTURE.md:1088`); Track 2 uses runtime tape helpers (`skinny/crates/runtime/src/lib.rs:1`, `skinny/crates/runtime/src/grammars/json/parser.rs:5` per prior 1F evidence), while CSS facts are comparator/fact-stream outputs. | no new retained substrate proven in this pass; CSS fact-stream still needs fence | Re-run `rg -n 'EventCursor|generated_eventcursor|structural_offsets|TapeAssembler' skinny/crates crates/core/src` before any close claim. |
| Grammar-name leaks | Runtime root exports JSON and seven CSS modules by name (`skinny/crates/runtime/src/lib.rs:3`-`25`); codegen profiles hardcode eight profiles (`skinny/crates/codegen/src/grammar_profile.rs:89`-`99`); A3 sees CSS/JSON decision facts (`skinny/crates/passes/src/decision_csp.rs:162`-`166`). | live Lock 14 violation | PRUNE-WAVE-B expands scan; PRUNE-WAVE-C removes codegen/runtime family branches. |
| Self-exempting gates | Legacy gate paths return `Ok(())` without explicit JSON/results check (`skinny/crates/bbnf-bench/src/bin/gate.rs:63`-`75`, `:91`-`92`); `xtask gate_json` only validates results when `--check-results` is present (`skinny/xtask/src/main.rs:285`-`292`). | live close risk | Gate close must reject missing `--check-results` or emit a non-close diagnostic; no silent pass. |

## Past-Corpora Do-Not-Redrive Ledger

| Finding family | Prior-corpora evidence | SK-V15 implication |
|---|---|---|
| Wave-graph cycles | REDRESS-183, 184, 209..212, and 213 remain pre-blocked (`restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:26`-`33`). | No delete/retire wave without rebuild-provider proof table; this is NEW-CH3-V5-01. |
| CSS broadcast admission | Alpha-C pre-blocks one CSS timing tuple projected into N conceptual admits (`restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:54`); A1 records the 24-row duplicate tuple (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A1-measurement-integrity.md:11`). | Do not re-admit CSS by row multiplication; require one aggregate diagnostic or N distinct measurements. |
| CSS comparator mismatch | Alpha-D invalidates CSS because a brace-counter summary was compared against lightningcss CSSOM (`restart/skinny/tranches/sk-v15/research/alpha/alpha-D-validated-invalidated.md:25`-`34`). | cssparser is near-term same-workload comparator; lightningcss waits for CSSOM/value parity. |
| Lock 14/16 gate holes | Alpha-C pre-blocks silent exclusion lists (`restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:57`); A3 details omitted roots and non-strict report coverage (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A3-lock14-lock16-generic-scan.md:36`-`84`). | No gate can close without exclusion report and strict primitive manifest. |
| Pattern H | Alpha-C pre-blocks Pattern H collapse without generated headers and ownership (`restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:58`); A6 finds 67 files / 0 headers (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A6-pattern-recurrence.md:23`-`27`). | Reheader-only or per-grammar workaround is not closure. |
| Decision Engine scaffold | Alpha-C pre-blocks zero-rule e-graph, non-driving CSP, and label-string lowerers (`restart/skinny/tranches/sk-v15/research/alpha/alpha-C-redress-digest.md:59`-`60`). | REBUILD-WAVE-F needs emitted runtime diffs, not fact strings. |

## Gaps / Missing Primitives

| Gap | Evidence | Receiver |
|---|---|---|
| No current top-level SK-V15 authority anchor in HANDOFF/INDEX/MASTER. | SK-V15 authority exists at `restart/skinny/tranches/sk-v15/SYNTHESIS.md:10`-`25`; top-level HANDOFF/INDEX still start from SK-V14 (`restart/HANDOFF.md:5`, `restart/skinny/INDEX.md:5`). | T-P3/Omega docs fold |
| No gate-exclusion-report field enforced by the current Lock 14/16 close gates. | SK-V15 telemetry requires `gate_exclusion_report` (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:121`-`127`); A3 shows current gates omit/exempt roots (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A3-lock14-lock16-generic-scan.md:66`-`76`). | PRUNE-WAVE-B |
| No typed CSS value/document/view API. | SK-V15 close requires CSS Value API (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:41`); A4 finds CSS output is summary/fact-stream text (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A4-generator-roundtrip.md:24`-`25`). | REBUILD-WAVE-E |
| No generated root runtime provenance for Pattern H. | A4/A6 report 67 root runtime files and 0 generated headers (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A4-generator-roundtrip.md:17`-`18`; `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A6-pattern-recurrence.md:35`). | PRUNE-WAVE-D |
| No load-bearing e-graph/CSP/lowerer decision loop. | A5 verdict table rows CRITICAL/HIGH (`restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A5-decision-engine-fold.md:10`-`16`). | REBUILD-WAVE-F |

## Open Questions

| UNKNOWN | Blocking question | verify_action |
|---|---|---|
| U-COH-001 | Which top-level surface becomes the canonical SK-V15 entry: `restart/HANDOFF.md`, `restart/skinny/INDEX.md`, or SK-V15 tranche handoff only? | T-P3/Omega fold should add one SK-V15 authority row to HANDOFF and INDEX, then cite SK-V15 SYNTHESIS/HANDOFF as current tranche authority. |
| U-COH-002 | Should Lock 14/16 text itself gain explicit "report all exclusions" language, or is SK-V15 telemetry sufficient? | 1E/T-P3 should compare `restart/locks/LOCKS.md:377`-`390` and `:480`-`:506` against `restart/skinny/tranches/sk-v15/SYNTHESIS.md:109`-`127`, then decide whether to amend LOCKS or only gate schema. |
| U-COH-003 | Are all claimed sidecar absences still true in the current dirty tree? | Capture exact `rg -n 'EventCursor|generated_eventcursor|structural_offsets|TapeAssembler|fixture_sidecar_facts|same-plane-source-sidecar' skinny/crates crates/core/src` output before any CH5 or substrate-close claim. |
| U-COH-004 | Does the G-Alpha auto-pass user pin need a durable V1 governance note? | Reconcile Alpha-F gate conflict (`restart/skinny/tranches/sk-v15/research/alpha/alpha-F-contract-draft.md:71`-`80`) with SK-V15 HANDOFF (`restart/skinny/tranches/sk-v15/HANDOFF.md:46`-`50`) in the next Omega/governance fold. |
