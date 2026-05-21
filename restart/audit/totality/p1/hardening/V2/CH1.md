# T-P1 V2 CH1 Correctness

Pass: T-P1 Excavation. Cycle: V2. Lens: CH1 CORRECTNESS.
Date: 2026-05-21.
Scope: `restart/audit/totality/p1/1A-substrate-evidence.md` through the
folded 1F artifacts, against V1 CH1 requirements in
`restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md`.

## Verdict

Disposition: REVISE.

V2 materially folded the V1 CH1 revisions, but not cleanly enough for
ACCEPT. The broken 1B codegen path is corrected, `LayoutFacts.backend_shape`
is no longer misclassified as absent, CSS L4 and JSON SinkOnly evidence are
preserved as admitted rows, and most negative-search claims are downgraded to
UNKNOWN with verify actions. Remaining CH1 defects are narrower: several
command-derived counts/test claims still stand without captured artifact
evidence, one 1E Lock 14 citation points at the allowance header instead of
the actual lock, and several V2 rows cite prior hardening files as evidence
where a primary source is required before synthesis.

## V1 CH1 Fold Check

| Finding | Disposition | Evidence |
|---|---|---|
| Broken 1B path citation fixed. | ACCEPT | V1 required `codegen/src/lib.rs:95-100` to become `skinny/crates/codegen/src/lib.rs:95-100` (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md:31`). V2 1B now cites `skinny/crates/codegen/src/lib.rs:92-101` and `:130-150` in the lowerer-boundary row (`restart/audit/totality/p1/1B-codegen-evidence.md:33`). The source resolves: `emit_from_source` passes `backend_ir`, `backend_shape`, `cost_facts`, and diagnostics into `emit_with_layout` at `skinny/crates/codegen/src/lib.rs:92-100`. |
| `LayoutFacts.backend_shape` corrected from absent to live-but-partial. | ACCEPT | V1 required distinguishing live `passes::LayoutFacts.backend_shape` from incomplete cost logic (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md:31`). 1A states the live field exists and narrows the gap to cost/priority/runtime materialization (`restart/audit/totality/p1/1A-substrate-evidence.md:36`, `restart/audit/totality/p1/1A-substrate-evidence.md:64`). 1B independently records side-table existence and handoff (`restart/audit/totality/p1/1B-codegen-evidence.md:36`). Source confirms population at `skinny/crates/passes/src/lib.rs:44-55` and field definitions at `skinny/crates/passes/src/lib.rs:84-92`. |
| Negative-search hygiene mostly folded. | ACCEPT | 1A explicitly routes EventCursor and old sidecar absence to UNKNOWN verify actions instead of closure (`restart/audit/totality/p1/1A-substrate-evidence.md:74-77`). 1F coherence says uncaptured scans are verify actions unless exact output is cited (`restart/audit/totality/p1/1F-coherence-scan.md:8`). 1F anti-pattern preserves the same rule for wc/rg/child-count summaries (`restart/audit/totality/p1/1F-anti-pattern.md:8`). |
| Lock 13 child-count narrowing folded. | ACCEPT | V1 required narrowing overbroad Lock 13 fanout claims to mixed-concern evidence (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md:31`). 1F now marks the child-count row `UNKNOWN mixed-concern status` (`restart/audit/totality/p1/1F-anti-pattern.md:31`) and gives a verify action to capture child lists and classify concern mixing (`restart/audit/totality/p1/1F-anti-pattern.md:82-83`). |
| CSS L4 and SinkOnly regression framing folded. | ACCEPT | 1A treats JSON SinkOnly as admitted direct evidence with remaining scheduling UNKNOWN (`restart/audit/totality/p1/1A-substrate-evidence.md:31`, `restart/audit/totality/p1/1A-substrate-evidence.md:40`) and preserves CSS declaration-values as admitted fact-stream evidence with a substrate-category gap (`restart/audit/totality/p1/1A-substrate-evidence.md:45`, `restart/audit/totality/p1/1A-substrate-evidence.md:56`). 1D also preserves the CSS PASS-ADMIT row from REDRESS/RESULTS (`restart/audit/totality/p1/1D-skinny-lessons.md:49`, `restart/audit/totality/p1/1D-skinny-lessons.md:71`). |
| Current SK-V13 pre-block / unblocked split folded. | ACCEPT | 1D adds the pre-block / unblocked table and states that "unblocked" is not route acceptance (`restart/audit/totality/p1/1D-skinny-lessons.md:79-94`). 1E folds the same route frame for lock interpretation (`restart/audit/totality/p1/1E-locks-evidence.md:41-48`). 1F past-corpora repeats the same distinction (`restart/audit/totality/p1/1F-past-corpora.md:39-52`). |
| Hidden-coupling planes folded into CH1-relevant citation hygiene. | ACCEPT | 1A explicitly classifies `StructuralIndex` and CSS source-sidecar planes without treating them as retained substrate closure (`restart/audit/totality/p1/1A-substrate-evidence.md:42`, `restart/audit/totality/p1/1A-substrate-evidence.md:44`, `restart/audit/totality/p1/1A-substrate-evidence.md:57`). 1F anti-pattern supplies source lines for `StructuralIndex`, CSS source sidecar, and proof-witness root coupling (`restart/audit/totality/p1/1F-anti-pattern.md:37-39`). |

## Findings

| ID | Disposition | Finding | Required fold |
|---|---|---|---|
| CH1-V2-001 | REVISE | 1C still makes command-result claims without a captured artifact. The executive summary says the runtime crate has 24 Rust files and that `cargo test -p runtime` ran 11 tests, all passing (`restart/audit/totality/p1/1C-runtime-evidence.md:34`), and the verification section repeats the test result (`restart/audit/totality/p1/1C-runtime-evidence.md:121-123`). Those claims do not resolve to path:line, RESULTS, REDRESS, or UNKNOWN verify_action. | Capture the exact command output into an audit artifact and cite it, or downgrade both the file/test counts and test-pass claim to UNKNOWN with verify_action. |
| CH1-V2-002 | REVISE | 1C's runtime census carries LOC counts (`json` 2,096; CSS 415; `tape` 532; runtime root 284) sourced from `find`/`wc` but not line-cited or artifact-captured (`restart/audit/totality/p1/1C-runtime-evidence.md:38-44`). V1 CH1 required captured or downgraded `wc -l` claims (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md:31`). | Either cite a committed wc transcript or convert the LOC figures to scan-derived estimates with an explicit verify_action. |
| CH1-V2-003 | REVISE | 1E miscites Lock 14 in multiple rows. `D-1E-09` cites `restart/locks/LOCKS.md:1` for the Lock 14 generic-code rule (`restart/audit/totality/p1/1E-locks-evidence.md:83`), and `LAC-1E-11` calls `restart/locks/LOCKS.md:1` the "L14 top allowance" (`restart/audit/totality/p1/1E-locks-evidence.md:101`). Line 1 is only the SK-V9 scoped allowance heading, while the actual Lock 14 rule is at `restart/locks/LOCKS.md:78`. | Replace the Lock 14 claim citation with `restart/locks/LOCKS.md:78`; keep `restart/locks/LOCKS.md:1-17` only when the row is specifically about the scoped SK-V9 allowance block. |
| CH1-V2-004 | REVISE | 1E's L13 row remains partly command-derived without a captured transcript: it states generated and bench files exceed 500 LOC and names counts for `json/generated.rs`, `report.rs`, and `passes/src/lib.rs`, then notes the `wc -l` audit is not captured (`restart/audit/totality/p1/1E-locks-evidence.md:66`). The divergence and amendment rows repeat "local `wc -l` audit" / "local `wc -l` evidence" (`restart/audit/totality/p1/1E-locks-evidence.md:82`, `restart/audit/totality/p1/1E-locks-evidence.md:97`). | Follow 1F's cleaner posture: make these exact counts UNKNOWN/verify_action until a committed transcript exists, or cite a committed generated-count artifact. |
| CH1-V2-005 | REVISE | 1D uses prior V1 CH2 hardening lines as evidence for live Sheets/BBNF-self implications (`restart/audit/totality/p1/1D-skinny-lessons.md:76-77`, `restart/audit/totality/p1/1D-skinny-lessons.md:105`). This is acceptable as a fold note, but not as primary evidence for the grammar-shape claim. 1B already has primary source lines for the same risk: JSON role mining at `skinny/crates/passes/src/lib.rs:1243-1306` and runtime profile hardcoding at `skinny/crates/codegen/src/grammar_profile.rs:89-93` (`restart/audit/totality/p1/1B-codegen-evidence.md:67-68`). | Replace hardening-file evidence in 1D's substantive rows with primary `passes`, `codegen`, SK-V13, RESULTS, or REDRESS citations; retain CH2 citations only in the hardening-fold summary. |
| CH1-V2-006 | ACCEPT | 1F's previously overbroad negative/no-match claims are now correctly fenced. `1F-past-corpora.md` says current EventCursor no-match status is UNKNOWN without captured scan (`restart/audit/totality/p1/1F-past-corpora.md:32`, `restart/audit/totality/p1/1F-past-corpora.md:59`) and gives a verify action for historical no-match claims (`restart/audit/totality/p1/1F-past-corpora.md:92`). | Preserve this pattern in V3. |

## Citation Spot Checks

| Claim | Disposition | Check |
|---|---|---|
| 1B lowerer boundary row. | ACCEPT | The 1B row cites `skinny/crates/codegen/src/lib.rs:92-101` for `emit_from_source` and `skinny/crates/codegen/src/lib.rs:130-150` for `emit_with_layout` (`restart/audit/totality/p1/1B-codegen-evidence.md:33`). The cited code resolves for the first span at `skinny/crates/codegen/src/lib.rs:92-100`; no broken V1 path remains. |
| 1F materialization-role leak row. | ACCEPT | 1F coherence cites `skinny/crates/passes/src/lib.rs:978-1119` for materialization labels (`restart/audit/totality/p1/1F-coherence-scan.md:36`, `restart/audit/totality/p1/1F-coherence-scan.md:53`). Those lines do carry `object`, `array`, `pair`, `string`, `number`, `bool`, and `null` labels. 1B separately cites `skinny/crates/passes/src/lib.rs:1243-1306` for the role-detection heuristics (`restart/audit/totality/p1/1B-codegen-evidence.md:49`, `restart/audit/totality/p1/1B-codegen-evidence.md:83`), which also resolves. |
| 1F proof-witness root coupling. | ACCEPT | 1F anti-pattern cites root proof modules at `skinny/crates/runtime/src/lib.rs:9-15` (`restart/audit/totality/p1/1F-anti-pattern.md:39`), and the source resolves to `json_event_grammar_witness` and `sheets_witness` cfg-gated exports. |
| 1A EventCursor/old sidecar absence. | ACCEPT | 1A no longer claims absence as closure. It gives explicit UNKNOWN verify actions for EventCursor and old substrate names (`restart/audit/totality/p1/1A-substrate-evidence.md:76-77`). |

## Open CH1 Requirements For V3

1. Capture or downgrade every command-derived count/test claim, including 1C runtime file/LOC/test output and 1E Lock 13 `wc -l` counts.
2. Fix 1E Lock 14 citations so substantive Lock 14 claims point to `restart/locks/LOCKS.md:78`, not the SK-V9 allowance header at `restart/locks/LOCKS.md:1`.
3. Replace hardening-file citations used as substantive evidence with primary source, RESULTS, REDRESS, or SK-V13 citations; hardening citations may remain only as fold provenance.
4. Preserve the V2 improvements: admitted CSS fact-stream evidence, admitted generated JSON SinkOnly evidence, `LayoutFacts.backend_shape` as live-but-partial, and UNKNOWN verify actions for uncaptured negative searches.
