---
agent: 1F
pass: T-P1-excavation
cycle: V6
generated_at: 2026-05-23T00:00:00-04:00
spec_surfaces_audited:
  - skinny/REDRESS.md
  - skinny/RESULTS.md
  - restart/HANDOFF.md
  - restart/skinny/INDEX.md
  - restart/skinny/tranches/sk-v5/research
  - restart/skinny/tranches/sk-v6/research
  - restart/skinny/tranches/sk-v7/research
  - restart/skinny/tranches/sk-v8/research
  - restart/skinny/tranches/sk-v9/research
  - restart/skinny/tranches/sk-v10/research
  - restart/skinny/tranches/sk-v11/research
  - restart/skinny/tranches/sk-v12/research
  - restart/skinny/tranches/sk-v13/research
  - restart/skinny/tranches/sk-v13/audit-overfit
  - restart/skinny/tranches/sk-v14/SYNTHESIS.md
  - restart/skinny/tranches/sk-v14/audit-overfit
  - restart/skinny/tranches/sk-v14/research/alpha
  - restart/skinny/tranches/sk-v14/research/p1
  - restart/audit
files_audited_count: 1428
live_truth_method: "find restart/skinny/tranches/sk-v{1..14} -name '*.md' | wc -l = 1428; SK-V14 alpha-C-redress-digest verbatim P-list cite; audit-overfit synthesis §1 + §1.3 cited verbatim"
prior_cycle_dispositions_folded:
  accepted:
    - PC-001  # dispatch-table/function-pointer alternates rejected (kept)
    - PC-002  # 12-byte token churn rejected (kept; superseded by lazy-offset)
    - PC-003  # EventCursor sidecar/prepass rejected (kept; ARCH §1571-1580 lowering-boundary-only)
    - PC-004  # Class A tiny-string NEON not the parse-G fix (kept)
    - PC-005  # bench-private SinkParser dishonesty closed (kept)
    - PC-006  # SinkOnly-from-BIR landed with residual (kept)
    - PC-007  # SK-V12 RESULTS A/Go drift vs HANDOFF (refreshed against SK-V14)
    - PC-008  # bbnf-simd Lock 14 partial-closure (kept verify-before-rederive posture)
  rejected: []
  revised:
    - SKV13-PB-001..008  # current SK-V13 pre-blocks: refreshed AND overlaid by SK-V14 audit pack falsification
  first_cycle_additions:
    - PC-009  # SK-V14 P-1 fake @generated header pattern (binding)
    - PC-010  # SK-V14 P-2 sonic_rs::from_slice::<Value> as fake strict comparator
    - PC-011  # SK-V14 P-3 tiny-fixture Criterion-overhead Mbps inflation (<400 bytes)
    - PC-012  # SK-V14 P-4 gate-relabel as admit
    - PC-013  # SK-V14 P-5 scaffold-research counted as load-bearing
    - PC-014  # SK-V14 P-6 per-grammar provider modules in generic codegen
    - PC-015  # SK-V14 P-7 Track 1 ≡ Track 2 dishonesty
    - PC-016  # SK-V14 audit-zero baseline supersedes SK-V13 nominal closes (binding)
    - PC-017  # 67-file Pattern H census (+3 vs V13's 64; css_pretty addition)
divergence_count:
  spec_claims_implemented: 6
  spec_claims_unimplemented: 11
  impl_exceeds_spec: 1
  unknown: 3
locks_amendment_candidates: 0
---

## Executive Summary

The prior-corpora ledger expands materially under SK-V14. The SK-V13 audit pack remains the most recent durable empirical floor and SK-V14 S-P0 reproduces 54 of 74 findings byte-for-byte; SK-V14 adds seven pattern-level pre-blocks (P-1..P-7) binding on every downstream wave. The totality cycle must not re-derive: (a) the six classical REDRESS rejections (PC-001..PC-006); (b) the SK-V13 G1-G7 pre-blocks (SKV13-PB-001..008), which remain ledger constraints but are now SK-V14-overlaid by the audit-falsification verdict; (c) the SK-V14 P-list pre-blocks (PC-009..PC-015), each barring a production technique (fake `@generated`; sonic-rs eager-DOM as strict comparator; tiny-fixture Mbps inflation under ~400 bytes; gate-relabel as admit; scaffold-as-load-bearing; per-grammar provider modules in generic codegen; Track 1 ≡ Track 2 plane collapse); (d) the audit-zero baseline `0/17 + 0/17 + 0/17 + 0/24` (PC-016, binding); (e) the Pattern H 67-file census (PC-017, +3 vs the V13 baseline of 64 from the `css_pretty` addition; PRUNE-4 sub-wave count = 9 not 8). V2 treats accepted historical pre-blocks as ledger constraints, not closure claims; V6 promotes the SK-V14 P-list and audit-zero baseline to first-class binding inputs.

## Spec-Claim ↔ Implementation Table

| ID | Prior finding (path:line) | Current evidence / SK-V14 binding (path:line) | Verdict | LOC / risk | Note |
|---|---|---|---|---|---|
| PC-001 | Dispatch-table/function-pointer alternate rejected at `skinny/REDRESS.md:216-224`. | Rust `match` canonical dispatch preserved; SK-V14 SYNTHESIS §0.4 leaves alternate-dispatch outside the P-list. | accepted historical pre-block; current absence UNKNOWN | 0 LOC; medium if reopened | Do not re-run as default optimization idea. Route to verify_action (parity with PC-003): capture exact `rg -n 'function_pointer\|dispatch_table' crates/ skinny/crates/` output before promoting to live absence. |
| PC-002 | 12-byte skipless token shape rejected as canonical at `skinny/REDRESS.md:226-234`. | Lazy-offset tape migration supersedes old token churn at `skinny/REDRESS.md:246-256`. | accepted historical pre-block; current absence UNKNOWN | 0 LOC; medium if reopened | Token-width churn is not the next substrate route. Route to verify_action (parity with PC-003): capture exact `rg -n 'Token12\|skipless_token' crates/ skinny/crates/` output before promoting to live absence. |
| PC-003 | EventCursor sidecar/prepass rejected at `restart/skinny/tranches/sk-v5/research/skv5-A4-tape-union-audit.md:57-80`. | ARCH §1571-1580 requires cursor as lowering boundary, not retained sidecar; current no-match scan UNKNOWN absent captured `rg` output. | accepted historical pre-block; current absence UNKNOWN | 0-160 LOC; high if reopened | Capture exact `rg -n 'EventCursor|generated_eventcursor'` before claiming live absence. |
| PC-004 | Class A tiny-string NEON wiring invalidated as parse-G fix at `skinny/REDRESS.md:394-413`. | `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs` remains a primitive; no current result row claims it closes parse-G. | accepted historical pre-block; current absence UNKNOWN | 0 LOC; medium | Primitive admission is not route admission. Route to verify_action (parity with PC-003): capture exact `rg -n 'match_tiny_plain_string\|tiny_string_neon' skinny/crates/runtime/ skinny/crates/codegen/` output before promoting to live absence (current absence here means absence of any production-path callsite, not absence of the primitive itself). |
| PC-005 | Bench-private SinkParser dishonesty identified and closed at `skinny/REDRESS.md:420-438`. | Generated `parse_direct` exists at `skinny/crates/runtime/src/grammars/json/generated.rs:393-407`; REDRESS says Track 1 calls generated runtime at `skinny/REDRESS.md:535-557`. | implemented | 0 LOC; low if respected | Do not treat old sink-only throughput rows as generated-code evidence. |
| PC-006 | SinkOnly lowerer now consumes BIR but no throughput claim at `skinny/REDRESS.md:662-683`. | `skinny/crates/codegen/src/lib.rs:145-150` requires lowered `SinkOnlyProgram`. | implemented with residual | 0 LOC for authority; throughput separate | Codegen honesty closed; performance remains separate. |
| PC-007 | `restart/HANDOFF.md` and `restart/skinny/INDEX.md` declared SK-V6 / N-direct NoGo. | Refreshed: `skinny/RESULTS.md:145-148` reports SK-V12 `A / Go`; SK-V14 audit pack falsifies SK-V13 nominal closes back to `0/17 + 0/24`. | corpus drift (now triple-stale) | 120-220 LOC; high | Totality must fold SK-V14 audit-corrected baseline before reusing any SK-V13 / SK-V12 conclusion. |
| PC-008 | SK-V5 Lock 14 `bbnf-simd` JSON hardcoded scalar references pending at `skinny/REDRESS.md:460-478`. | `bbnf-simd` exposes generic alphabet/table APIs at `skinny/crates/bbnf-simd/src/lib.rs:20-49`; SK-V14 axis A3 D1 records `StringFlags::HAS_ESC` naming as DELTA-NOTE only, not violation. | revised/partially closed; SK-V5 verify-before-rederive obligation retained (carried as `U-PC-002` below) | 80-200 LOC verify; medium | 1E/CH2 verifies all old JSON alphabet sites are gone or are documentary. 1D row 113 must cross-cite PC-008 + `U-PC-002` so the SK-V14 axis A3 "grammar-neutral substrate PROVED" verdict carries this open-question pointer; verify_action = run targeted `rg -n 'JSON_STRUCTURAL\|scan_json\|JsonParseIndex' skinny/crates/bbnf-simd skinny/crates/runtime` and cite every remaining hit. |
| PC-009 | NEW. SK-V14 P-1 (alpha-C-redress-digest §2.1): fake `@generated` header on hand-written templates. | All 7 CSS L4 providers `include_str!()` hand-written templates with fake `@generated`; recurrence vector for the entire CSS L4 fake-admit cluster; nested_layout (124× anomaly) carries preemptive round-trip-rule trigger. | accepted SK-V14 pre-block | 0 LOC; high if reopened | SK-V14 PRUNE-2 deletes; R4 lands `cargo xtask regen-css` first. |
| PC-010 | NEW. SK-V14 P-2: `sonic_rs::from_slice::<Value>` mislabelled as strict comparator. | `audit-overfit/validation/v6-comparator-integrity.md §1 + §3`; eager-DOM API was bound for all three planes. SK-V14 R1 binds three plane-correct comparators. | accepted SK-V14 pre-block | 0 LOC; high if reopened | Do not use eager-DOM API as strict-typed comparator. |
| PC-011 | NEW. SK-V14 P-3: tiny-fixture Criterion-overhead Mbps inflation under ~400 bytes. | 85-357-byte fixtures embedded in bench source produce ~54 ns/parse Criterion-dominated measurements; SK-V14 R5 mandates ≥800 KB corpora; rows on <1 KB fixtures cannot admit. | accepted SK-V14 pre-block | 0 LOC; high if reopened | No row admit on tiny fixtures. |
| PC-012 | NEW. SK-V14 P-4: gate-relabel as admit. | W14.1-5 source diffs touched only `gate.rs` / `report.rs` / `lock14_baseline.rs`; the parser was unchanged. SK-V14 admit requires per-row parser/codegen source delta + measurement evidence. | accepted SK-V14 pre-block | 0 LOC; high if reopened | No bench-config-only admit. |
| PC-013 | NEW. SK-V14 P-5: scaffold-research counted as load-bearing. | W8 + W9 documented facts without runtime consumer; SK-V14 PRUNE-5 wires both end-to-end; no row admit cites W8 / W9 until runtime consumer is measured. | accepted SK-V14 pre-block | 0 LOC until PRUNE-5; high if reopened | SCAFFOLD-only is not load-bearing. |
| PC-014 | NEW. SK-V14 P-6: per-grammar provider modules in generic codegen. | `v3-lock14-deep-scan.md §1` (3 CRIT + 4 HIGH) — 8 hand-written per-grammar provider modules under `skinny/crates/codegen/` are the Lock-14 recurrence vector. PRUNE-3 collapses to ONE grammar-agnostic generator template. | accepted SK-V14 pre-block (active live violation) | 400-900 LOC PRUNE-3; high | Live impl evidence in `1F-anti-pattern.md` AP-012. |
| PC-015 | NEW. SK-V14 P-7: Track 1 ≡ Track 2 dishonesty. | Per cross-reference to prior Lock 1 violations + PASS-ALPHA.md §3W CH5 lens; SK-V14 bench harness must keep Track 1 (generated) structurally distinct from Track 2 (independent oracle); any plane collapse fails gate. | accepted SK-V14 pre-block | 0 LOC; high if reopened | Maintain strict Track 1 / Track 2 separation. |
| PC-016 | NEW. SK-V14 SYNTHESIS §0.2 audit-zero baseline: JSON `parse_only` 0/17, JSON `direct_to_struct` 0/17, JSON `real_typed_struct` 0/17, CSS L4 0/24. | All four populations reclassify AUDIT-FALSIFIED under v6 §1 rows 3-4 (same comparator-misbinding pattern). The 25 CSS L4 + 5 parse_only + 4 direct + 7 typed admits in `ROLLING-SOTA-DELTA.md` at `653cdf795+w15.1-redress` are FALSIFIED. | binding empirical floor | 0 LOC for ledger; high orchestration risk | Every V1 governance surface must restate this floor (see 1F-coherence-scan.md COH-007). |
| PC-017 | NEW. SK-V14 audit-overfit §1.3 Pattern H 67-file census. | Live: 9 grammar dirs at `crates/core/src/runtime/` carrying per-grammar census `bbnf=8, bnf=7, css_l4=7, css_pretty=7, csv=7, ebnf=7, google_sheets=10, json=7, math=7 = 67` (verified `find crates/core/src/runtime/google_sheets -type f \| wc -l` = 10 at HEAD 2026-05-23; matches S-P0 A6 baseline `sk-v14-audit-overfit-pre-restart-pattern.md:53` `google_sheets/ = 10`). V13 baseline was 64; +3 from `css_pretty` addition. | impl-exceeds-spec (drift +3) | 0 LOC for census; 1500-3000 LOC for PRUNE-4 | PRUNE-4 sub-wave count = 9 not 8; wave manifest must reflect. |

## Current SK-V13 + SK-V14 Pre-Block Table

The SK-V13 G1-G7 pre-blocks remain binding as ledger constraints, now overlaid by the SK-V14 audit-falsification verdict per `restart/skinny/tranches/sk-v14/SYNTHESIS.md:54-83`.

| ID | SK-V13 disposition | SK-V14 overlay | V6 implication |
|---|---|---|---|
| SKV13-PB-001 | Do not claim SK-V13 close from the single SK-V12 CSS declaration-values row at `tranches/sk-v13/SYNTHESIS.md:241`. | SK-V14 PRUNE-2 reverts all 24 CSS L4 rows including W1b; the single row is no longer admitted evidence. | CSS admission remains evidence only AFTER R4+R5+R6 land. |
| SKV13-PB-002 | Do not use lossy/permissive/different-plane comparators as SOTA anchors. | SK-V14 R1 binds three plane-correct strict comparators per plane. | Comparator plane must be explicit per row; P-2 enforces. |
| SKV13-PB-003 | Do not treat `parse_only` as diagnostic-only at `tranches/sk-v13/SYNTHESIS.md:244`. | SK-V14 R8 stands up a distinct `parse_only` code path in `generated_json` then wires to Skipper-class comparator. | Parse-only rows need R8 + fresh strict evidence. |
| SKV13-PB-004 | Do not close JSON through REDRESS-119 history. | SK-V14 R7 re-baselines every JSON direct + typed row against rebound strict comparators. | Prior direct fixpoint is not current closure authority. |
| SKV13-PB-005 | Do not accept producer-only SIMD/union/resolver/codegen artifacts without same-wave consumer. | SK-V14 PRUNE-5 wires W8/W9 to LOAD-BEARING; every primitive-producing row needs same-wave consumer metadata. | Producer-only is SCAFFOLD-ONLY; no admit. |
| SKV13-PB-006 | Do not reuse non-JSON/shared `bbnf-simd` alphabet-only dispatch without `G-SIMD-GRAMMAR-POLICY`. | SK-V14 axis A3 D1 confirms `bbnf-simd` STAYS CLEAN; future-rename concern only. | Verify-before-rederive; no new admit absent G-SIMD-GRAMMAR-POLICY landing. |
| SKV13-PB-007 | Do not admit grammar-name branches, parser-owned sidecars, hidden Track 1/Track 2 coupling, or stale comparator sidecars. | SK-V14 P-7 binds Track 1 ≡ Track 2 separation; live evidence in 1F-anti-pattern.md AP-010/AP-011. | Folded into 1F anti-pattern rows; PRUNE-3/4 + proof-surface waves close. |
| SKV13-PB-008 | Do not dispatch Wave 0 before G-Omega closes. | SK-V14 dispatch context binds T-P1/T-P2/T-P3 to run alongside S-P1/S-P2/S-P3; G-Omega remains the implementation-wave gate. | Work-order rows need wave metadata + hard cap. |

## Divergences Catalogued

The ID-keyed `V2 Planning Metadata` table below is the authoritative CH4 carrier; this index is structural only.

| ID | Divergence / pre-block | Evidence | LOC / risk |
|---|---|---|---|
| PC-001 | Rejected alternates remain blocked absent new measurements. | `skinny/REDRESS.md:216-224,291-297` | 0 LOC; medium |
| PC-002 | EventCursor can be a lowering boundary only, not a parallel prepass. | `restart/ARCHITECTURE.md:1571-1580`; SK-V5 research; current no-match UNKNOWN absent captured scan | 0-160 LOC; high |
| PC-003 | Direct-to-struct codegen honesty is closed; do not re-diagnose as symbol absence. | `skinny/REDRESS.md:440-458,662-683` | 0 LOC; low |
| PC-004 | Direct throughput is a residual, not proof that SinkOnly is absent. | `skinny/REDRESS.md:535-557`; `skinny/RESULTS.md:98-144` | 0 LOC for diagnosis; high if conflated |
| PC-005 | SK-V14 audit pack triple-stales the HANDOFF/INDEX/MASTER current-state authority. | `restart/HANDOFF.md:3-12`; `restart/skinny/tranches/sk-v14/SYNTHESIS.md:54-83` | 120-220 LOC; high |
| PC-006 | Lock 14 pending findings require verify-before-rederive. | `skinny/REDRESS.md:460-498`; `skinny/crates/bbnf-simd/src/lib.rs:20-49` | 80-200 LOC verify; medium |
| PC-009 | Fake `@generated` header on hand-written templates (CSS L4 + JSON `json_provider::normalize`). | SK-V14 alpha-C §2.1; live AP-013/AP-014 | 0 LOC pre-block; high if reopened |
| PC-010 | `sonic_rs::from_slice::<Value>` as strict comparator. | SK-V14 alpha-C §2.2 | 0 LOC; high if reopened |
| PC-011 | Tiny-fixture (<400 B) Criterion-overhead Mbps inflation. | SK-V14 alpha-C §2.3 | 0 LOC; high if reopened |
| PC-012 | Gate-relabel as admit (no parser/codegen diff). | SK-V14 alpha-C §2.4 | 0 LOC; high if reopened |
| PC-013 | Scaffold-research counted as load-bearing (W8/W9). | SK-V14 alpha-C §2.5 | 0 LOC until PRUNE-5; high if reopened |
| PC-014 | Per-grammar provider modules in generic codegen. | SK-V14 alpha-C §2.6; live AP-012 | 400-900 LOC PRUNE-3; high |
| PC-015 | Track 1 ≡ Track 2 dishonesty. | SK-V14 alpha-C §2.7 | 0 LOC; high if reopened |
| PC-016 | Audit-zero baseline: 0/17 + 0/17 + 0/17 + 0/24. | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:54-65` | 0 LOC ledger; high orchestration |
| PC-017 | Pattern H 67-file census (+3 vs V13 64); per-grammar breakdown `bbnf=8, bnf=7, css_l4=7, css_pretty=7, csv=7, ebnf=7, google_sheets=10, json=7, math=7 = 67`. | `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:194-211`; live `find crates/core/src/runtime/google_sheets -type f \| wc -l` = 10 (2026-05-23) | 0 LOC census; 1500-3000 LOC PRUNE-4 |

## V2 Planning Metadata (authoritative CH4 carrier)

| ID | loc_budget | risk | wave | hard_cap | same_wave_consumer | evidence_basis |
|---|---:|---|---|---:|---|---|
| PC-001 | 0 LOC | medium if reopened | all waves | 0 LOC unless fresh row | fresh before/after measurement | `skinny/REDRESS.md:216-224` |
| PC-002 | 0-160 LOC audit/fencing | high if reopened | substrate-fencing wave | 220 LOC | retained-substrate audit consumer | `restart/ARCHITECTURE.md:1571-1580` |
| PC-003 | 0 LOC for diagnosis | low | closed authority | 0 LOC | none | `skinny/REDRESS.md:440-458,662-683` |
| PC-004 | 0 LOC diagnosis | high if conflated | SK-V13 row waves | row-specific cap | strict row benchmark consumer | `skinny/REDRESS.md:535-557` |
| PC-005 | 120-220 LOC docs | high | T-P3 governance | 280 LOC | current-state authority pages | `restart/HANDOFF.md:3-12`; `restart/skinny/tranches/sk-v14/SYNTHESIS.md:54-83` |
| PC-006 | 80-200 LOC verify | medium | SIMD policy wave | 260 LOC | same-wave SIMD consumer | `skinny/REDRESS.md:460-498` |
| PC-009 | 0 LOC pre-block | high if reopened | SK-V14 PRUNE-2 (gated by R4) | 0 LOC ledger; PRUNE-2 cap per AP-014 | `regen-css` xtask consumer | SK-V14 alpha-C §2.1 |
| PC-010 | 0 LOC pre-block | high if reopened | SK-V14 R1 | 0 LOC ledger; R1 cap per harness | three plane-correct comparators | SK-V14 alpha-C §2.2 |
| PC-011 | 0 LOC pre-block | high if reopened | SK-V14 R5 | 0 LOC ledger; R5 cap per corpus | ≥800 KB corpora consumer | SK-V14 alpha-C §2.3 |
| PC-012 | 0 LOC pre-block | high if reopened | SK-V14 PRUNE-1 | 0 LOC ledger; per-row cap on parser/codegen delta | strict admit reviewer | SK-V14 alpha-C §2.4 |
| PC-013 | 0 LOC until PRUNE-5 | high if reopened | SK-V14 PRUNE-5 | per-wave cap | runtime consumer of W8/W9 | SK-V14 alpha-C §2.5 |
| PC-014 | 400-900 LOC | high | SK-V14 PRUNE-3 | 1200 LOC | grammar-agnostic generator template | SK-V14 alpha-C §2.6 |
| PC-015 | 0 LOC pre-block | high if reopened | all waves | per-row cap | Track 1 / Track 2 plane separator | SK-V14 alpha-C §2.7 |
| PC-016 | 0 LOC ledger | high | T-P3 governance | 120 LOC | HANDOFF/MASTER/INDEX/LOCKS audit-zero anchors | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:54-65` |
| PC-017 | 0 LOC census; 1500-3000 LOC PRUNE-4 | high | SK-V14 PRUNE-4 (9 sub-waves) | 4000 LOC | per-grammar generator template | `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:194-211` |
| SKV13-PB-005 | producer LOC varies | high | same wave as primitive | row-specific cap | required for SIMD/union/resolver/codegen artifacts | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:246-247` |
| SKV13-PB-008 | 0 LOC until G-Omega | high | G-Omega first | 0 LOC before unblock | G-Omega closure | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:253` |

## Gaps / Missing Primitives

| Gap | Evidence | LOC / risk |
|---|---|---|
| No compact "do not rederive" ledger spans SK-V5 → SK-V14. | 1428 .md files under `restart/skinny/tranches/sk-v{1..14}/` (live `find` count); no single index. | 200-400 LOC ledger; high process risk |
| No single row maps REDRESS accepted/rejected items + SK-V14 audit P-list to current code symbols and current result rows. | REDRESS direct close at `skinny/REDRESS.md:535-557`; SK-V14 P-list at `restart/skinny/tranches/sk-v14/SYNTHESIS.md:106-149`. | 300-600 LOC tooling/report; medium-high |
| Lock 14 pending findings from SK-V5 lack explicit current disposition under SK-V14 axis A3. | SK-V5 pending rows at `skinny/REDRESS.md:460-498`; SK-V14 axis A3 carries 30 violations + 1 DELTA-NOTE. | 80-200 LOC verify; medium |
| SK-V14 R-target goalset (R1-R10) is not mirrored on any V1 spec surface. | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:88-103` only. | 100-200 LOC; high |

## Open Questions

| UNKNOWN | Blocking question | verify_action |
|---|---|---|
| U-PC-001 | Are all SK-V13 G1-G7 dispositions correctly overlaid by SK-V14 audit pack? | T-P3 reads `restart/skinny/tranches/sk-v14/SYNTHESIS.md §1.1-1.2` end-to-end + `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §0-§1` and emits a per-G overlay table. |
| U-PC-002 | Are the SK-V5 pending Lock 14 `bbnf-simd` findings fully closed under SK-V14 axis A3 D1? | Run targeted `rg -n 'JSON_STRUCTURAL|scan_json|JsonParseIndex' skinny/crates/bbnf-simd skinny/crates/runtime` + cite every remaining hit; A3 says CLEAN with rename concern only. |
| U-PC-003 | Which historical no-match claims (EventCursor, alternate dispatch, stale comparator sidecar names) are still true on the live SK-V14 tree? | Capture exact `rg` output before promoting accepted historical pre-blocks to live absence claims. |
