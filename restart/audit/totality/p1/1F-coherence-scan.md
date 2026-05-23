---
agent: 1F
pass: T-P1-excavation
cycle: V6
generated_at: 2026-05-23T00:00:00-04:00
spec_surfaces_audited:
  - restart/ARCHITECTURE.md
  - restart/MASTER-PLAN.md
  - restart/locks/LOCKS.md
  - restart/HANDOFF.md
  - restart/skinny/INDEX.md
  - skinny/REDRESS.md
  - skinny/RESULTS.md
  - restart/skinny/tranches/sk-v14/SYNTHESIS.md
  - restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
  - restart/skinny/tranches/sk-v14/research/alpha/alpha-C-redress-digest.md
  - restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md
files_audited_count: 3974
live_truth_method: "wc -l + grep -n + find for spec-surface anchors; SK-V14 audit-overfit synthesis as binding ground-truth for the audit-corrected baseline; no recalled LOC"
prior_cycle_dispositions_folded:
  accepted:
    - COH-001  # HANDOFF/INDEX vs RESULTS authority drift (extended to SK-V14 binding)
    - COH-002  # 2-vs-3-surface grammar onboarding drift
    - COH-004  # BackendShape 8-step derivation surface vs tests
    - COH-005  # ARCH self-acknowledged JSON-shape pass leaks
  rejected: []
  revised:
    - COH-003  # H-wave gate drift retained but evidence anchors refreshed against SK-V14 audit pack
    - COH-006  # INDEX SK-V12-vs-SK-V14 lag rewritten as SK-V13/SK-V14-vs-spec lag
  first_cycle_additions:
    - COH-007  # spec-surface vs SK-V14 audit-zero baseline (0/17 + 0/24) drift
    - COH-008  # 30 Lock-14-violation census not reflected in LOCKS or ARCH
    - COH-009  # PRUNE-1..PRUNE-5 wave sequencing absent from MASTER-PLAN wave manifest
    - COH-010  # P-1..P-7 pattern pre-blocks absent from MASTER/ARCH/LOCKS
    - COH-011  # nine-grammar `crates/core/src/runtime/` census vs ARCH/spec
    - COH-012  # CH7 Overfit-Prune lens not registered in PASS-1-EXCAVATION.md §3
divergence_count:
  spec_claims_implemented: 4
  spec_claims_unimplemented: 8
  impl_exceeds_spec: 2
  unknown: 3
locks_amendment_candidates: 0
---

## Executive Summary

The cross-document coherence picture is dominated by a single rupture: the SK-V14 audit pack and its 74-finding S-P0 prune list reset the empirical baseline to JSON `parse_only` 0/17, JSON `direct_to_struct` 0/17, JSON `real_typed_struct` 0/17, CSS L4 0/24, yet none of the four V1 governance surfaces (`ARCHITECTURE.md`, `MASTER-PLAN.md`, `LOCKS.md`, `HANDOFF.md`) reflect this restated floor. `restart/HANDOFF.md:12` still claims `skinny/RESULTS.md` at SK-V13 head is the current measured authority; `restart/skinny/INDEX.md:5-13` still routes through SK-V13 W0; `restart/MASTER-PLAN.md:512-575` still keys H.W4/H.W6 receivers off SK-V13 G1-G7 without an SK-V14 PRUNE prelude; `restart/ARCHITECTURE.md:1166` cites the SK-V13 reopen ledger without naming the SK-V14 audit-falsification overlay. Twelve catalogued coherence rows fall out: six are inherited from the prior cycle (COH-001..006 refreshed against the SK-V14 binding); six are new (COH-007..012 — audit-zero baseline absence, 30-Lock-14-violation census silence, PRUNE-1..PRUNE-5 wave-sequencing gap, P-1..P-7 pattern pre-block silence, the nine-grammar `crates/core/src/runtime/` totality census, and the CH7 Overfit-Prune lens not registered in PASS-1-EXCAVATION.md §3). Companion outputs are warranted: `1F-anti-pattern.md` carries the live-code anti-pattern evidence (with SK-V14 P-list classification); `1F-past-corpora.md` carries the prior-corpora pre-block ledger including the SK-V13/SK-V14 audit-pack pre-blocks.

## Spec-Claim ↔ Implementation Table

| ID | Spec / authority claim (path:line) | Counter-surface / SK-V14 binding (path:line) | Verdict | LOC / risk | Note |
|---|---|---|---|---|---|
| COH-001 | `restart/HANDOFF.md:12` declares "Current measured authority is `skinny/RESULTS.md` at SK-V13 head". | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:54-65` declares the SK-V13 totals AUDIT-FALSIFIED and the SK-V14 obligation is `0/17` + `0/24` reopens; `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:25-43` records aggregate 74-finding FAIL with 31 CRIT + 20 HIGH. | spec-surface drift | 120-220 LOC doc reconciliation; high orchestration risk | The current-cycle authority page is two tranches stale. |
| COH-002 | `restart/HANDOFF.md:17` says onboarding a grammar requires two declarative surfaces (`.bbnf` + workspace metadata). | `restart/locks/LOCKS.md:220` says three surfaces, including optional per-grammar declaration crate; `restart/ARCHITECTURE.md:37` calls the declaration crate a rare escape valve. | spec-surface drift | 20-40 LOC doc edit; medium Lock 14 onboarding risk | Two-surface and three-surface wording must be normalised. |
| COH-003 | `restart/MASTER-PLAN.md:512-575` keys H.W4/H.W6 close gates off the SK-V13 G1-G7 receiver map. | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:88-103` re-binds the close gates to SK-V14 R1-R10 PRUNE waves before any new admit. | spec-surface drift | 60-120 LOC doc edit; high gate-routing risk | The R-target goalset is not visible in MASTER. |
| COH-004 | `restart/ARCHITECTURE.md:1090-1098` defines the 8-step `BackendShape` derivation; `restart/locks/LOCKS.md:164` says Pratt + SIMD auto-detect through the cost model. | `skinny/crates/passes/src/lib.rs:44-45` calls shape derivation but unit tests pin every JSON rule to `OffsetTape` at `skinny/crates/passes/src/lib.rs:1497-1503`; `skinny/crates/codegen/src/grammar_profile.rs:11-25` hand-enumerates two `RuntimeProvider` variants then seven CSS variants. | partially implemented | 150-300 LOC + tests; high cost-model risk | Selection surface exists; measured per-rule selection is not proven by current tests. |
| COH-005 | `restart/ARCHITECTURE.md:1129-1131` itself states remaining generic-pass grammar-specific mining must be removed. | `skinny/crates/passes/src/lib.rs` is 1869 LOC; `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (axis A3) certifies 30 Lock-14 violations still resident. | unimplemented cleanup | 300-600 LOC; high Lock 14 risk | Spec debt = live code debt; the SK-V14 PRUNE-3 wave is the closure path. |
| COH-006 | `restart/skinny/INDEX.md:5-17` keys the current dispatch posture on SK-V13 SYNTHESIS / SK-V13 DISPATCH-PROMPT and the SK-V12 close packet. | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:1-8` is the active SK-V14 contract draft (date 2026-05-22); SK-V14 PRUNE waves invalidate every SK-V13 nominal close `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:25-43`. | spec-surface drift | 80-160 LOC doc/authority update; high gate risk | INDEX is the entry-page; it must point to SK-V14, not SK-V13. |
| COH-007 | NEW. No V1 governance surface restates the audit-zero baseline `0/17 + 0/17 + 0/17 + 0/24`. | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:54-65` is the only surface carrying it; `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:40-43` confirms. | spec-claims-unimplemented (silent-must-add) | 40-80 LOC doc edit (one row each in HANDOFF + MASTER + INDEX); high orchestration risk | Empirical floor must be stated everywhere the spec claims a working SK-V13 receiver. |
| COH-008 | NEW. `restart/locks/LOCKS.md:220` text bans grammar-name leaks in generic crates; no surface tracks the live 30-violation census. | `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-lock14-scan.md` (axis A3) records 11 CRIT + 7 HIGH + 5 MED + 7 LOW = 30 violations; the 8 hand-written per-grammar provider modules under `skinny/crates/codegen/` are the recurrence vector. | spec-claims-unimplemented (silent-must-add) | 60-120 LOC LOCKS / ARCH addendum; high enforcement risk | The lock requires `rg`-verifiable invariants and Pattern H is uncited. |
| COH-009 | NEW. `restart/MASTER-PLAN.md:564-575` enumerates MP.NW0..MP.NW10 receivers; no entry sequences SK-V14 PRUNE-1..PRUNE-5. | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:88-103` binds R3 PRUNE-1..PRUNE-5 + R4 + R5 as the gating wave-prelude; `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:53-59` recites the `PASS-0-OVERFIT-AUDIT.md §Failure mode` clause halting the campaign until prune converges. | spec-claims-unimplemented | 100-200 LOC MASTER amendment; high wave-routing risk | The wave manifest cannot dispatch new admit waves until PRUNE is enumerated. |
| COH-010 | NEW. P-1..P-7 pattern-level pre-blocks are absent from MASTER / ARCH / LOCKS. | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:106-149` enumerates seven pattern pre-blocks (fake `@generated`, sonic-rs eager-DOM, tiny-fixture inflation, gate-relabel, scaffold-as-load-bearing, per-grammar provider modules, Track 1 ≡ Track 2). | spec-claims-unimplemented (silent-must-add) | 80-160 LOC pre-block ledger (one anchor each in MASTER + LOCKS); high regression risk | Without P-list registration, future tranches re-derive the same fake-patterns. |
| COH-011 | NEW. `restart/ARCHITECTURE.md:765` cites the eight-grammar set `(bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math)` but is not authoritative on per-grammar file counts. | Live `find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d \| wc -l` returns 9; per-grammar census `bbnf=8, bnf=7, css_l4=7, css_pretty=7, csv=7, ebnf=7, google_sheets=6, json=7, math=7 = 67 hand-written files` confirmed at `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:194-211`. | impl-exceeds-spec (drift +3 vs V13 baseline of 64) | 0 LOC for census; 600-1200 LOC for PRUNE-4 generator rollout | The +3 delta comes from the `css_pretty` directory addition; PRUNE-4 sub-wave count is 9 not 8. |
| COH-012 | NEW. `restart/prompts/totality/PASS-1-EXCAVATION.md:91-138` registers six lenses CH1..CH6; no CH7 line. | `restart/locks/LOCKS.md:46` declares "Lock 14 + CH7 Overfit-Prune lens binding"; `restart/skinny/tranches/sk-v14/SYNTHESIS.md:22` cites the same CH7 binding. | spec-surface drift (impl ahead of spec) | 30-60 LOC `PASS-1-EXCAVATION.md §3` extension | The CHALLENGE wave omits the Overfit-Prune lens the SK-V14 contract relies on. |

## Divergences Catalogued

The ID-keyed `V2 Planning Metadata` table is the authoritative CH4 carrier; this index is structural only.

| ID | Divergence | Evidence | LOC / risk |
|---|---|---|---|
| COH-001 | HANDOFF stale against SK-V14 audit-corrected baseline. | `restart/HANDOFF.md:12`; `restart/skinny/tranches/sk-v14/SYNTHESIS.md:54-65` | 120-220 LOC; high |
| COH-002 | Two-surface vs three-surface grammar onboarding wording. | `restart/HANDOFF.md:17`; `restart/locks/LOCKS.md:220`; `restart/ARCHITECTURE.md:37` | 20-40 LOC; medium |
| COH-003 | MASTER H.W-wave receivers key on SK-V13 G1-G7, not SK-V14 R1-R10. | `restart/MASTER-PLAN.md:512-575`; `restart/skinny/tranches/sk-v14/SYNTHESIS.md:88-103` | 60-120 LOC; high |
| COH-004 | BackendShape selection surface exists; tests pin every JSON rule to OffsetTape. | `restart/ARCHITECTURE.md:1090-1098`; `skinny/crates/passes/src/lib.rs:1497-1503`; `skinny/crates/codegen/src/grammar_profile.rs:11-25` | 150-300 LOC; high |
| COH-005 | Spec acknowledges generic-pass grammar leaks; SK-V14 axis A3 confirms 30 unresolved Lock-14 violations. | `restart/ARCHITECTURE.md:1129-1131`; `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (axis A3); `skinny/crates/passes/src/lib.rs:324-349` | 300-600 LOC; high |
| COH-006 | INDEX still routes through SK-V13 SYNTHESIS / SK-V12 close packet. | `restart/skinny/INDEX.md:5-17`; `restart/skinny/tranches/sk-v14/SYNTHESIS.md:1-50` | 80-160 LOC; high |
| COH-007 | Audit-zero baseline (`0/17 + 0/17 + 0/17 + 0/24`) not stated on any V1 surface. | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:54-65`; absent in HANDOFF/MASTER/INDEX/LOCKS/ARCH | 40-80 LOC; high |
| COH-008 | 30 Lock-14 violations census uncited on LOCKS/ARCH. | `audit-overfit/sk-v14-audit-overfit-lock14-scan.md`; `restart/locks/LOCKS.md:220` | 60-120 LOC; high |
| COH-009 | PRUNE-1..PRUNE-5 wave sequencing absent from MASTER wave manifest. | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:88-103`; `restart/MASTER-PLAN.md:564-575` | 100-200 LOC; high |
| COH-010 | P-1..P-7 pattern pre-blocks unregistered in MASTER/ARCH/LOCKS. | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:106-149` | 80-160 LOC; high |
| COH-011 | Nine-grammar `crates/core/src/runtime/` census (67 files) drifts +3 vs prior baseline; PRUNE-4 sub-waves 9 not 8. | live `find` output; `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:194-211` | 0 LOC census; 600-1200 LOC PRUNE-4 | medium-high |
| COH-012 | CH7 Overfit-Prune lens missing from PASS-1-EXCAVATION.md §3 registry. | `restart/prompts/totality/PASS-1-EXCAVATION.md:91-138`; `restart/locks/LOCKS.md:46` | 30-60 LOC; medium |

## V2 Planning Metadata (authoritative CH4 carrier)

| ID | loc_budget | risk | wave | hard_cap | same_wave_consumer | evidence_basis |
|---|---:|---|---|---:|---|---|
| COH-001 | 120-220 LOC docs | high | T-P3 governance (Pass Omega CRUD) | 280 LOC | totality HANDOFF + SK-V14 contract consumers | `restart/HANDOFF.md:12`; `restart/skinny/tranches/sk-v14/SYNTHESIS.md:54-65` |
| COH-002 | 20-40 LOC docs | medium | T-P3 governance | 60 LOC | grammar onboarding docs | `restart/HANDOFF.md:17`; `restart/locks/LOCKS.md:220`; `restart/ARCHITECTURE.md:37` |
| COH-003 | 60-120 LOC docs | high | T-P3 governance | 160 LOC | H.W wave manifest reconciler | `restart/MASTER-PLAN.md:512-575`; `restart/skinny/tranches/sk-v14/SYNTHESIS.md:88-103` |
| COH-004 | 150-300 LOC code+tests | high | pass-hardening wave (SK-V14 PRUNE-3 sub-wave) | 400 LOC | codegen BackendShape selection tests | `restart/ARCHITECTURE.md:1090-1098`; `skinny/crates/passes/src/lib.rs:1497-1503`; `skinny/crates/codegen/src/grammar_profile.rs:11-25` |
| COH-005 | 300-600 LOC code+tests | high | Lock 14 wave (PRUNE-3) | 800 LOC | CSS/Sheets/BBNF-self recognizer fixtures | `restart/ARCHITECTURE.md:1129-1131`; `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` axis A3 |
| COH-006 | 80-160 LOC docs | high | T-P3 governance | 220 LOC | INDEX consumer page | `restart/skinny/INDEX.md:5-17`; `restart/skinny/tranches/sk-v14/SYNTHESIS.md:1-50` |
| COH-007 | 40-80 LOC docs | high | T-P3 governance | 120 LOC | HANDOFF/MASTER/INDEX/LOCKS audit-zero anchors | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:54-65` |
| COH-008 | 60-120 LOC docs | high | T-P3 governance + Lock 14 lint wave | 180 LOC | Lock 14 lint consumer + LOCKS amendment | `audit-overfit/sk-v14-audit-overfit-lock14-scan.md`; `restart/locks/LOCKS.md:220` |
| COH-009 | 100-200 LOC docs | high | T-P3 governance | 260 LOC | S-P3 wave manifest | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:88-103` |
| COH-010 | 80-160 LOC docs | high | T-P3 governance + LOCKS amendment | 220 LOC | pattern pre-block ledger consumer | `restart/skinny/tranches/sk-v14/SYNTHESIS.md:106-149` |
| COH-011 | 0 LOC census; 600-1200 LOC PRUNE-4 | medium-high | PRUNE-4 (9 sub-waves) | 1400 LOC for code | per-grammar generator template consumer | live `find` + `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:194-211` |
| COH-012 | 30-60 LOC docs | medium | T-P3 governance | 80 LOC | PASS-1-EXCAVATION §3 amendment; CH7 author | `restart/prompts/totality/PASS-1-EXCAVATION.md:91-138`; `restart/locks/LOCKS.md:46` |

## Gaps / Missing Primitives

| Gap | Evidence | LOC / risk |
|---|---|---|
| No single current-state authority surface spans SK-V13 close → SK-V14 audit overlay → SK-V14 R-targets. | HANDOFF SK-V13 head at `restart/HANDOFF.md:12`; SK-V14 SYNTHESIS active contract at `restart/skinny/tranches/sk-v14/SYNTHESIS.md:1`. | 120-220 LOC; high |
| Lock 14 verification commands at `restart/locks/LOCKS.md:220` do not include the SK-V14 generic-codegen-provider census command (audit-overfit/sk-v14-audit-overfit-lock14-scan.md axis A3). | Verification command list at `restart/locks/LOCKS.md:220` ends at three `rg/find/rg` commands; the per-grammar `RuntimeProvider` variant scan is missing. | 30-60 LOC; medium |
| MASTER-PLAN MP.NW-receiver enumeration has no PRUNE-prelude row. | `restart/MASTER-PLAN.md:564-575`. | 100-200 LOC; high |
| PASS-1-EXCAVATION CH-lens registry omits CH7 Overfit-Prune. | `restart/prompts/totality/PASS-1-EXCAVATION.md:91-138` lists CH1..CH6 only; LOCKS §0 line `46` cites CH7. | 30-60 LOC; medium |

## Open Questions

| UNKNOWN | Blocking question | verify_action |
|---|---|---|
| U-COH-007 | Which surface is authoritative for the audit-zero baseline restatement: HANDOFF, MASTER, or INDEX? | T-P3 disposes; Pass Omega CRUD adds the audit-zero anchor to all three; 1E proposes LOCKS-amendment candidate L-AUDIT-ZERO. |
| U-COH-011 | Does the nine-grammar `crates/core/src/runtime/` census require a Lock 14 amendment to admit `css_pretty` and acknowledge the 9-sub-wave PRUNE-4 partitioning, or is the existing Lock 14 prohibition sufficient? | 1E proposes amendment candidate; T-P3 disposes; PRUNE-4 implementation order independent of amendment. |
| U-COH-012 | Should CH7 register in `PASS-1-EXCAVATION.md §3` (per LOCKS:46) or only in the SK-V14 totality-pass overlay? | T-P3 disposes; the SK-V14 dispatch context already cites "CHALLENGE V1 (CH1-CH7 + aggregator)" — implementation lags only the lens prose. |
