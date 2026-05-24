# Ω-C Locks Amendments — Pass Omega V2 (T-P3 V4-LOCKED apply)

Pass: Pass Omega V2. Source: T-P3 V4 LOCK at commit `69eea1c5c`.
Date: 2026-05-24. G-Omega: CLOSED by user sign-off (this orchestration turn).
Authority: `restart/audit/totality/p3/3C-locks-crystallisation.md` +
`restart/audit/totality/p3/3C-locks-v+1-diff.md`.

## Application Summary

- 21 hunks total: 12 V3-merged (already present at HEAD `e12c5323d`; no v+1 delta)
  + 9 V4-NEW (newly applied this cycle).
- 51 candidates dispositioned: 38 ACCEPT + 13 MODIFY + 0 REJECT + 0 DEFER.
- 16-lock count: **PRESERVED**
  (`grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md` → 16).
- LAC-1E-12 in PREFACE (NOT Lock 17): **VERIFIED**
  (`restart/locks/LOCKS.md:44` `## CH7 Overfit-Prune lens binding`;
  `:60` `clause, NOT Lock 17 — preserves the 16-lock count`).
- LAC-2F-V5-02 elevated at Lock 1 substrate-union: **VERIFIED**
  (`restart/locks/LOCKS.md:137` `2026-05-23 v+1 substrate-union ELEVATION
  (LAC-2F-V5-02; T-P3 §3C amendment surface)`).
- LAC-1E-14 FactStream 5th-SUBSTRATE at Lock 1 manifest: **VERIFIED**
  (`restart/locks/LOCKS.md:100` `v+1 FactStream 5th substrate category
  (LAC-1E-14)`; `:107` `NOT a 6th BackendShape variant`; `:108` 5-shape canon
  at Lock 10 preserved).

## Per-hunk apply log

| Hunk # | Type | Target | Lock | Status | LOCKS.md line(s) |
|---|---|---|---|---|---|
| V3-1 | V3-merged | Supersede SK-V9 allowance | preface | already-merged | LOCKS.md:1-13 |
| V3-2 | V3-merged | Lock 1 substrate-ceiling history | Lock 1 | already-merged | LOCKS.md:77-135 |
| V3-3 | V3-merged | Lock 2 live-first wording | Lock 2 | already-merged | LOCKS.md:139-147 |
| V3-4 | V3-merged | Lock 3 empty-path verification | Lock 3 | already-merged | LOCKS.md:151-158 |
| V3-5 | V3-merged | Lock 8 row-plane + BENCH §8 | Lock 8 | already-merged | LOCKS.md:204-269 (range now extended by V4-5) |
| V3-6 | V3-merged | Lock 9 runtime API obligations | Lock 9 | already-merged | LOCKS.md:272-277 |
| V3-7 | V3-merged | Lock 10 decision-engine cost | Lock 10 | already-merged | LOCKS.md:281-293 |
| V3-8 | V3-merged | Locks 11+12 workspace drift | Locks 11/12 | already-merged | LOCKS.md:319-332 |
| V3-9 | V3-merged | Lock 13 generated/report exceptions | Lock 13 | already-merged | LOCKS.md:336-345 |
| V3-10 | V3-merged | Lock 14 generated-output + per-wave gate | Lock 14 | already-merged | LOCKS.md:349-400 (extended by V4-4-B + V4-7) |
| V3-11 | V3-merged | Lock 15 profile scope | Lock 15 | already-merged | LOCKS.md:443-451 |
| V3-12 | V3-merged | Lock 16 manifest + checkasm + orphans | Lock 16 | already-merged | LOCKS.md:480-553 (extended by V4-8 + V4-9) |
| V3-13 | V3-merged | G-Omega Governance Boundary footer | footer | already-merged | LOCKS.md:581-590 |
| V4-1 | V4-NEW | Preface CH7 Overfit-Prune binding | (preface) | applied | LOCKS.md:44-69 |
| V4-2 | V4-NEW | Lock 1 substrate-union ELEVATION | Lock 1 | applied | LOCKS.md:137-158 |
| V4-3 | V4-NEW | Lock 1 FactStream 5th substrate category | Lock 1 | applied | LOCKS.md:100-116 |
| V4-4-A | V4-NEW | Lock 6 regen round-trip discipline | Lock 6 | applied | LOCKS.md:185-198 |
| V4-4-B | V4-NEW | Lock 14 generated-output bound to V4-4-A | Lock 14 | applied | LOCKS.md:360-366 |
| V4-5 | V4-NEW | Lock 8 audit-overlay 4-column + numeric abrogate gates | Lock 8 | applied | LOCKS.md:213-233 |
| V4-6 | V4-NEW | Lock 10 cohort-wide BBNF_SIMD_STRICT + regex/HIR mandate | Lock 10 | applied | LOCKS.md:295-316 |
| V4-7 | V4-NEW | Lock 14 Pattern H census + byte_class_from_range_64 sibling | Lock 14 | applied | LOCKS.md:402-435 |
| V4-8 | V4-NEW | Lock 16 CollapsedStage x86-only + BackendExpr.substrate_target | Lock 16 | applied | LOCKS.md:520-538 |
| V4-9 | V4-NEW | Lock 16 bbnf-regex::Dfa admissibility + CH3 pre-flight reflex | Lock 16 | applied | LOCKS.md:555-578 |

## Post-apply state

- `wc -l restart/locks/LOCKS.md`: **564 → 779** (+215 lines for 9 V4 hunks).
- `grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md`: **16** (lock-numbered
  headings preserved; `Lock 17` does not appear).
- `grep -nE "^## (CH7|Gestalt|v\+1 Governance)" restart/locks/LOCKS.md`:
    - `:44 ## CH7 Overfit-Prune lens binding` (V4-1 carrier paragraph).
    - `:71 ## Gestalt — sixteen locks` (lock-count anchor preserved).
    - `:581 ## v+1 Governance Boundary` (governance footer preserved).
- All cross-references from `ARCHITECTURE.md` / `MASTER-PLAN.md` /
  `HANDOFF.md` / `BENCH.md` / `MIGRATION.md` / `PASS-0-OVERFIT-AUDIT.md`
  still resolve at `restart/locks/LOCKS.md` line-coordinates; per-document
  back-references defer to CRUD-6 reconciliation per Pass Omega V2 §4.

## Invariant cross-checks (G-Omega closure surface)

| invariant | source | result |
|---|---|---|
| 16-lock count preserved | PASS-3 §8.1, V4 diff §V4-1 | PASS (`grep` = 16) |
| LAC-1E-12 in preface, NOT Lock 17 | T-P1 V5 §6.1, V4 hunk 3C-PREFACE-ch7-binding | PASS (`:44` preface heading; `:60` explicit `NOT Lock 17`) |
| LAC-2F-V5-02 ELEVATED at Lock 1 substrate-union | HARDENING-T-P2-V3-CONSOLIDATED §4 row 4 | PASS (`:137` LOC anchor + STRONGEST AMENDMENT SURFACE callout `:158`) |
| LAC-1E-14 FactStream as 5th SUBSTRATE | 1C-runtime-evidence:102 (1C-D5) | PASS (`:100` substrate-manifest classification; `:107` explicit `NOT 6th BackendShape variant`) |
| 5-shape `BackendShape` canon at Lock 10 holds | V4 diff §V4-3 carrier note | PASS (Lock 10 body unchanged at `:230`; 5-shape enumeration `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` reaffirmed at LOCKS.md:108) |
| REDRESS 96/97/98 generalised to ALL transient classifier-state primitives | LAC-2F-V5-02 hunk text | PASS (`:144` `closure of REDRESS 96 / 97 / 98 ... generalises to ALL transient classifier-state primitives`) |
| `admits_collapsed_stage` predicate co-requires `target.arch == x86` | LAC-2D-06 hunk text | PASS (`:521` `MUST co-require target.arch == x86`) |
| `BackendExpr.substrate_target` declaration bound | LAC-2D-06 hunk text | PASS (`:528` ∈ four admitted values) |
| `cargo xtask regen-{grammar}` round-trip clean check folded into Lock 6 + Lock 14 | LAC-1E-13 hunk text | PASS (Lock 6 anchor `:185`; Lock 14 bound clause `:360`) |
| 4 audit-overlay columns required + `xtask gate-json` REJECTS missing rows | LAC-1E-16 hunk text | PASS (`:213` 4-column binding; `:217` xtask gate-json reject) |
| 6 abrogate-gate numerics numerically bound | T2A-LAC-V1-05 numeric-bind | PASS (`:225` ≤50000/≤10000/≤30/≤1s/≤30%/...) |
| Cohort-wide `BBNF_SIMD_STRICT=1` precondition at 2A:192 + 2C:303-305 + 2D:142-149 | F-V3-CH4-A discharge | PASS (`:295` cohort precondition + tri-site cross-refs) |
| Regex/HIR facts MANDATORY; opaque pattern strings insufficient | LAC-2F-V5-04 hunk text | PASS (`:307` mandate + `:309` SinkOnlyExpr::RegexProgram counter-example) |
| Pattern H per-tranche census with `-mindepth 2` (no `-maxdepth 2`) | LAC-1E-15 hunk text | PASS (`:403` find command + `:406` MUST omit `-maxdepth 2`) |
| `byte_class_from_range_64` sibling of `byte_class_from_eq_set_64` | LAC-2F-V5-03 hunk text | PASS (`:426` pinned as sibling primitive) |
| `bbnf-regex::Dfa` admissibility row + CH3 pre-flight reflex with REDRESS pre-block | LAC-2F-V5-01 + F-CH3-2F-08 | PASS (`:555` admissibility row + `:563` pre-flight reflex citing REDRESS 96/97/98) |

## Source provenance

- T-P3 V4 LOCK commit: `69eea1c5c` (per task brief authority §4).
- LOCKS.md prior-state commit: `e12c5323d` (Pass Omega CRUD-3 LOCKS amendment;
  V3 baseline at HEAD).
- 3C-locks-crystallisation.md and 3C-locks-v+1-diff.md consumed verbatim;
  no V4 hunk text was paraphrased or condensed in transit to LOCKS.md.

## Carry-forward to CRUD-6

The propagation surfaces enumerated in 3C-crystallisation §Consequences
(ARCHITECTURE.md cost/fact schema + 5th substrate category +
`BackendExpr.substrate_target` enum; MASTER-PLAN.md wave gates including
regen-roundtrip family + Pattern H census; BENCH.md 4 NEW gate-consumed
columns; HANDOFF.md G-Omega + CH7 binding announcement; MIGRATION.md
Pattern H 67-file consolidation; PASS-0-OVERFIT-AUDIT.md cross-reference
into CH7 binding clause carrier) defer to subsequent Pass Omega V2 CRUD
turns. This Ω-C log records the LOCKS.md surface edit only.
