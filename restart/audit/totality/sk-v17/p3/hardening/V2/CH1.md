---
lens: CH1 CORRECTNESS
pass: T-P3-synthesis
cycle: V2
subject: SK-V17 T-P3 synthesis artefacts
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac1959ef027df4d28e09be2b0b0bbec7f
artefacts_reviewed:
  - restart/audit/totality/sk-v17/p3/3c-locks-v+1-diff.md
  - restart/audit/totality/sk-v17/p3/3c-locks-crystallisation.md
  - restart/audit/totality/sk-v17/p3/3a-architecture-synthesis.md
verdict: ACCEPT
counts:
  accept: 19
  revise: 0
  reject: 0
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: [CH1-SKV17-01]
---

# CH1 CORRECTNESS — SK-V17 T-P3 V2

## Mandate

CH1 scans, per PASS-3-SYNTHESIS §3 / ORCHESTRATOR §3W:
1. every proposed delta cites a real T-P2 LAC / T-P1 divergence;
2. every cited V1-surface section resolves at `file:line`;
3. the 3C disposition matrix references real amendment candidates;
4. `3c-locks-v+1-diff.md` applies cleanly to the current `LOCKS.md`
   (`git apply --check`).

READ-ONLY against V1 surfaces. T-P3 PROPOSES; Pass Omega CRUD applies
post-G-Omega. Master HEAD confirmed `git rev-parse HEAD` =
`2a76916ac1959ef027df4d28e09be2b0b0bbec7f`.

## Executive verdict — ACCEPT (the one V1 load-bearing defect is folded)

The single V1 REVISE — `CH1-SKV17-01`, the `3c-locks-v+1-diff.md` hunk-header
arithmetic error (`@@ -606,6 +606,52 @@` → corrupt patch at line 27, EXIT 128)
— is **fully folded** in V2. The current diff header at
`3c-locks-v+1-diff.md:49` reads exactly the prescribed fix `@@ -606,7 +606,22 @@`;
`git apply --check` returns **EXIT 0 (clean)**; the body arithmetic
(7 old-side context, 22 new-side context+insert) matches the header exactly.
The gate object now applies.

The citation base is **sound across the board**, re-verified live at HEAD:
every one of the 5 LOCKS deltas and 8 ARCHITECTURE deltas cites a real T-P2
LAC / T-P1 divergence; every cited V1-surface section resolves at `file:line`;
the 14-candidate disposition matrix references only real amendment candidates
with zero silent drops; the tally (9 ACCEPT + 3 ORQ-ACCEPT + 2 MODIFY + 0
REJECT + 0 DEFER = 14) is internally consistent and matches the diff narrative.

## Folded finding — CH1-SKV17-01 (REVISE → resolved)

**V1 defect.** `3c-locks-v+1-diff.md:49` declared `@@ -606,6 +606,52 @@`; the
actual hunk body carries 7 old-side context lines and 22 new-side lines, so
`git apply` ran off the declared 52nd new line and aborted `corrupt patch at
line 27`. The anchor and content were correct; only the header arithmetic was
wrong. Prescribed fix: `@@ -606,7 +606,22 @@`.

**V2 resolution — verified.**

```
$ sed -n '49p' restart/audit/totality/sk-v17/p3/3c-locks-v+1-diff.md
@@ -606,7 +606,22 @@                                   ← prescribed fix applied

$ git apply --check <extracted-diff>
EXIT 0                                                 ← clean

header arithmetic: old-side context+del = 7, new-side context+add = 22
                                                       ← matches @@ -606,7 +606,22 @@
```

The insertion anchors at `restart/locks/LOCKS.md:608`-`609` (the two blank
lines after the SK-V15 addendum's Lock-16 clause at `:607`, before
`## v+1 Governance Boundary` at `:610`). Context lines 606-612 of the diff's
`-` side match `LOCKS.md:606`-`612` verbatim. Disposition: **resolved.**

## ACCEPT rows — 3C diff + crystallisation (the G3 gate object)

### Source-finding existence (every cited LAC / divergence is real)

| candidate cited | source doc:line | resolves |
|---|---|---|
| LAC-2F-FOLD-01..05 | `2f-fold-gaps.md:580`-`584` | ✓ five rows present |
| 2F-FOLD-U1/U2/U3 (ORQs) | `2f-fold-gaps.md:563`-`565` | ✓ three ORQ rows present |
| LAC-1E-SKV17-01..06 | `1e-locks-evidence.md:178`-`183` | ✓ six rows present |
| D-1E-SKV17-01..06 (divergences) | `1e-locks-evidence.md:126`-`131` | ✓ six divergence rows present |
| T-P2 §3Z lock provenance (V2=98.6% + V3=100.0%) | `HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:15`-`19` | ✓ resolves |

### LOCKS.md V1-surface anchors (every cited section resolves)

| anchor | content at line | resolves |
|---|---|---|
| `LOCKS.md:75` | Lock 1 "parallel substrates dead / one encoding" | ✓ |
| `LOCKS.md:100`-`116` | LAC-1E-14 FactStream 5th-substrate-category precedent | ✓ |
| `LOCKS.md:107`-`108` | 5-shape canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` | ✓ |
| `LOCKS.md:109` | 6th `BackendShape` G-Omega gated | ✓ |
| `LOCKS.md:118`-`127` | substrate_target manifest | ✓ |
| `LOCKS.md:137`-`149` | Lock-1 v+1 no-cross-call classifier-state retention | ✓ |
| `LOCKS.md:160`,`:162`-`166` | Lock 2 canonical `Layout`; v+1 live-state note | ✓ |
| `LOCKS.md:349` | Lock 14 grammar-generalisation | ✓ |
| `LOCKS.md:453`,`:520`-`533` | Lock 16 manifest; `admits_collapsed_stage` x86-binding | ✓ |
| `LOCKS.md:581`,`:610` | SK-V15 addendum heading; `## v+1 Governance Boundary` | ✓ |
| 16 numbered-lock lines `75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453` | all 16 resolve to lock headings; count preserved | ✓ |

### ARCHITECTURE.md / SPEC.md / source-tree anchors (load-bearing subset)

| anchor | content | resolves |
|---|---|---|
| `ARCHITECTURE.md:1088` | `LayoutFacts.backend_shape` substrate-manifest prose | ✓ |
| `ARCHITECTURE.md:1090` | `BackendShape` enum block | ✓ |
| `ARCHITECTURE.md:1151`,`:1282` | `admits_collapsed_stage` x86-bound (LAC-2D-06) | ✓ |
| `ARCHITECTURE.md:1206`,`:1279`-`1280` | CollapsedStage row / UNKNOWN-2D-05 | ✓ |
| `ARCHITECTURE.md:1840`,`:1861`,`:1863` | §9.1 Tape / ValueRef identity / §9.2 Direct-To-Struct Union | ✓ |
| `SPEC.md:252` | preserve-rich-ast | ✓ |
| `SPEC.md:258`/`:806`/`:808` | aarch64-only / no-SVE / §9 sixth-shape bar | ✓ |
| `SPEC.md:314`-`317` | alphabet-as-data L1 classifier | ✓ |
| `SPEC.md:577`/`:793`-`795`/`:825`/`:839` | REDRESS-53 / 28-65×/983×/10583× registry regression | ✓ |
| `core/.../tape/record.rs:103` | `struct TapeRec` `#[repr(C, align(4))]` AoS | ✓ |
| `skinny/.../tape/mod.rs:94`,`:175` | `struct Tape<'input>` SoA / `ValueRef<'doc,'input,K,G:EventGrammar>` | ✓ |
| `core/.../tape/mod.rs:185`-`186` | `begin_compound` reads `layout.rule_id & 0x1F`; grep-zero `StructRegistry` (verified =0) | ✓ |
| `core/.../bbnf/arena.rs:47` | `match StructRegistry::compound_kind_for_layout(layout)` coupling | ✓ |
| `core/.../bbnf/builder.rs:102` | `BbnfCompoundKind::from_layout(layout)` caller | ✓ |
| `core/.../bnf/kind.rs:20` | local `match layout.rule_id` (not registry) | ✓ |
| `core/.../css_l4/builder.rs:16` (817 LOC) / `json/builder.rs:9` | `enum OpenFrame<'p>` eager builders (LOC=817 verified) | ✓ |
| `core/.../css_l4/value.rs:414` | `enum CssTypedValue<'p>` eager per-grammar value enum | ✓ |
| generated OnceCell sites `json.rs:732`, `css_l4.rs:15982`, `google_sheets.rs:3559`, `bbnf.rs:4843` | `::simd_scan::scan_structural(input, &alphabet)` | ✓ |
| `ir/.../registry/struct.rs:84`,`:313` | `enum FieldSource` / `struct StructRegistry` | ✓ |
| `simd-scan/src/lib.rs:80` / `bbnf-simd/.../dispatch.rs:42` | `scan_structural(...&StructuralAlphabet)` / `select_classifier(&[u8;64])` | ✓ |
| `grep StructLayout crates/` = 960 (verified) / `grep 'backend_shape\|LayoutFacts' crates/` = 0 (verified) | Lock-2 path-(a) rename surface vs path-(b) grep-zero side-table | ✓ |

### Disposition-matrix integrity

- 14 candidates each carry exactly one disposition; tally 9 ACCEPT + 3
  ORQ-ACCEPT + 2 MODIFY + 0 REJECT + 0 DEFER = 14 (`3c-locks-crystallisation.md:142`-`148`).
  Diff narrative claims the same (`3c-locks-v+1-diff.md:39`). Consistent.
- Zero silent drops: every LAC/ORQ/divergence in the source docs appears as a
  matrix row with a `folds into` D-clause. ✓
- The two MODIFYs (LAC-2F-FOLD-05, LAC-1E-SKV17-04) correctly decline to pick
  path-(a)-vs-(b) inside the lock — a route choice, not a lock edit. The
  grep-zero `LayoutFacts crates/`=0 fact backing path-(b)'s non-zero sizing is
  live-verified. ✓
- The three ORQs are crystallised, not engineered-defers: each names a receiver
  + blocker + receiving gate (`3c-locks-crystallisation.md:136`-`138`). U3's
  receiver is the EXISTING 5-shape gate + the G-Omega 6th-shape path — no
  phantom future wave named. ✓ (CH6's scope, noted clean.)

### 3A — eight ARCHITECTURE deltas

D01-D08 each cite a real LAC + T-P1 divergence + resolving ARCH/§-anchor
(`3a-architecture-synthesis.md:62`-`69`). Spot-verified: D01 (§7.3/§9.1, LAC-2F-FOLD-01),
D02 (§9.2, LAC-2F-FOLD-03-home, `value.rs:414`/`tape/mod.rs:175`), D04 (§7.3/§9
BackendShape-category, LAC-2F-FOLD-02), D05 (§7.3 fence with verified
`arena.rs:47`→`builder.rs:102` caller chain, LAC-2F-FOLD-04), D08 (three-ORQ
note). All resolve.

## Invariant checks (CH1-adjacent, confirming)

- **16-lock count preserved**: the addendum adds no numbered lock, retires none,
  renumbers none; all 16 lock headings resolve at the cited lines. ✓ (§8.1)
- **5-shape canon verbatim**: the addendum restates `{EagerTape, OffsetTape,
  EventTape, SinkOnly, CollapsedStage}` in the heading + Lock 10 clause; no
  sixth variant; a sixth stays G-Omega gated. ✓ (§8.2)
- **No new directive / BIR / substrate / public substrate API / retained
  sidecar**: tape recorded as substrate-manifest CATEGORY (`substrate_target =
  existing_tape`); NEON classifier `retention_lifetime = transient-single-call`;
  OnceCell carriers declare `existing_tape`/`local_temp_only`. ✓ (§8.5)
- **T-P3 proposes only**: addendum sits above the in-force `## v+1 Governance
  Boundary`; applied by Pass Omega CRUD post-G-Omega. ✓ (§8.6)

## Open Questions

| lens | question | receiver | gate |
|---|---|---|---|
| CH1 | None load-bearing. The one V1 REVISE is folded; the gate object applies clean. The Pass-Omega placement question (one addendum section vs distribute per-lock) is a governance-style choice carried in `3c-locks-crystallisation.md:216`, not a citation defect. | Pass Omega CRUD owner | clean `git apply --check` (now EXIT 0) + CH1 path-resolution (now all ✓). |

## Disposition summary

| item | disposition |
|---|---|
| `3c-locks-v+1-diff.md` applies clean (`git apply --check` EXIT 0) | **ACCEPT** |
| Hunk header `@@ -606,7 +606,22 @@` matches body arithmetic (7/22) | **ACCEPT** |
| 5 LOCKS deltas cite real LACs + resolving sections | **ACCEPT** (×5) |
| 8 ARCHITECTURE deltas cite real LACs + resolving sections | **ACCEPT** (×8) |
| 14-candidate disposition matrix: real candidates, zero silent drops, consistent tally | **ACCEPT** |
| 16-lock count + 5-shape canon preserved verbatim | **ACCEPT** |
| CH1-SKV17-01 (V1 header arithmetic) | **REVISE → folded/resolved** |

**Counts: 19 ACCEPT, 0 REVISE, 0 REJECT.** No load-bearing CH1 defect
remains. The G3 gate object applies cleanly at HEAD `2a76916ac`.
