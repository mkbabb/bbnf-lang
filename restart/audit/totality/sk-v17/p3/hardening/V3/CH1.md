---
lens: CH1 CORRECTNESS
pass: T-P3-synthesis
cycle: V3
subject: SK-V17 T-P3 synthesis artefacts
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac1959ef027df4d28e09be2b0b0bbec7f
artefacts_reviewed:
  - restart/audit/totality/sk-v17/p3/3c-locks-v+1-diff.md
  - restart/audit/totality/sk-v17/p3/3c-locks-crystallisation.md
  - restart/audit/totality/sk-v17/p3/3a-architecture-synthesis.md
  - restart/audit/totality/sk-v17/p3/3b-master-plan-reconciliation.md
  - restart/audit/totality/sk-v17/p3/3e-grammar-generalisation.md
verdict: ACCEPT
counts:
  accept: 21
  revise: 0
  reject: 0
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
---

# CH1 CORRECTNESS — SK-V17 T-P3 V3

## Mandate

CH1 scans, per PASS-3-SYNTHESIS §3 / ORCHESTRATOR §3W:
1. every proposed delta cites a real T-P2 LAC / T-P1 divergence;
2. every cited V1-surface section resolves at `file:line`;
3. the 3C disposition matrix references real amendment candidates (zero
   silent drops);
4. `3c-locks-v+1-diff.md` applies cleanly to the current `LOCKS.md`
   (`git apply --check`).

READ-ONLY against V1 surfaces. T-P3 PROPOSES; Pass Omega CRUD applies
post-G-Omega. Master HEAD confirmed `git rev-parse HEAD` =
`2a76916ac1959ef027df4d28e09be2b0b0bbec7f`.

## Executive verdict — ACCEPT (zero load-bearing CH1 defect; all V2 REVISEs folded)

V3 carries forward the V2-clean citation base and gate object. The single
V2-cycle CH-class REVISE that touched the gate object — **CH5-V2-R01**
(the R03 distribution invariant present in `3c-locks-crystallisation.md`
but ABSENT from the `3c-locks-v+1-diff.md` Invariant Check) — is **fully
folded**: the distribution-invariant bullet now rides the gate object at
`3c-locks-v+1-diff.md:103`, verbatim per the V2 fix. No CH1-owned REVISE
was open entering V3.

Re-verified live at HEAD `2a76916ac`:
- **`git apply --check` EXIT 0** — the gate object applies cleanly to the
  current `restart/locks/LOCKS.md`.
- Every one of the **5 LOCKS deltas** and **8 ARCHITECTURE deltas** cites a
  real T-P2 LAC / T-P1 divergence that resolves in the source doc.
- Every cited V1-surface section (LOCKS.md, ARCHITECTURE.md, MASTER-PLAN.md,
  SK-V17 SPEC.md) and every source-tree anchor resolves at `file:line`.
- The **14-candidate disposition matrix** references only real amendment
  candidates with zero silent drops; the tally (9 ACCEPT + 3 ORQ-ACCEPT +
  2 MODIFY + 0 REJECT + 0 DEFER = 14) is internally consistent and matches
  the diff narrative (`3c-locks-v+1-diff.md:39`).
- **16-lock count + 5-shape canon** preserved verbatim.

## (1) git apply --check — EXIT 0 (the G3 gate object applies)

Extracted the unified diff from `3c-locks-v+1-diff.md` (the fenced ```diff
block) and ran `git apply --check` at HEAD:

```
$ awk '/^```diff$/{f=1;next} /^```$/{if(f)exit} f' 3c-locks-v+1-diff.md > /tmp/v3.patch
$ git apply --check /tmp/v3.patch ; echo EXIT=$?
EXIT=0
```

Hunk header `@@ -606,7 +606,22 @@` (`3c-locks-v+1-diff.md:63`) — arithmetic
correct: 7 old-side context lines, 22 new-side context+insert. Anchor
verified against the live file:

| diff context | LOCKS.md line | content |
|---|---|---|
| Lock 16 primitive-manifest clause | `:606` | ✓ matches verbatim |
| blank | `:607`-`608` | ✓ insertion point |
| `## v+1 Governance Boundary` | `:610` | ✓ post-context |

`LOCKS.md` is 808 lines; the SK-V15 addendum heading resolves at `:581`,
the governance boundary at `:610`. The addendum inserts above the in-force
governance boundary — correct T-P3 proposal placement (§8.6).

## (2) V1-surface section resolution — all ✓ at file:line

### LOCKS.md anchors (every cited section resolves)

| anchor | content at line | resolves |
|---|---|---|
| `LOCKS.md:75` | Lock 1 "Tape is the substrate … columnar SoA" one-encoding | ✓ |
| `LOCKS.md:100`-`116` | LAC-1E-14 FactStream 5th-substrate-category precedent | ✓ |
| `LOCKS.md:107`-`108` | 5-shape canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` | ✓ |
| `LOCKS.md:109` | 6th `BackendShape` G-Omega gated | ✓ |
| `LOCKS.md:118`-`127` | substrate manifest (e-graph/backend/scanner-plan rows) | ✓ |
| `LOCKS.md:137`-`149` | Lock-1 v+1 ELEVATION; no cross-call classifier-state retention | ✓ |
| `LOCKS.md:160`,`:162`-`166` | Lock 2 canonical `Layout`; v+1 live-state `LayoutFacts.backend_shape` note | ✓ |
| `LOCKS.md:349` | Lock 14 grammar-generalisation / zero overfitting | ✓ |
| `LOCKS.md:453`,`:520`-`533` | Lock 16 allowlist; LAC-2D-06 `admits_collapsed_stage` aarch64-refusal | ✓ |
| `LOCKS.md:581`,`:610` | SK-V15 addendum heading; `## v+1 Governance Boundary` | ✓ |
| 16 numbered-lock lines `75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453` | all 16 resolve to lock headings; count preserved | ✓ |

### ARCHITECTURE.md anchors (3A deltas + 3C cross-refs)

| anchor | content | resolves |
|---|---|---|
| `ARCHITECTURE.md:1083`,`:1088` | `PrimitiveFacts` / `LayoutFacts.backend_shape` substrate-manifest prose | ✓ |
| `ARCHITECTURE.md:1090` | `BackendShape` enum block (```rust fence) | ✓ |
| `ARCHITECTURE.md:1118`,`:1128` | cost-model derivation pipeline / `BackendExpr` | ✓ |
| `ARCHITECTURE.md:1151`,`:1282` | `admits_collapsed_stage` x86-bound (LAC-2D-06) | ✓ |
| `ARCHITECTURE.md:1206`,`:1279`-`1280` | CollapsedStage row / UNKNOWN-2D-05 aarch64-refusal | ✓ |
| `ARCHITECTURE.md:1286` | §7.4 SK-V5..SK-V15 implementation status | ✓ |
| `ARCHITECTURE.md:1840`,`:1861`,`:1863`,`:1877` | §9.1 Tape / ValueRef identity / §9.2 Direct-To-Struct Union | ✓ |

### MASTER-PLAN.md + SK-V17 SPEC.md anchors

| anchor | content | resolves |
|---|---|---|
| `MASTER-PLAN.md:567` | §13 Tranche H — Pratt, SIMD, typed-event codegen | ✓ |
| `MASTER-PLAN.md:669` | §13.1 admissible SIMD primitives (Lock 16 allowlist) | ✓ |
| `MASTER-PLAN.md:192`,`:248`,`:579`,`:620` | Tranche B substrate / H.W1/H.W2 rows (3B spot-checks) | ✓ |
| `SPEC.md:252` | preserve-rich-ast | ✓ |
| `SPEC.md:314`-`317` | alphabet-as-data L1 classifier (`alphabet: &[u8;64]`) | ✓ |
| `SPEC.md:577`/`:793`-`795`/`:825`/`:839` | REDRESS-53 / 28-65×/983×/10583× registry regression | ✓ |
| `SPEC.md:806`/`:808`/`:854` | aarch64-only no-SVE / §9 sixth-shape bar / no-D6 second substrate | ✓ |

### Source-tree anchors (load-bearing subset, all live at HEAD)

| anchor | content | resolves |
|---|---|---|
| `crates/core/.../tape/record.rs:103` | `pub struct TapeRec` AoS | ✓ |
| `skinny/.../runtime/src/tape/mod.rs:94`,`:175` | `pub struct Tape<'input>` SoA / `pub struct ValueRef<'doc,'input:'doc,K,G:EventGrammar=…>` | ✓ |
| `crates/core/.../tape/mod.rs:185`-`186` | `begin_compound` reads `layout.rule_id & 0x1F`; grep `StructRegistry` in file = **0** (verified) | ✓ |
| `crates/core/.../bbnf/arena.rs:47` | `match StructRegistry::compound_kind_for_layout(layout)` coupling | ✓ |
| `crates/core/.../css_l4/builder.rs:16` | `enum OpenFrame<'p>`; LOC = **817** (verified `wc -l`) | ✓ |
| `crates/core/.../json/builder.rs:9` | `enum OpenFrame<'p>` eager builder | ✓ |
| `crates/core/.../css_l4/value.rs:414` | `pub enum CssTypedValue<'p>` eager per-grammar value enum | ✓ |
| `crates/ir/.../registry/struct.rs:84`,`:313` | `pub enum FieldSource` / `pub struct StructRegistry` | ✓ |
| `crates/simd-scan/src/lib.rs:80` | `pub fn scan_structural(input, alphabet: &StructuralAlphabet)` | ✓ |
| `skinny/.../bbnf-simd/src/dispatch.rs:42` | `pub fn select_classifier(alphabet: &'static [u8;64])` | ✓ |
| `skinny/.../passes/src/lib.rs:96` | `pub backend_shape: HashMap<ir::RuleId, BackendShape>` (LayoutFacts skinny-only) | ✓ |
| generated OnceCell sites `json.rs:732`, `css_l4.rs:15982`, `google_sheets.rs:3559`, `bbnf.rs:4843` | `::simd_scan::scan_structural(input, &alphabet)` | ✓ |
| `skinny/.../bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs` | scalar reference oracle exists | ✓ |

### Grep-count claims (live-verified)

| claim in diff | live result | resolves |
|---|---|---|
| `grep StructLayout crates/`=960 (line count, default grep) | `grep -r StructLayout crates/ \| wc -l` = **960** | ✓ |
| `grep 'backend_shape\|LayoutFacts' crates/`=0 | `grep -rE … crates/ \| wc -l` = **0** | ✓ |
| `StructRegistry` in `tape/mod.rs` = 0 | `grep -c` = **0** | ✓ |

Note: the occurrence count (`grep -o`) is 1030, but the diff writes the
default-grep line form (`grep StructLayout crates/`) which yields 960
exactly. The citation is correct under its own stated grep form — not a
defect.

## (3) Source-finding existence — every cited LAC / divergence is real

| candidate cited | source doc:line | resolves |
|---|---|---|
| LAC-2F-FOLD-01..05 | `2f-fold-gaps.md:580`-`584` | ✓ five rows present |
| 2F-FOLD-U1/U2/U3 (ORQs) | `2f-fold-gaps.md:563`-`565` | ✓ three ORQ rows present |
| LAC-2F-FOLD-03 value/scan-axis scoping | `2f-fold-gaps.md:530`-`534` | ✓ resolves |
| 2F verdict (5-shape-coherent fold) | `2f-fold-gaps.md:616`-`623` | ✓ resolves |
| LAC-1E-SKV17-01..06 | `1e-locks-evidence.md:178`-`183` | ✓ six rows present |
| D-1E-SKV17-01..06 (divergences) | `1e-locks-evidence.md:126`-`131` | ✓ six divergence rows present |
| T-P1 clean-final / G1-auto-pinned provenance | `1e-locks-evidence.md:80`-`82` | ✓ resolves |
| T-P2 §3Z lock provenance (V2=98.6% + V3=100.0%, orphan_revise:0, CONVERGED) | `HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:15`-`19` | ✓ resolves |

## (4) Disposition-matrix integrity (zero silent drops)

- 14 candidates each carry exactly one disposition; tally **9 ACCEPT + 3
  ORQ-ACCEPT + 2 MODIFY + 0 REJECT + 0 DEFER = 14**
  (`3c-locks-crystallisation.md:152`-`158`); diff narrative claims the same
  (`3c-locks-v+1-diff.md:39`). Consistent.
- Zero silent drops: every LAC / ORQ / divergence in `2f-fold-gaps.md` +
  `1e-locks-evidence.md` appears as a matrix row with a `folds into`
  D-clause. ✓
- The two MODIFYs (LAC-2F-FOLD-05, LAC-1E-SKV17-04) correctly decline to
  pick path-(a)-960-rename vs path-(b)-side-table inside the lock — a route
  choice, not a lock edit. The grep-zero `backend_shape|LayoutFacts crates/`
  = 0 fact backing path-(b)'s non-zero sizing is live-verified. ✓
- The three ORQs are crystallised, not engineered-defers: each names a
  receiver + blocker + receiving gate (`3c-locks-crystallisation.md:146`-`148`).
  U3's receiver is the EXISTING 5-shape gate + the G-Omega 6th-shape path —
  no phantom future wave named. ✓ (CH6's primary scope; noted clean.)

## (5) 3A / 3B / 3E synthesis-artefact citation integrity

- **3A** — D01-D08 each cite a real LAC + T-P1 divergence + resolving
  ARCH/§-anchor (`3a-architecture-synthesis.md:62`-`69`). All cited ARCH
  lines (`:1083,:1088,:1090,:1118,:1128,:1282,:1286,:1863,:1877`) resolve
  within the 2338-line file. V2 ACCEPTed every 3A delta on every lens; no
  REVISE open at V3. ✓
- **3B** — MASTER-PLAN line citations spot-checked (`:192,:248,:579,:620`)
  resolve to real Tranche B / H.W1 / H.W2 rows; §13 SIMD at `:567`. ✓
- **3E** — correctly anchors Lock 14 grammar-generalisation at `LOCKS.md:349`
  (9 cites) and the `ValueRef<G>` plane at `skinny/.../tape/mod.rs:175`;
  `select_classifier` grammar-generality cited 4×. ✓

## Invariant checks (CH1-adjacent, confirming)

- **16-lock count preserved**: addendum adds no numbered lock, retires none,
  renumbers none; all 16 lock headings resolve at the cited lines. ✓ (§8.1)
- **5-shape canon verbatim**: addendum restates `{EagerTape, OffsetTape,
  EventTape, SinkOnly, CollapsedStage}` in heading + Lock 10 clause; no sixth
  variant; a sixth stays G-Omega gated (`LOCKS.md:109`). ✓ (§8.2)
- **No new directive / BIR / substrate / public substrate API / retained
  sidecar**: tape recorded as substrate-manifest CATEGORY
  (`substrate_target = existing_tape`); NEON classifier `retention_lifetime
  = transient-single-call`; OnceCell carriers declare `existing_tape` /
  `local_temp_only`. ✓ (§8.5)
- **Distribution invariant on the gate object** (V2 CH5-R01 fold): present
  at `3c-locks-v+1-diff.md:103` — the Lock-10 tape-category clause must
  retain its inline cross-ref to the Lock-1 substrate manifest under any
  Pass-Omega distribution. ✓
- **T-P3 proposes only**: addendum sits above the in-force `## v+1
  Governance Boundary`; applied by Pass Omega CRUD post-G-Omega. ✓ (§8.6)

## Open Questions

| lens | question | receiver | gate |
|---|---|---|---|
| CH1 | None load-bearing. The gate object applies clean (`git apply --check` EXIT 0); all delta citations and V1-surface anchors resolve at `file:line`; the V2 distribution-invariant REVISE is folded into the gate object. The Pass-Omega placement question (one addendum section vs distribute per-lock) is a governance-style choice (`3c-locks-crystallisation.md:226`), not a citation defect. | Pass Omega CRUD owner | clean `git apply --check` + CH1 path-resolution (all ✓). |

## Disposition summary

| item | disposition |
|---|---|
| `3c-locks-v+1-diff.md` applies clean (`git apply --check` EXIT 0) | **ACCEPT** |
| Hunk header `@@ -606,7 +606,22 @@` matches body arithmetic (7/22) | **ACCEPT** |
| 5 LOCKS deltas cite real LACs + resolving sections | **ACCEPT** (×5) |
| 8 ARCHITECTURE deltas cite real LACs + resolving ARCH §-anchors | **ACCEPT** (×8) |
| 14-candidate disposition matrix: real candidates, zero silent drops, consistent tally | **ACCEPT** |
| 16-lock count + 5-shape canon preserved verbatim | **ACCEPT** |
| Distribution-invariant present in gate object (V2 CH5-R01 fold) | **ACCEPT** |
| 3A/3B/3E synthesis-artefact citations resolve at file:line | **ACCEPT** |
| Grep-count claims (StructLayout=960, backend_shape/LayoutFacts=0, StructRegistry tape=0) | **ACCEPT** |

**Counts: 21 ACCEPT, 0 REVISE, 0 REJECT.** No load-bearing CH1 defect
remains. The G3 gate object applies cleanly at HEAD `2a76916ac`; every
proposed delta cites a real T-P2 LAC / T-P1 divergence; every cited
V1-surface section resolves at `file:line`; the disposition matrix is
complete with zero silent drops.
