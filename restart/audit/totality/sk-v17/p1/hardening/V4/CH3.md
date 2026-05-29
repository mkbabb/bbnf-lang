---
lens: CH3-REGRESSION
pass: T-P1-excavation
cycle: V4
generated_at: 2026-05-29T25:30:00Z
subject_artefacts:
  - restart/audit/totality/sk-v17/p1/1a-substrate-evidence.md
  - restart/audit/totality/sk-v17/p1/1b-codegen-evidence.md
  - restart/audit/totality/sk-v17/p1/1c-runtime-evidence.md
  - restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md
  - restart/audit/totality/sk-v17/p1/1e-locks-evidence.md
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
contract: restart/prompts/totality/PASS-1-EXCAVATION.md §3 (CH3), §3Z; ORCHESTRATOR §3W/§3Z
master_head: 445925167
verification_method: "Read over all eight V4 1X artefacts + prior V2/V3 CH3.md; sed -n + grep -n/-rn over restart/skinny/tranches/sk-v17/SPEC.md (verified anchors :258, :481-483, :791, :793-795, :796, :806, :807-811, :824, :825, :837-840, :852-854, :110-114), restart/locks/LOCKS.md (:75, :100-116, :107-108), restart/ARCHITECTURE.md (:1088, :1206); grep -n over crates/ir/src/registry/struct.rs (FieldSource:84, StructLayout:202, StructRegistry:313, layout():331), crates/core/src/runtime/tape/mod.rs (begin_compound:185, layout.rule_id & 0x1F:186); grep -rn over sk-v17/p1/*.md for revival verbs, monotonic-direction guards, and residual :807-as-x86-bar errors; no cargo/build mutation"
disposition_counts:
  accept: 8
  revise: 0
  reject: 0
prior_cycle_dispositions_status:
  CH3-R4-SUB17-005-x86-bar-at-806-not-807: RESOLVED
---

## §0 — Lens Charge

CH3 REGRESSION scans the SK-V17 T-P1 V4 excavation for three failure modes
(PASS-1-EXCAVATION §3 CH3): (1) an inventory **re-opens a route already
pre-blocked** in the SK-V17 corpus (the `skinny/REDRESS.md`-derived
pre-block ledger crystallised into SPEC §9 `:789-857`, restated for the
W1 context at `:481-483`); (2) the **rejected-route pre-block list is
correctly identified** — §3 CH3 names **1D and 1E** the canonical
identifiers; (3) an **admitted REDRESS row is mis-catalogued as
unimplemented**. The four load-bearing pre-blocks the fold must respect,
per the V4 charge: **AZ-IV eager** (SPEC `:791`), **StructRegistry
indirection** (`:793-795`), **fact-stream** (`:796`), **x86** (`:806`).
No revival implied: every crates/core construct excavated is the SK-V18
fold TARGET (totality tree), not a proposal; the monotonic direction is
skinny-proven → crates/core.

SUBJECT-SET COMPLETENESS. The full 1A–1F inventory set is present under
`restart/audit/totality/sk-v17/p1/` at `cycle: V4` (frontmatter verified
for 1a/1d/1e; the 1b/1c/1f triad carry the V4 fold). The canonical
pre-block identifiers 1D (`1d-skinny-lessons.md`) and 1E
(`1e-locks-evidence.md`) both exist and both carry a Do-Not-Redrive /
pre-block ledger (1D "Past-Corpora Do-Not-Redrive Ledger" `:112-127`; 1E
"CH3 Do-Not-Redrive Ledger" `:126-137`). CH3 discharges its §3 CH3 "1D and
1E correctly identify the list" charge against the canonical artefacts.

PRIOR-CYCLE FOLD VERIFICATION. The single V3 CH3 REVISE disposition is
verified RESOLVED in V4 — fold-checked at the cited locus, not taken on
the author's frontmatter assertion:

| V3 disposition | Folded? | Live-evidence locus verified |
|---|---|---|
| **CH3-R4** SUB17-005 x86-bar §9 anchor off-by-one (`:807` should be `:806`) | RESOLVED | `1a-substrate-evidence.md:82` SUB17-005 now reads "the x86 bar is `…SPEC.md:806` ('x86 / AVX-512 / SVE: aarch64 only'; the second-substrate block is the separate `:807-811`), and `:258`". The fold is recorded in 1A frontmatter `:18` `CH3-R4-SUB17-005-x86-bar-at-806-not-807` and narrated in the V4-fold note `:66-69`. `sed -n '806p;807p'` over SPEC verifies `:806` = "x86 / AVX-512 / SVE: aarch64 only (Apple cores have no SVE)" and `:807` = "Second substrate: skinny StructLayout/TapeStructBuilder/TapeCursor …" — the corrected anchors are exact. A `grep -rn :807` over `sk-v17/p1/*.md` filtered for x86/AVX/SVE returns ONLY the CH3-R4 fix itself (frontmatter `:18` + the corrected cell `:82`); no surviving `:807`-as-x86-bar error in any inventory. |

A second V4 sibling fold (CH5-S8, recorded in 1A frontmatter `:19`)
re-anchored the Sidecar-producer firewall row's REDRESS-53 cite to SPEC
`:825` (W2 row) + `:839` (shortlist condition 1), retaining `:807-811`
only as the general second-substrate cite. From a CH3 view this is
*toward* citation truth: `sed -n '825p'` = the W2 REDRESS-53 row,
`sed -n '837,840p'` = shortlist condition 1 "A retained parallel index
collapses into REDRESS-53". No route re-opened by the re-anchor; the
REDRESS-53 pre-block is now cited at its naming lines rather than the
second-substrate block. Noted, not a CH3 defect.

## §1 — Pre-Block Citation Resolution (do the §9 cites resolve)

Every §9 pre-block citation made by the V4 inventories was resolved
against `restart/skinny/tranches/sk-v17/SPEC.md` by `sed -n` + `grep -n`.
The four load-bearing pre-blocks resolve to exact lines:

| Pre-block | SPEC anchor (verified V4) | Verified line text | Resolves |
|---|---|---|---|
| AZ-IV eager value tree (118×) | `:791` | "**AZ-IV eager value tree** (the 118× regression): eager per-leaf payload, f64-alloc-per-number, per-color `Box<CssColor>`. Materialization stays lazy-by-default." | YES |
| StructRegistry / Arena<G> / Builder<G> hot-path indirection | `:793-795` | "**StructRegistry / Arena<G> / Builder<G> hot-path indirection** (28-65× bbnf/sheets, 983× css bootstrap, 10583× WATCHDOG tailwind). No registry lookup in the per-leaf hot path." | YES |
| CSS fact-stream String (admission plane) | `:796` | "**CSS fact-stream String** as a live admission plane (`emit_fact_stream`/`emit_full_parse`/…): diagnostic-only." | YES |
| x86 / AVX-512 / SVE | `:806` | "**x86 / AVX-512 / SVE:** aarch64 only (Apple cores have no SVE)." | YES |
| Second substrate (D6) | `:807-811` | "**Second substrate:** skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor`, public `UnionTape`, … sixth `BackendShape`, retained sidecars …" | YES |
| W1 attribution (StructRegistry indirection) | `:824` | "W1 | AZ-IV eager; fact-stream as admission; W5C array …; StructRegistry indirection; 24-row broadcast; second substrate; REDRESS 50-55, 60-72" | YES |
| W1-context restatement | `:481-483` | "Pre-blocked routes: AZ-IV eager value tree; the fact-stream String …; StructRegistry/Arena<G> hot-path indirection; … a second substrate" | YES |
| REDRESS-53 (W2 row / shortlist cond 1) | `:825` / `:837-840` | `:825` "L1/L4 index as parallel retained vector (REDRESS-53)"; `:839` "A retained parallel index collapses into REDRESS-53." | YES |
| §9 REJECTed set (x86 collapsed-stage / lo6 / D6) | `:852-854` | "asmjson collapsed-stage FSM (x86, host-blocked) · lo6 `classify_tbl4` on the CSS alphabet · D6 second substrate." | YES |
| aarch64 primary x86 bar | `:258` | "aarch64 only. No x86, no AVX-512, no SVE." | YES |

The live-code anchors of the StructRegistry fence resolve exactly:
`crates/ir/src/registry/struct.rs:84` `pub enum FieldSource`, `:202`
`pub struct StructLayout`, `:313` `pub struct StructRegistry`, `:331`
`pub fn layout(&self, rule_id: RuleId) -> Option<&StructLayout>`; consumer
`crates/core/src/runtime/tape/mod.rs:185` `fn begin_compound(&mut self,
layout: &StructLayout)` reading **only** `layout.rule_id & 0x1F` at `:186`
— confirming the fold inherits a no-per-leaf-lookup `begin_compound`. The
fence is anchored to real code, not recalled.

No phantom REDRESS id; no recalled LOC. The citation floor is **sound with
zero residual off-by-one** — the V3 CH3-R4 defect is closed and no new
mis-anchor surfaced under this cycle's line-exact re-resolution.

## §2 — Re-Opened-Route Scan (does any V4 inventory revive a pre-blocked route)

CH3's central firewall. Verdict: **no inventory re-opens a pre-blocked
route.** A `grep -rn` over `sk-v17/p1/*.md` for revival verbs
(`re-introduce`, `revive`, `add StructRegistry`, `new substrate`, `sixth
BackendShape`, `adopt x86`, `relocate … into skinny`) returns only
guarded negations — the two `6th shape` hits are both "absorb **without** a
6th shape" (1D `SK17L-006` `:137`, 1B `BSHAPE17-005` `:91`). Each
crates/core construct that NAMES a pre-blocked shape is catalogued as the
fold-deletion / fold-fence TARGET, with the monotonic skinny→totality
direction guarded (guard present in 1A, 1D, 1E, 1f-coherence, 1f-past):

- **AZ-IV eager (`:791`):** the live eager `CssTypedValue` enum
  (`crates/core/src/runtime/css_l4/value.rs:414`) + six `pending_*` Vecs +
  `pending_value: Option` (`builder.rs:71-79`) is catalogued by 1D
  `SK17L-003` (`:89`) + ledger `L-SK17-01` (`:120`), 1E ledger `:135`,
  1f-past as the eager-value-tree fold-DELETION target — "replace with
  lazy `ValueRef<G>`, do not carry forward." The lazy projection is
  catalogued unimplemented; the eager tree is the deletion target. No
  revival. (V2 "nine pending Vecs" overcount remains correctly folded to
  six-Vec + Option across 1D/1E.)
- **StructRegistry indirection (`:793-795`):** catalogued as the
  fold-FENCE target across 1D `SK17L-004` (`:90`) + ledger `L-SK17-02`/
  `L-SK17-02b` (`:121-122`), 1E `D-1E-SKV17-03` (`:121`) + ledger `:133` +
  `LAC-1E-SKV17-02` (`:168`), 1C `RT17-006` (`:77`) + `U-RT17-003` (`:118`).
  The fence — "no per-leaf `StructRegistry::layout(rule_id)` lookup in the
  hot path; the `FieldSource` walk is compile-time emission resolved once"
  — matches the live shape: `begin_compound` already takes a pre-resolved
  `&StructLayout` and reads only `layout.rule_id & 0x1F` (`:186`),
  performing NO internal registry lookup. No per-leaf indirection
  proposed; the regression is fenced, not re-opened.
- **fact-stream (`:796`):** 1D `SK17L-005` (`:91`) + ledger `L-SK17-03`
  (`:123`) catalogue the CSS fact-stream String as PERMANENT-PRE-BLOCK as
  an admission plane, with the V1 Lock-1 `FactStream` category
  (`LOCKS.md:100-116`) reconciled (1E `:113`) as surviving only for
  typed-schema output planes — NOT a contradiction, NOT a revival of the
  String CSS admission plane. The live skinny `W5C_REQUEST_FACT_PROFILES`
  (`codegen/src/lib.rs:336`) is catalogued RETIRE-to-diagnostic, not
  extend.
- **x86 (`:806`) + aarch64 CollapsedStage:** 1A `SUB17-005` (`:82`,
  corrected anchor) / `SUB17-008` (`:85`), 1B `BSHAPE17-005`, 1C
  `RT17-007`, 1D `SK17L-006` (`:92`) + ledger `L-SK17-06` (`:126`), 1E
  `D-1E-SKV17-04` (`:122`) + ledger `:137` all classify §7.3
  CollapsedStage as x86/AVX-512-pinned and the aarch64 candidate as the
  SPEC-named **UNKNOWN-2D-05** (`ARCHITECTURE.md:1206`), NOT a fresh gap
  and NOT an aarch64-CollapsedStage re-derivation. The 5-shape Lock-10
  canon (`LOCKS.md:107-108`) is held; NEON sits under the four LLVM
  shapes' scan-leaf FFI. No 6th shape proposed. The multi-arch
  `crates/simd-scan` (neon/avx2/avx512/wasm/scalar) is catalogued
  impl-exceeds-spec scope pressure deferred to T-P2 (1D `U-SK17L-003`
  `:156`, 1E `LAC-1E-SKV17-06` `:172`), NOT an x86 admission.
- **D6 second substrate (`:807-811`):** the single most important CH3
  inversion guard. The §9 block names skinny `StructLayout`/
  `TapeStructBuilder`/`TapeCursor` as FORBIDDEN-IN-SKINNY — precisely the
  crates/core fold-target names. 1D `L-SK17-07` (`:127`), 1E ledger `:136`,
  1f-past all correctly state the fold "adopts the PROVEN skinny
  `Tape`/`ValueRef` INTO crates/core, never relocates crates/core
  `TapeStructBuilder`/`TapeCursor` INTO skinny" (monotonic skinny→totality,
  SPEC `:110-114`, verified). No inversion.

The AoS-vs-SoA tape divergence (core 16-byte `TapeRec`, `record.rs:103`;
vs skinny SoA `Tape<'input>` `mod.rs:94`) is correctly catalogued as a
fold-convergence question (1C `RT17-001` `:72`, 1E `L01` row `:109`, 1D
`SK17L-001` `:87`), NOT a same-tree parallel substrate and NOT a re-opened
route — both encodings are admitted offset-tape shapes under Lock 1, and
1C `:72` explicitly distinguishes them from the AV.04 columnar-SoA that
Lock 1 buries.

## §3 — Admitted-REDRESS-Row Mis-Catalogue Scan

Verdict: **no admitted REDRESS row is mis-catalogued as unimplemented.**
The "unimplemented" verdicts across 1A–1F attach to the SKINNY-PROVEN
generalizations (SoA `Tape`, lazy `ValueRef<G>`, the `BackendRule`/
`FieldSource`-walk projection) that are ABSENT in crates/core — the
correct sense of "unimplemented in the fold target." The pre-blocked
shapes (eager builder, AoS `TapeRec`, `StructRegistry`/`StructLayout`
indirection) are catalogued as PRESENT fold-targets, not as "missing." The
UNWIRED tape is correctly labelled spec-claims-implemented (1C `RT17-003`
`:74` "spec-claims-implemented (UNWIRED confirmed as stated)", 1E `:103`
"UNWIRED (grep-zero outside `tape/`)") — the §0.1.11 claim that the tape
exists-but-unwired is verified true, not mislabelled.

The V3 scan false-negative CORRECTION carried into V4 is clean from a CH3
view: the core CSS structural scan IS wired (1C `:109` `css_l4.rs:15976-
15982` builds `StructuralAlphabet` + calls `scan_structural`; all 8
generated grammars carry `scan_structural` + `OnceCell<StructuralIndex>`,
`math.rs` holds the field but scan-count 0 → 8 wired, not 9). The residual
gap is correctly re-classified as the absent tape CONSUMER, not the scan.
No admitted row flipped to unimplemented in the wrong direction; the
retained `OnceCell` index is correctly bound to REDRESS-53 (1C `RT17-005`
`:76`, SPEC `:825`,`:839`) with the index-IS-tape-or-`local_temp_only`
fork, not catalogued as a permissible parallel substrate.

## §4 — Dispositions

### ACCEPT (8)

| ID | Section | Basis |
|---|---|---|
| CH3-A1 | V3 CH3-R4 fold (1A SUB17-005 `:82` x86-bar `:806`) | Closes the sole V3 REVISE. SUB17-005 now cites the x86 bar at SPEC `:806` (verified line "x86 / AVX-512 / SVE: aarch64 only") with `:807-811` correctly tagged the separate second-substrate block; fold recorded in frontmatter `:18` + V4-fold note `:66-69`. `grep -rn :807` over the tree filtered for x86/AVX/SVE returns only the fix itself — zero residual mis-anchor. The pre-block is correctly identified; no route re-opened. |
| CH3-A2 | 1D `SK17L-004` + ledger `L-SK17-02`/`L-SK17-02b` + 1E `D-1E-SKV17-03`/ledger `:133` + 1C `RT17-006` (StructRegistry fence) | The 28-65×/983×/10583× pre-block is in both canonical 1D/1E ledgers with the no-per-leaf-lookup fence; SPEC `:793-795`,`:824` verified exact; `begin_compound:185` reading only `layout.rule_id & 0x1F` (`:186`) verified — the fence matches live code. No re-opening. |
| CH3-A3 | 1D `L-SK17-02b` + 1E `LAC-1E-SKV17-02` + 1C Value-API row `:86` (FieldSource compile-time) | The `FieldSource{TypedLeaf,BranchTag,SeqPosition,RepeatElement,RuleReference}` walk is fenced "COMPILE-TIME projection-emission, resolved once … NOT a per-leaf runtime StructRegistry indirection", SPEC `:794-795` anchored; the generalization is fenced against the regression it abuts. |
| CH3-A4 | 1D `L-SK17-07` + 1E ledger `:136` + 1f-past (D6 inversion guard) | The FORBIDDEN-in-skinny vs fold-target-in-core distinction is exactly right; SPEC `:807-811` verified to name `StructLayout`/`TapeStructBuilder`/`TapeCursor`; monotonic direction `:110-114` verified; prevents the worst CH3 failure (relocating core substrate into skinny). |
| CH3-A5 | 1A `SUB17-005`/`SUB17-008` + 1B `BSHAPE17-005` + 1D `L-SK17-06` + 1E `D-1E-SKV17-04` (x86/CollapsedStage not target) | x86 path classified non-target, aarch64 CollapsedStage classified the SPEC-named UNKNOWN-2D-05 not a fresh gap; SPEC `:258`,`:806`,`:852-854` + `ARCHITECTURE.md:1206` verified; 5-shape canon held (Lock 10 `:107-108`); no 6th shape, no aarch64-CollapsedStage re-derivation. |
| CH3-A6 | 1D `SK17L-003` + ledger `L-SK17-01` + 1E ledger `:135` (AZ-IV eager fold-deletion) | The eager `CssTypedValue` + six pending Vecs is catalogued as deletion target with lazy-`ValueRef<G>` replacement, not carried forward; `value.rs:414` + `builder.rs:71-79` verified. No revival. |
| CH3-A7 | 1D `SK17L-005` + 1E `:113` (fact-stream / FactStream reconcile) | The CSS fact-stream String admission plane is PERMANENT-PRE-BLOCK while the V1 `FactStream` category (`LOCKS.md:100-116`) survives for typed-schema output planes — correctly NOT catalogued as a contradiction, correctly NOT reviving the String admission plane. |
| CH3-A8 | 1C `RT17-005` REDRESS-53 anchor + 1A Sidecar firewall row (CH5-S8 re-anchor `:825`,`:839`) | The retained `OnceCell<StructuralIndex>` (8 carriers) is correctly bound to REDRESS-53 at its naming lines SPEC `:825`/`:839` with the index-IS-tape-or-`local_temp_only` fork; the UNWIRED tape is correctly labelled spec-claims-implemented; no admitted row mis-flipped to unimplemented. |

### REVISE (0)

No V4 inventory carries an unfenced pre-block, a mis-identified resolved
pre-block, an off-by-one §9 anchor, or an admitted REDRESS row
mis-catalogued as unimplemented. The single V3 REVISE (CH3-R4) is folded
and verified; no new defect surfaced under this cycle's line-exact
re-resolution.

### REJECT (0)

No V4 inventory re-opens a pre-blocked route, mis-identifies a resolved
pre-block, mis-catalogues an admitted REDRESS row as unimplemented, or
inverts the monotonic direction. Nothing rises to REJECT.

## §5 — Continuity + Convergence Accounting

V3 CH3 returned 7 ACCEPT / 1 REVISE / 0 REJECT (87.5%). The single V3
REVISE (CH3-R4) is verified RESOLVED in V4 (§0 fold table), with no orphan
left open and no new REVISE surfaced. V4 CH3 returns **8 ACCEPT / 0 REVISE
/ 0 REJECT** — ACCEPT-rate **8/8 = 100%**, at/above the §3Z ≥95% bar. The
subject set is complete (1A–1F all present at `cycle: V4`). Against the
§3Z two-consecutive-cycle rule: V3 was the first substantive cycle on the
complete subject set (V2 covered only the 1F triad), V3 CH3 was 87.5%
(below bar), V4 CH3 is 100% (above bar) — CH3 alone now has ONE
above-bar cycle; the two-consecutive close requires the *aggregate* wave
to hold ≥95% across two consecutive cycles, which the aggregator
accounts. No CH3 finding blocks convergence.

## §6 — Verdict

CH3 ACCEPT-rate on the V4 subject set: **8 ACCEPT / 8 dispositioned =
100%**, with **0 REVISE / 0 REJECT**. The sole V3 CH3 REVISE (CH3-R4, the
`:807`→`:806` off-by-one for the x86 pre-block's §9 anchor in 1A
SUB17-005) is verified FOLDED — the corrected cell cites the x86 bar at
SPEC `:806` with `:807-811` correctly tagged the separate second-substrate
block, and a tree-wide `grep -rn :807` filtered for x86/AVX/SVE returns
only the fix itself. The re-opened-route firewall is clean: no inventory
revives AZ-IV eager, StructRegistry indirection, fact-stream, x86, or the
D6 second substrate; every crates/core construct is the SK-V18 fold TARGET
with the monotonic skinny→totality direction guarded; the D6 inversion
trap is correctly fenced; the StructRegistry/FieldSource fence matches the
live `begin_compound(&StructLayout)` no-per-leaf-lookup shape. The four
load-bearing pre-blocks are correctly identified in both canonical 1D/1E
ledgers. No admitted REDRESS row is mis-catalogued as unimplemented. CH3
converges at 100% on this cycle; nothing to fold into V5.
