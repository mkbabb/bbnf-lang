---
lens: CH3-REGRESSION
pass: T-P1-excavation
cycle: V3
generated_at: 2026-05-29T24:30:00Z
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
verification_method: "Read over all eight V3 1X artefacts + prior V2 CH3.md; sed/grep over restart/skinny/tranches/sk-v17/SPEC.md (§9 :481-483, :789-857), restart/ARCHITECTURE.md, restart/locks/LOCKS.md; grep -n over crates/ir/src/registry/struct.rs (FieldSource:84, StructLayout:202, StructRegistry:313, layout():331), crates/core/src/runtime/tape/{mod,record}.rs (begin_compound:185, layout.rule_id&0x1F:186, TapeRec:103/120), crates/core/src/runtime/css_l4/builder.rs (pending Vecs :71-79), skinny/crates/runtime/src/tape/mod.rs (SoA Tape:94, ValueRef:175); no cargo/build mutation"
disposition_counts:
  accept: 7
  revise: 1
  reject: 0
prior_cycle_dispositions_status:
  CH3-R1-StructRegistry-preblock-absent: RESOLVED
  CH3-R2-FieldSource-walk-unfenced: RESOLVED
  CH3-R3-1d-1e-pre-block-identifiers-absent: RESOLVED
---

## §0 — Lens Charge

CH3 REGRESSION scans the SK-V17 T-P1 V3 excavation for three failure modes
(PASS-1-EXCAVATION §3 CH3): (1) an inventory **re-opens a route already
pre-blocked** in the SK-V17 corpus (the `skinny/REDRESS.md`-derived
pre-block ledger crystallised into SPEC §9 `:789-857`, with the
W1-context restatement at `:481-483`); (2) the **rejected-route pre-block
list is correctly identified** — and §3 CH3 names **1D and 1E** as the
canonical identifiers; (3) an **admitted REDRESS row is mis-catalogued as
unimplemented**. The four load-bearing pre-blocks the fold must respect,
per the V3 charge: **AZ-IV eager** (SPEC `:791`), **StructRegistry
indirection** (`:793-795`), **fact-stream** (`:796`), **x86** (`:806`).
No revival implied: every crates/core construct excavated is the SK-V18
fold TARGET (totality tree), not a proposal; the monotonic direction is
skinny-proven → crates/core.

SUBJECT-SET COMPLETENESS (closes prior CH3-R3). The full 1A–1F inventory
set is now present under `restart/audit/totality/sk-v17/p1/` at `cycle:
V3`. The canonical pre-block identifiers 1D (`1d-skinny-lessons.md`) and
1E (`1e-locks-evidence.md`) — absent in V2 and the sole basis of prior
CH3-R3 — both exist and both carry a Do-Not-Redrive / pre-block ledger.
CH3 can now discharge its §3 CH3 "1D and 1E correctly identify the list"
charge against the canonical artefacts rather than against the 1F triad
alone.

PRIOR-CYCLE FOLD VERIFICATION. All three V2 CH3 REVISE dispositions are
verified RESOLVED in V3 — fold-checked at the cited loci, not taken on the
authors' frontmatter assertions:

| V2 disposition | Folded? | Live-evidence locus verified |
|---|---|---|
| **CH3-R1** StructRegistry/Arena<G>/Builder<G> pre-block wholly absent from all p1 artefacts | RESOLVED | `grep -rln StructRegistry` over `sk-v17/p1/*.md` now returns 7 of 8 inventories. 1D `L-SK17-02` (`1d-skinny-lessons.md:118`), 1E `D-1E-SKV17-03` + ledger row (`1e-locks-evidence.md:116,128`), 1f-anti-pattern row (`:65`), 1c `RT17-006` (`:84`). Each cites SPEC `:793-795` + W1 `:824` + the live `struct.rs:313`/`begin_compound:185` triad. |
| **CH3-R2** FieldSource walk unfenced against per-leaf indirection on the value-API fold | RESOLVED | 1f-coherence Value-API row (`:93`) and Layout-shape row (`:94`) now both carry "compile-time projection-emission, resolved ONCE … NOT a per-leaf runtime StructRegistry walk", citing SPEC `:794-795`. Mirrored in 1D `L-SK17-02b` (`:119`), 1E `LAC-1E-SKV17-02` (`:159`), 1c `U-RT17-003` (`:125`). |
| **CH3-R3** 1D/1E pre-block identifiers absent under sk-v17/ | RESOLVED | both `1d-skinny-lessons.md` and `1e-locks-evidence.md` present at `cycle: V3`; both carry a pre-block ledger (1D "Past-Corpora Do-Not-Redrive Ledger" `:109-124`; 1E "CH3 Do-Not-Redrive Ledger" `:121-133`). |

## §1 — Pre-Block Citation Resolution (do the §9 cites resolve)

Every §9 pre-block citation made by the V3 inventories was resolved
against `restart/skinny/tranches/sk-v17/SPEC.md` by `grep -n` + `sed -n`.
The four load-bearing pre-blocks resolve to exact lines:

| Pre-block | SPEC anchor (verified) | Verified line text | Resolves |
|---|---|---|---|
| AZ-IV eager value tree (118×) | `:791` | "AZ-IV eager value tree (the 118× regression): eager per-leaf payload, f64-alloc-per-number, per-color `Box<CssColor>`. Materialization stays lazy-by-default." | YES |
| StructRegistry / Arena<G> / Builder<G> hot-path indirection | `:793-795` | "StructRegistry / Arena<G> / Builder<G> hot-path indirection (28-65× bbnf/sheets, 983× css bootstrap, 10583× WATCHDOG tailwind). No registry lookup in the per-leaf hot path." | YES |
| CSS fact-stream String (admission plane) | `:796` | "CSS fact-stream String as a live admission plane … diagnostic-only." | YES |
| x86 / AVX-512 / SVE | `:806` | "x86 / AVX-512 / SVE: aarch64 only (Apple cores have no SVE)." | YES |
| Second substrate (D6) | `:807-811` | "Second substrate: skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor`, public `UnionTape`, … sixth `BackendShape` …" | YES |
| W1 attribution (StructRegistry indirection) | `:824` | "W1 | AZ-IV eager; fact-stream as admission; W5C array …; StructRegistry indirection; …" | YES |
| W1-context restatement | `:481-483` | "Pre-blocked routes: AZ-IV eager value tree; the fact-stream String …; StructRegistry/Arena<G> hot-path indirection; …" | YES |
| §9 REJECTed set (D6 / x86 collapsed-stage) | `:852-854` | "asmjson collapsed-stage FSM (x86, host-blocked) · lo6 `classify_tbl4` on the CSS alphabet · D6 second substrate." | YES |
| aarch64 primary x86 bar | `:258` | "aarch64 only. No x86, no AVX-512, no SVE." | YES |

The live-code anchors of the StructRegistry fence also resolve exactly:
`crates/ir/src/registry/struct.rs:84` `pub enum FieldSource`, `:202`
`pub struct StructLayout`, `:313` `pub struct StructRegistry`, `:331`
`fn layout(&self, rule_id) -> Option<&StructLayout>`; and the consumer
`crates/core/src/runtime/tape/mod.rs:185` `fn begin_compound(&mut self,
layout: &StructLayout)` reading **only** `layout.rule_id & 0x1F` at `:186`
— confirming the fold inherits a no-per-leaf-lookup `begin_compound`. The
fence is anchored to real code, not recalled.

No phantom REDRESS id; no recalled LOC. The citation floor is sound
**save the single off-by-one in §4 CH3-R4**.

## §2 — Re-Opened-Route Scan (does any V3 inventory revive a pre-blocked route)

CH3's central firewall. Verdict: **no inventory re-opens a pre-blocked
route.** Each crates/core construct that NAMES a pre-blocked shape is
catalogued as the fold-deletion / fold-fence TARGET, with the monotonic
skinny→totality direction explicitly guarded:

- **AZ-IV eager (`:791`):** the live eager `CssTypedValue` enum
  (`crates/core/src/runtime/css_l4/value.rs:414`, verified) + six
  `pending_*` Vecs + `pending_value: Option` (`builder.rs:71-79`,
  verified — `pending_value` + `pending_rules`/`pending_decls`/
  `pending_selectors`/`pending_values`/`pending_blocks`/`pending_components`)
  is catalogued by 1D `SK17L-003`, 1E pre-block ledger `:130`, 1f-past
  `:59` as the eager-value-tree fold-DELETION target — "replace with
  lazy `ValueRef<G>`, do not carry forward." The lazy projection is
  catalogued unimplemented; the eager tree is the deletion target. No
  revival. (Note: the V2 CH5-S4 "nine pending Vecs" overcount is
  correctly folded to six-Vec + Option across 1D/1E/1f.)
- **StructRegistry indirection (`:793-795`):** catalogued as the
  fold-FENCE target across 1D `L-SK17-02`/`L-SK17-02b`, 1E
  `D-1E-SKV17-03`, 1c `RT17-006`, 1f-anti-pattern `:65`. The fence — "no
  per-leaf `StructRegistry::layout(rule_id)` lookup in the hot path; the
  `FieldSource` walk is compile-time emission resolved once" — matches the
  live shape: `begin_compound` already takes a pre-resolved `&StructLayout`
  and performs NO internal registry lookup. No per-leaf indirection
  proposed; the regression is fenced, not re-opened.
- **fact-stream (`:796`):** 1D `SK17L-005` + ledger `L-SK17-03`
  catalogue the CSS fact-stream String as PERMANENT-PRE-BLOCK as an
  admission plane, with the V1 Lock-1 `FactStream` category
  (`LOCKS.md:100-116`) correctly reconciled as surviving only for
  typed-schema output planes — NOT a contradiction, NOT a revival of the
  String CSS admission plane. The live skinny `W5C_REQUEST_FACT_PROFILES`
  (`codegen/src/lib.rs:336`) is catalogued as RETIRE-to-diagnostic, not
  extend.
- **x86 (`:806`) + aarch64 CollapsedStage:** 1A `SUB17-005`/`SUB17-008`,
  1B `BSHAPE17-005`, 1C `RT17-007`, 1D `SK17L-006`/`L-SK17-06`, 1E
  `D-1E-SKV17-04`, 1f-past `:60` all classify §7.3 CollapsedStage as
  x86/AVX-512-pinned and the aarch64 candidate as the SPEC-named
  **UNKNOWN-2D-05** (`ARCHITECTURE.md:1206`), NOT a fresh gap and NOT an
  aarch64-CollapsedStage re-derivation. The 5-shape Lock-10 canon
  (`LOCKS.md:107-108`) is held; NEON sits under the four LLVM shapes'
  scan-leaf FFI. No 6th shape proposed. The multi-arch `crates/simd-scan`
  (neon/avx2/avx512/wasm/scalar) is catalogued as impl-exceeds-spec scope
  pressure deferred to T-P2, NOT as an x86 admission.
- **D6 second substrate (`:807-811`):** the single most important CH3
  inversion guard. The §9 block names skinny `StructLayout`/
  `TapeStructBuilder`/`TapeCursor` as FORBIDDEN-IN-SKINNY — and these are
  precisely the crates/core fold-target names. 1A `SUB17`, 1D
  `L-SK17-07`, 1E ledger `:131`, 1f-past `:61` all correctly state the
  fold "adopts the PROVEN skinny `Tape`/`ValueRef` INTO crates/core, never
  relocates crates/core `TapeStructBuilder`/`TapeCursor` INTO skinny"
  (monotonic skinny→totality, SPEC `:110-114`, verified). No inversion.

The AoS-vs-SoA tape divergence (core 16-byte `TapeRec`, `record.rs:103`
const-asserted `:120`, mod-doc "kept AoS first … later SoA split"
`mod.rs:6-9`, verified; vs skinny SoA `Tape<'input>` `mod.rs:94`) is
correctly catalogued as a fold-convergence question (1A `SUB17-002`, 1E
`D-1E-SKV17-01`, 1D `U-SK17L-001`), NOT a same-tree parallel substrate and
NOT a re-opened route. 1A `:100` explicitly verifies "NO same-tree parallel
substrate."

## §3 — Admitted-REDRESS-Row Mis-Catalogue Scan

Verdict: **no admitted REDRESS row is mis-catalogued as unimplemented.**
The "unimplemented" verdicts across 1A–1F attach to the SKINNY-PROVEN
generalizations (SoA `Tape`, lazy `ValueRef<G>`, the `BackendRule`/
`FieldSource`-walk projection) that are ABSENT in crates/core — the
correct sense of "unimplemented in the fold target." The pre-blocked
shapes (eager builder, AoS `TapeRec`, `StructRegistry`/`StructLayout`
indirection) are catalogued as PRESENT fold-targets, not as "missing." The
UNWIRED tape is correctly labelled spec-claims-implemented (1A `:100`,
1C, 1E `:98` "UNWIRED … grep-zero outside `tape/`") — the §0.1.11 claim
that the tape exists-but-unwired is verified true, not mislabelled.

The V3 false-negative CORRECTION is itself clean from a CH3 view: the V2
1F triad claimed crates/core CSS structural scan was unwired; V3 (1D
`SK17L-008`, 1E verified-invariant `:97`, 1f-coherence COH-014 catch)
corrects to "all 8 generated grammars carry `scan_structural` +
`OnceCell<StructuralIndex>`, css_l4 included (`css_l4.rs:15982`)." This is
a correction TOWARD truth (no route re-opened by it), and it correctly
re-classifies the residual gap as the absent tape CONSUMER, not the scan.
No admitted row flipped to unimplemented in the wrong direction.

## §4 — Dispositions

### ACCEPT (7)

| ID | Section | Basis |
|---|---|---|
| CH3-A1 | 1D `L-SK17-02` + 1E `D-1E-SKV17-03` + 1c `RT17-006` (StructRegistry fence) | Closes prior CH3-R1. The 28-65×/983×/10583× pre-block is now in the canonical 1D/1E ledgers with the no-per-leaf-lookup fence; SPEC `:793-795`,`:824` verified exact; `begin_compound:185` reading only `layout.rule_id & 0x1F` (`:186`) verified — the fence matches live code. No re-opening. |
| CH3-A2 | 1f-coherence Value-API `:93` + Layout-shape `:94` (FieldSource compile-time) | Closes prior CH3-R2. Both fold-reconciliation cells now carry "compile-time projection-emission, resolved ONCE … NOT a per-leaf runtime StructRegistry walk", SPEC `:794-795` anchored; mirrored in 1D `:119`, 1c `U-RT17-003`. The generalization is now fenced against the regression it abuts. |
| CH3-A3 | 1D `L-SK17-07` + 1E ledger `:131` + 1f-past `:61` (D6 inversion guard) | The FORBIDDEN-in-skinny vs fold-target-in-core distinction is exactly right; SPEC `:807-811` verified to name `StructLayout`/`TapeStructBuilder`/`TapeCursor`; monotonic direction `:110-114` verified; prevents the worst CH3 failure (relocating core substrate into skinny). |
| CH3-A4 | 1A `SUB17-005`/`SUB17-008` + 1B `BSHAPE17-005` + 1D `L-SK17-06` (x86/CollapsedStage not target) | x86 path classified as non-target, aarch64 CollapsedStage classified as the SPEC-named UNKNOWN-2D-05 not a fresh gap; SPEC `:258`,`:852-854` + `ARCHITECTURE.md:1206` verified; 5-shape canon held (Lock 10 `:107-108`); no 6th shape, no aarch64-CollapsedStage re-derivation. |
| CH3-A5 | 1D `SK17L-003` + 1E ledger `:130` + 1f-past `:59` (AZ-IV eager fold-deletion) | The eager `CssTypedValue` + six pending Vecs is catalogued as deletion target with lazy-`ValueRef<G>` replacement, not carried forward; `value.rs:414` + `builder.rs:71-79` verified (six pending Vecs + `pending_value:Option`, V2 overcount correctly folded). No revival. |
| CH3-A6 | 1D `SK17L-005` + 1E (fact-stream / FactStream reconcile) | The CSS fact-stream String admission plane is PERMANENT-PRE-BLOCK while the V1 `FactStream` category (`LOCKS.md:100-116`) survives for typed-schema output planes — correctly NOT catalogued as a contradiction, correctly NOT reviving the String admission plane. |
| CH3-A7 | V3 scan false-negative correction (1D `SK17L-008`, 1E `:97`, COH-014 catch) | The "core CSS scan unwired" V2 false-negative is corrected toward truth (all 8 grammars scan-wired, `css_l4.rs:15982` verified) without re-opening any route; residual gap correctly re-scoped to the absent tape consumer. No admitted row mis-flipped. |

### REVISE (1)

| ID | Section / locus | Defect | Concrete fix |
|---|---|---|---|
| **CH3-R4** | 1A `SUB17-005` impl/spec cell (`restart/audit/totality/sk-v17/p1/1a-substrate-evidence.md:79`) | The x86-bar §9 anchor is **off by one line**. SUB17-005 cites "SK-V17 … EXPLICITLY bars x86/AVX-512/SVE (`…SPEC.md:258`, **:807**)". SPEC `:807` is the *Second-substrate* block heading ("**Second substrate:** skinny `StructLayout`/…"); the x86/AVX-512/SVE pre-block is at **`:806`** ("**x86 / AVX-512 / SVE:** aarch64 only (Apple cores have no SVE)"). One of the four load-bearing pre-blocks thus carries a one-off secondary cite for its own §9 anchor. The pre-block is *correctly identified* (the primary cite `:258` is exact, the monotonic-direction guard holds, no route re-opened), so this is a citation-precision defect under §3 CH3 "pre-block list correctly identified", not a re-opening — hence REVISE not REJECT. | Change SUB17-005's secondary cite from `:807` to `:806` (the x86/AVX-512/SVE line). If the intent was to also cite the Second-substrate block, write it as a distinct cite: "x86 bar `:806`; second-substrate block `:807-811`." Cross-check: 1A `:112` already cites `:258,:852-854` correctly for the same pre-block, so only the `:79` cell needs the `:807`→`:806` correction. |

### REJECT (0)

No V3 inventory re-opens a pre-blocked route, mis-identifies a resolved
pre-block, mis-catalogues an admitted REDRESS row as unimplemented, or
inverts the monotonic direction. Nothing rises to REJECT.

## §5 — Continuity + Convergence Accounting

V2 CH3 returned 6 ACCEPT / 3 REVISE / 0 REJECT (67%). All three V2 REVISEs
(CH3-R1/R2/R3) are verified RESOLVED in V3 (§0 fold table), with no orphan
left open. V3 CH3 returns **7 ACCEPT / 1 REVISE / 0 REJECT** — the single
residual REVISE (CH3-R4) is a new finding (an off-by-one cite uncovered by
this cycle's line-exact resolution of all four pre-block anchors), not a
re-surfacing of a folded V2 disposition. The subject set is now complete
(1A–1F all present at `cycle: V3`), so the §3Z two-consecutive-cycle
accounting can begin: V2 was the first substantive cycle (no V1 CH-set
exists in this tree), V3 is the second. CH3's ACCEPT-rate this cycle is
7/8 = **87.5%**, below the §3Z ≥95% bar — driven solely by CH3-R4. With
CH3-R4 folded (a one-character `:807`→`:806` edit), CH3 projects ≥95% in
V4, enabling the two-consecutive close if the aggregate wave holds.

## §6 — Verdict

CH3 ACCEPT-rate on the V3 subject set: **7 ACCEPT / 8 dispositioned =
87.5%**, with **1 REVISE / 0 REJECT**. The three V2 CH3 REVISE
dispositions are verified FOLDED — the StructRegistry hot-path-indirection
pre-block (the dominant V2 defect) is now in both canonical 1D/1E ledgers
with a live-code-matched no-per-leaf-lookup fence; the FieldSource walk is
fenced compile-time-once on the value-API fold; and 1D/1E now exist as the
canonical pre-block identifiers per §3 CH3. The re-opened-route firewall is
clean: no inventory revives AZ-IV eager, StructRegistry indirection,
fact-stream, x86, or the D6 second substrate; every crates/core construct
is the SK-V18 fold TARGET with the monotonic skinny→totality direction
guarded; the D6 inversion trap is correctly fenced. No admitted REDRESS row
is mis-catalogued as unimplemented. The single residual defect, CH3-R4, is
a one-line off-by-one cite (`:807` vs `:806`) for the x86 pre-block's §9
anchor in one 1A cell — pre-block identification is otherwise sound. Fold
CH3-R4 into V4.
