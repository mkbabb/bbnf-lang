---
lens: CH3-REGRESSION
pass: T-P1-excavation
cycle: V2
generated_at: 2026-05-29T22:40:00Z
subject_artefacts:
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
contract: restart/prompts/totality/PASS-1-EXCAVATION.md §3 (CH3), §3Z; ORCHESTRATOR §3W/§3Z
master_head: 445925167
verification_method: "Read over the three 1F V2 artefacts; rg/sed over restart/skinny/tranches/sk-v17/SPEC.md, restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md, restart/locks/LOCKS.md, restart/ARCHITECTURE.md; grep+sed over crates/core/src/runtime/{tape,css_l4,json}/, crates/ir/src/registry/{mod,struct}.rs; prior continuity sed over restart/audit/totality/p1/1F-coherence-scan.md; no cargo/build mutation"
disposition_counts:
  accept: 6
  revise: 3
  reject: 0
---

## §0 — Lens Charge

CH3 REGRESSION scans the SK-V17 T-P1 excavation for three failure modes
(PASS-1-EXCAVATION §3 CH3): (1) an inventory **re-opens a route already
pre-blocked** in the SK-V17 corpus (the `skinny/REDRESS.md`-derived
pre-block ledger lives in SPEC §9 `:789-857` for this tranche); (2) the
**pre-block list is mis-identified** by the excavation (1D/1E in the canon;
the 1F past-corpora ledger here); (3) an **admitted REDRESS row is
mis-catalogued as unimplemented**. Per the V2 charge, the load-bearing
pre-blocks the fold must respect are: **AZ-IV eager**, **StructRegistry
indirection**, **fact-stream**, **x86**. No revival implied: every
crates/core construct excavated is the SK-V18 fold TARGET, not a proposal.

NOTE ON SUBJECT SET. The `restart/audit/totality/sk-v17/p1/` directory
holds only the three 1F V2 artefacts; the 1A–1E inventories are not yet
present under the `sk-v17/` path. CH3 dispositions therefore cover the
sections actually written. The absence of 1D/1E (the canonical pre-block
identifiers per §3 CH3) is itself surfaced as REVISE CH3-R3 — the 1F
past-corpora ledger currently carries the pre-block-identification burden
alone, and one load-bearing pre-block falls through that gap.

NOTE ON CYCLE LABEL. All three 1F artefacts carry `cycle: V2` with
`prior_cycle_dispositions_folded: {accepted:[], rejected:[], revised:[]}`
and only `first_cycle_additions`. No V1 CH-set exists under
`sk-v17/p1/hardening/`. The V2 label has no V1 predecessor in this tree;
the artefacts are first-cycle in substance. Not a CH3 defect (no route
re-opened by the relabel), but recorded for the aggregator's convergence
accounting (§3Z two-consecutive-cycle rule cannot yet be satisfied).

## §1 — Pre-Block Citation Resolution (do the REDRESS/pre-block cites resolve)

Every pre-block citation made by the three 1F artefacts was resolved to
its SPEC/alphaC/LOCKS line. All resolve:

| Cited pre-block | 1F citation | Verified line | Resolves |
|---|---|---|---|
| AZ-IV eager value tree (118×) | PC17-003 → SPEC `:791-793` | SPEC `:791-793` "AZ-IV eager value tree (the 118× regression)…lazy-by-default" | YES |
| REDRESS-53 parallel index | PC17-001, AP17-002 → SPEC `:578`,`:837-840`; AP `:825` | SPEC `:577`,`:825`,`:839` carry REDRESS-53 + "retained parallel index collapses into REDRESS-53" | YES |
| x86/AVX-512/SVE not target | PC17-004, COH17-004 → SPEC `:806`,`:826`,`:854`; alphaC `:307-316` | SPEC `:806` "aarch64 only", `:854` "D6 second substrate…x86 host-blocked"; alphaC `:307-316` x86 §6 | YES |
| D6 second substrate | PC17-006 → SPEC `:807-811`,`:854` | SPEC `:807-811` second-substrate block names `StructLayout`/`TapeStructBuilder`/`TapeCursor`; `:854` "D6 second substrate" | YES |
| Class-column / cursor (REDRESS 96-98) | PC17-002 → LOCKS `:129-135`,`:142-144` | LOCKS `:129-135` UnionTape/class-column/streaming-cursor not shortlist-safe | YES |
| Lock-2 StructLayout retired | COH17-006, PC17-005 → LOCKS `:160`; alphaC `:29` | LOCKS `:160`; alphaC `:29` "Lock 2 (LOCKS.md:160) RETIRED the term StructLayout" | YES |
| Lock-1 FactStream 5th category | COH17-007 → LOCKS `:100-116`; ARCH `:1803` | LOCKS `:100-116` LAC-1E-14; ARCH `:1803` `admitted_fact_output` | YES |
| §0.1.11 unwired-tape fold | COH17-003 → SPEC `:110-114` | SPEC `:110-114` "Foldable into TOTALITY…adopt them in SK-V18" | YES |
| alphaC "measured into the ground" | PC exec → alphaC `:13`,`:20-25` | alphaC `:13`,`:20-25` core-tree symbols return zero on benched surface | YES |

No recalled LOC; no phantom REDRESS id. The citation floor is sound.

## §2 — Re-Opened-Route Scan (does any inventory revive a pre-blocked route)

CH3's central firewall. Verdict: **no inventory re-opens a pre-blocked
route.** Each crates/core construct that NAMES a pre-blocked shape is
catalogued as the fold-deletion TARGET with the monotonic skinny→totality
direction explicitly guarded:

- **AZ-IV eager:** the live `CssTypedValue` enum + nine `pending_*` Vecs
  (`crates/core/src/runtime/css_l4/builder.rs:74-79`, verified;
  `value.rs:414` `pub enum CssTypedValue`) is catalogued by PC17-003 as
  "the eager-value-tree shape; the fold must replace it with lazy
  `ValueRef<G>`…NOT carry the eager tree forward." Correct: the lazy
  projection is catalogued unimplemented (COH17-002), the eager tree is
  catalogued as deletion target — no revival.
- **x86 / CollapsedStage:** COH17-004 + PC17-004 catalogue the §7.3
  x86/AVX-512 `CollapsedStage` and `crates/simd-scan` avx2/avx512/wasm
  kernels as "NOT the SK-V17 admission target," deferring the totality
  scope decision to T-P2 as "architecture pressure, not admission." No
  aarch64-CollapsedStage re-derivation proposed.
- **D6 second substrate:** PC17-006 explicitly catches the inversion trap —
  the SPEC §9 second-substrate block names skinny `StructLayout`/
  `TapeStructBuilder`/`TapeCursor` as FORBIDDEN-IN-SKINNY, and these are
  precisely the crates/core fold-target names; PC17-006 states "the fold
  must NOT relocate the crates/core `TapeStructBuilder`/`TapeCursor` INTO
  skinny…SK-V18 adopts the PROVEN skinny `Tape`/`ValueRef` shape into
  crates/core, not vice-versa." This is the single most important CH3
  guard and the 1F ledger gets it right.
- **REDRESS-53 parallel index:** AP17-002 + PC17-001 catalogue the live
  `OnceCell<StructuralIndex>` (`crates/core/src/grammar/generated/json.rs:686,702,732`)
  as a retained scan cache that, under the fold, MUST become the tape's
  `offsets` (index IS the tape, Lock 1) or `local_temp_only` — "never a
  retained index parallel to a wired tape." No re-opening.

The Direction-Monotonicity Note (1f-past-corpora `:60-69`) is the explicit
CH3 regression guard and is correct.

## §3 — Admitted-REDRESS-Row Mis-Catalogue Scan

Verdict: **no admitted REDRESS row is mis-catalogued as unimplemented.**
The "unimplemented" verdicts in 1f-coherence attach to the SKINNY-PROVEN
generalizations (SoA `Tape`, lazy `ValueRef<G>`, BackendRule-walk
projection) that are ABSENT in crates/core — which is the correct sense of
"unimplemented in the fold target." The eager builders / AoS `TapeRec` /
`StructLayout` name are catalogued as PRESENT-in-core fold targets, not as
"missing." COH17-003 correctly labels the UNWIRED tape "spec-claims-
implemented (UNWIRED confirmed as stated)" — the §0.1.11 claim that the
tape exists-but-unwired is verified true, not mislabelled. Clean.

## §4 — Dispositions

### ACCEPT (6)

| ID | Section | Basis |
|---|---|---|
| CH3-A1 | 1f-past-corpora PC17-006 (D6 inversion guard) | The forbidden-in-skinny vs fold-target-in-core distinction is exactly right; SPEC `:807-811` verified; the monotonic-direction guard prevents the worst CH3 failure (relocating core substrate into skinny). |
| CH3-A2 | 1f-past-corpora Direction-Monotonicity Note `:60-69` | Correct CH3 regression guard; SPEC `:110-114` verified to say "adopt…in SK-V18", direction skinny→totality. |
| CH3-A3 | 1f-coherence COH17-004 + PC17-004 (x86/CollapsedStage not target) | x86 path correctly classified as non-target, not re-derived; SPEC `:806`,`:854` + alphaC `:307-316` verified; scope deferral to T-P2 is the correct non-disposition. |
| CH3-A4 | 1f-anti-pattern AP17-002 (OnceCell index → REDRESS-53) | The retained-index → REDRESS-53 binding is correct; the "index IS the tape OR local_temp_only" fork matches SPEC `:839`; no revival. |
| CH3-A5 | 1f-coherence COH17-007 (FactStream UNKNOWN) | Correctly NOT catalogued as a contradiction; Lock 1 `admitted_fact_output` (LOCKS `:100-116`) vs SK-V17 CSS-admission-bar are reconcilable; UNKNOWN + verify_action is the right posture (no premature mis-catalogue, no re-opening). |
| CH3-A6 | 1f-coherence COH17-003 (UNWIRED tape "spec-claims-implemented") | The admitted-as-stated UNWIRED row is correctly NOT flagged unimplemented; matches §3 CH3 "no admitted row mis-catalogued." |

### REVISE (3)

| ID | Section / locus | Defect | Concrete fix |
|---|---|---|---|
| **CH3-R1** | 1f-past-corpora "Do-Not-Redrive Ledger" (`restart/audit/totality/sk-v17/p1/1f-past-corpora.md:51-58`); also absent from `1f-anti-pattern.md:53-59` and `1f-coherence-scan.md:65-74` | **The StructRegistry / Arena<G> / Builder<G> hot-path-indirection pre-block is wholly absent from all three 1F artefacts.** This pre-block is named verbatim in the V2 charge AND sits at SPEC `:794-795` ("StructRegistry / Arena<G> / Builder<G> hot-path indirection (28-65× bbnf/sheets, 983× css bootstrap, 10583× WATCHDOG tailwind). No registry lookup in the per-leaf hot path."), AND is a W1 load-bearing row (SPEC `:824` "StructRegistry indirection"). It is verified live in the fold target: `crates/ir/src/registry/struct.rs:313 pub struct StructRegistry` and the tape's `begin_compound(&StructLayout)` (`crates/core/src/runtime/tape/mod.rs:185`) sources the layout from that registry. `grep StructRegistry` over `restart/audit/totality/sk-v17/p1/` returns ZERO. The do-not-redrive ledger thereby fails its §3 CH3 "pre-block list correctly identified" charge for one of the four named pre-blocks. | Add a ledger row: "StructRegistry / Arena<G> / Builder<G> hot-path indirection | SPEC `:794-795`,`:824`; live fold-target `crates/ir/src/registry/struct.rs:313`, consumed by `crates/core/src/runtime/tape/mod.rs:185 begin_compound(&StructLayout)` | the fold's `BackendRule`/`FieldSource` projection walk must resolve layout ONCE per compound (or hoist to codegen), with NO `StructRegistry` lookup in the per-leaf hot path; re-introducing per-leaf registry indirection re-opens the 28-65×/983×/10583× regression." |
| **CH3-R2** | 1f-coherence Cross-Tree Substrate Map, "Value API" row (`restart/audit/totality/sk-v17/p1/1f-coherence-scan.md:82`) and the "Layout shape" row (`:83`, "FieldSource IS the BackendRule-walk recipe") | The fold-reconciliation cell prescribes "generate one `BackendRule`/`FieldSource`-walking lazy projection" with **no constraint that the walk avoid per-leaf StructRegistry indirection.** Because crates/core's `FieldSource` enum lives inside the live `StructRegistry` (`crates/ir/src/registry/struct.rs:84` `FieldSource`, `:313` `StructRegistry`; `mod.rs:39` re-exports both), a naive "FieldSource-walking" generator that dereferences the registry per leaf is exactly the pre-blocked indirection (SPEC `:794-795`). The excavation surfaces the generalization without fencing it against the regression it abuts. | Annotate both rows with the constraint from CH3-R1: the `FieldSource` walk is a COMPILE-TIME projection-emission recipe (the layout is resolved once and the walk is unrolled into the emitted projection code), NOT a runtime per-leaf registry traversal. Cite SPEC `:794-795` as the binding fence on the COH17-002 fold. |
| **CH3-R3** | Subject-set completeness (PASS-1-EXCAVATION §3 CH3: "the rejected-route pre-block list is correctly identified by **1D and 1E**") | The canonical pre-block identifiers (1D skinny-lessons, 1E locks-evidence) do not exist under `restart/audit/totality/sk-v17/p1/`; only the 1F triad is present. CH3's charge presumes 1D/1E carry the pre-block ledger; with them absent, the burden falls to 1f-past-corpora alone, and CH3-R1 shows it has a hole. This is a structural gap, not a falsehood — but it blocks the §3Z convergence accounting (the pre-block identification cannot be cross-checked against 1D/1E). | Either (a) author the missing `sk-v17/p1/{1d-skinny-lessons,1e-locks-evidence}.md` so the pre-block ledger is cross-identified per §3 CH3, OR (b) if 1F is by design the sole pre-block identifier for this tranche, record that scoping decision in the CONSOLIDATED verdict and fold CH3-R1's missing row so the single ledger is complete. Until one holds, CH3 cannot certify the pre-block list "correctly identified." |

### REJECT (0)

No section re-opens a pre-blocked route, mis-identifies a resolved pre-block,
or mis-catalogues an admitted REDRESS row. Nothing rises to REJECT.

## §5 — Continuity + Cross-Reference Audit

The prior-totality continuity claim (1f-past-corpora `:71-79`) cites SK-V14
1F COH-014 root-OnceCell coupling at `restart/audit/totality/p1/1F-coherence-scan.md:87`
and COH-008 BackendShape depth at `:81`. Both verified: `:87` is COH-014
(root `OnceCell<StructuralIndex>`), `:81` is COH-008 (five-shape canon
depth). The prior file is `cycle: V4` (frontmatter verified). Re-anchoring
to current crates/core line positions (AP17-002, COH17-004) is sound — no
SK-V14 finding re-derived without a current re-anchor. ACCEPT (folded into
CH3-A4 basis).

## §6 — Verdict

CH3 ACCEPT-rate on this subject set: **6 ACCEPT / 9 dispositioned = 67%**,
with **3 REVISE / 0 REJECT**. Below the §3Z ≥95% bar; the pass does not
converge this cycle. The dominant defect is CH3-R1/R2: the **StructRegistry
hot-path-indirection pre-block** — one of the four pre-blocks the V2 charge
names load-bearing — is missing from the do-not-redrive ledger and unfenced
on the COH17-002 value-API fold, despite being live in the fold target
(`crates/ir/src/registry/struct.rs:313`) and abutting the very
`FieldSource`-walk generalization the excavation proposes. CH3-R3 records
that the canonical 1D/1E pre-block identifiers are absent under `sk-v17/`,
leaving the single 1F ledger to carry §3 CH3's identification charge alone
— and it has the CH3-R1 hole. No route re-opened; no admitted REDRESS row
mis-catalogued; citation floor sound. Fold all three REVISE into V3.
