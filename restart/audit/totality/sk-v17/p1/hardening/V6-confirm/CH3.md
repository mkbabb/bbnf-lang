---
lens: CH3-REGRESSION
pass: T-P1-excavation
cycle: V6-confirm
generated_at: 2026-05-29T00:00:00Z
role: confirming-challenge
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
prior_challenge: restart/audit/totality/sk-v17/p1/hardening/V5/CH3.md (8 ACCEPT / 0 REVISE / 0 REJECT, 100%)
master_head: 445925167
verification_method: "Re-read the V5-folded CH3.md + the V5 CONSOLIDATED + all eight 1X artefacts; live re-resolution via sed -n / grep -n / grep -rn at master HEAD 445925167 of: SPEC.md (:110-114, :258, :481-483, :789-797, :806, :807-811, :824-826, :839, :852-854 — all line-exact; REDRESS-53 grep = :577/:657/:825/:839); crates/ir/src/registry/struct.rs (FieldSource:84, StructLayout:202, StructRegistry:313, layouts BTreeMap:314, insert:326, layout():331, layout_by_name:337); crates/core/src/runtime/tape/{mod.rs begin_compound:185 + layout.rule_id & 0x1F:186, record.rs TapeRec:103 #[repr(C,align(4)]:102}; crates/core/src/runtime/css_l4/{value.rs CssTypedValue:414, builder.rs pending_value:Option:71 + six pending_ Vecs:74-79}; crates/simd-scan/src/{lib.rs scan_structural:80, alphabet.rs KernelShape:116 select:118}; crates/core/src/grammar/generated/css_l4.rs:15976-15982 scan_structural wire; GRAMMAR_STRUCTURAL_ALPHABET grep-count 3-each across 8 grammars + 0 in math.rs; scan_structural( grep-count 1-each across 8 + 0 in math.rs; grep -rn 'enum BackendShape' crates/ = ZERO (exit 1); grep -rn derive_backend_shape crates/ = ZERO; skinny BackendShape ir/lib.rs:340, derive_backend_shape passes/lib.rs:392, select_classifier dispatch.rs:42, Tape tape/mod.rs:94, ValueRef:175, value_from_ref json/value.rs:143; revival-verb grep over 1[a-f]*.md (re-introduce/revive/reopen/add StructRegistry/new substrate/sixth BackendShape/adopt x86/relocate-into-skinny) = only guarded fences; paper-close grep (now resolved/fully wired/no longer a divergence/now wired) = ZERO; :807-as-x86-bar grep = only the two corrected SUB17-005 cells; no cargo/build/edit mutation"
disposition_counts:
  accept: 8
  revise: 0
  reject: 0
prior_cycle_dispositions_status:
  V5-CH3-all-ACCEPT-100pct: HELD (re-verified live, not on author assertion; no orphan REVISE carried into V6-confirm)
---

## §0 — Lens Charge (confirming cycle)

This is the **confirming CH3 REGRESSION re-review** of the V5-folded SK-V17
T-P1 excavation artefacts. The prior climb ran V2 61.9% → V3 85.3% → V4
93.3% → V5 98.7% (V1 was a VOID infrastructure cycle, 0 CH files, not a
disposition cycle). V5 CH3 returned 8 ACCEPT / 0 REVISE / 0 REJECT (100%),
its second consecutive above-bar cycle after V4. The discipline of a
confirming cycle: **ACCEPT where the V5-folded artefact is correct +
complete; flag only genuine residual defects** as REVISE/REJECT.

CH3 scans the three §3-CH3 failure modes: (1) an inventory **re-opens a
route already pre-blocked** in the SK-V17 corpus; (2) the **rejected-route
pre-block list is correctly identified** (§3 CH3 names **1D and 1E** the
canonical identifiers); (3) an **admitted REDRESS row is mis-catalogued as
unimplemented**. The REDRESS pre-blocks the fold must respect, per the
charge: **AZ-IV eager** (SPEC `:791`), **StructRegistry indirection**
(`:793-795`), **fact-stream** (`:796`), **x86** (`:806`), **lo6** (`:826`/
`:854`), **D6 second substrate** (`:807-811`/`:854`). The monotonic core-
vs-skinny direction must be skinny-proven → crates/core (SPEC `:110-114`):
crates/core is the SK-V18 fold TARGET, skinny/crates the proven engine.

SUBJECT-SET COMPLETENESS. All eight 1A–1F artefacts are present at `cycle:
V5` (frontmatter verified live for all eight via `grep -l 'cycle: V5'`).
The canonical pre-block identifiers **1D** (`1d-skinny-lessons.md`) and
**1E** (`1e-locks-evidence.md`) both carry a Do-Not-Redrive ledger (1D
"Past-Corpora Do-Not-Redrive Ledger (CH3 REGRESSION + CH3-R1 fence)"
`:113-128`; 1E "CH3 Do-Not-Redrive Ledger" `:133-145`). §3-CH3's "1D and 1E
correctly identify the list" charge discharges against the canonical
artefacts.

## §1 — Pre-Block Citation Resolution (re-verified line-exact)

Every load-bearing §9 pre-block citation re-resolved against
`restart/skinny/tranches/sk-v17/SPEC.md` at master HEAD `445925167`. No
drift from the V5-verified anchor set:

| Pre-block | SPEC anchor | Verified live | Resolves |
|---|---|---|---|
| AZ-IV eager value tree (118×) | `:791` | "**AZ-IV eager value tree** (the 118× regression): eager per-leaf payload, f64-alloc-per-number, per-color `Box<CssColor>`. Materialization stays lazy-by-default." | YES |
| StructRegistry / Arena<G> / Builder<G> indirection | `:793-795` | "**StructRegistry / Arena<G> / Builder<G> hot-path indirection** (28-65× bbnf/sheets, 983× css bootstrap, 10583× WATCHDOG tailwind). No registry lookup in the per-leaf hot path." | YES |
| CSS fact-stream String (admission plane) | `:796` | "**CSS fact-stream String** as a live admission plane (…): diagnostic-only." | YES |
| x86 / AVX-512 / SVE | `:806` | "**x86 / AVX-512 / SVE:** aarch64 only (Apple cores have no SVE)." | YES |
| Second substrate (D6) | `:807-811` | "**Second substrate:** skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor`, public `UnionTape`, … sixth `BackendShape`, retained sidecars …" | YES |
| W1 attribution (StructRegistry indirection) | `:824` | "W1 \| AZ-IV eager; … StructRegistry indirection; 24-row broadcast; second substrate; REDRESS 50-55, 60-72" | YES |
| W1-context restatement | `:481-483` | "Pre-blocked routes: AZ-IV eager value tree; the fact-stream String …; StructRegistry/Arena<G> hot-path indirection; … a second substrate" | YES |
| REDRESS-53 (W2 row / shortlist cond 1) | `:825` / `:839` | `:825` "L1/L4 index as parallel retained vector (REDRESS-53)"; `:839` "A retained parallel index collapses into REDRESS-53." (grep also `:577`/`:657`) | YES |
| §9 REJECTed set (x86 collapsed-stage / lo6 / D6) | `:852-854` | "asmjson collapsed-stage FSM (x86, host-blocked) · lo6 `classify_tbl4` on the CSS alphabet · D6 second substrate." | YES |
| aarch64 primary x86 bar | `:258` | "**aarch64 only.** No x86, no AVX-512, no SVE." | YES |
| Monotonic skinny→totality | `:110-114` | item 11 "**Foldable into TOTALITY.** … the TOTALITY tree (`crates/core/src/runtime/tape/`) can adopt them in SK-V18." | YES |

The StructRegistry-fence live anchors re-resolve exact: `struct.rs:84`
`pub enum FieldSource`, `:202` `pub struct StructLayout`, `:313` `pub
struct StructRegistry`, `:314` `layouts: BTreeMap<RuleId, StructLayout>`,
`:326` `pub fn insert`, `:331` `pub fn layout(&self, rule_id: RuleId) ->
Option<&StructLayout>`, `:337` `pub fn layout_by_name`; consumer
`crates/core/src/runtime/tape/mod.rs:185` `fn begin_compound(&mut self,
layout: &StructLayout)` reading **only** `(layout.rule_id & 0x1F) as u8`
at `:186` — no internal registry lookup. The fence is anchored to live
code, not recalled.

The V3 CH3-R4 `:807`→`:806` defect remains closed: a tree-wide `grep -rn
:807` filtered for x86/AVX/SVE returns ONLY the corrected SUB17-005 body
cell at 1A `:96` and its frontmatter record at `:20` — both correctly
cite the x86 bar at `:806` and name `:807-811` as the *separate* second-
substrate block. No new mis-anchor surfaced under this line-exact
re-resolution. The citation floor is **sound with zero residual
off-by-one**.

## §2 — Re-Opened-Route Scan (no V5 inventory revives a pre-blocked route)

CH3's central firewall. Verdict: **no inventory re-opens a pre-blocked
route.** A `grep -rni` over `sk-v17/p1/1[a-f]*.md` for revival verbs
(`re-introduce`, `revive`, `reopen`/`re-open`, `add StructRegistry`, `new
substrate`, `sixth BackendShape`/`6th shape`, `adopt x86`, `relocate …
into skinny`) returns ONLY guarded negations — every hit is a fence, a
do-not-redrive ledger row, or an explicit "No new substrate, BIR variant,
or shape is proposed" (1A `:66`, 1f-coherence `:71`) / "no 6th shape" /
"never relocates … INTO skinny" disclaimer. Each crates/core construct
naming a pre-blocked shape is catalogued as the fold-deletion / fold-fence
TARGET with the monotonic skinny→totality direction guarded (guard present
in 1A `:66`, 1D `L-SK17-07` `:128`, 1E ledger `:143`, 1f-coherence `:71`,
1f-past `:63`).

- **AZ-IV eager (`:791`):** live eager `CssTypedValue<'p>` enum
  (`crates/core/src/runtime/css_l4/value.rs:414`, verified `pub enum
  CssTypedValue<'p>`) + `pending_value: Option<CssTypedValue<'p>>`
  (`builder.rs:71`) + six `pending_rules/decls/selectors/values/blocks/
  components` Vecs (`:74-79`) catalogued by 1D `SK17L-003` (`:90`) +
  ledger `L-SK17-01` (`:121`), 1E ledger `:142`, 1f-past as the eager-
  value-tree fold-DELETION target — "replace with lazy `ValueRef<G>`, do
  not carry forward." Lazy projection catalogued unimplemented; eager tree
  is the deletion target. No revival. The six-Vec + `pending_value:Option`
  count is correct against live `builder.rs:71-79` (the V2 nine-Vec
  overcount stays folded).
- **StructRegistry indirection (`:793-795`):** catalogued as the fold-
  FENCE target across 1D `SK17L-004` (`:91`) + ledger `L-SK17-02`/
  `L-SK17-02b` (`:122-123`), 1E `D-1E-SKV17-03` (`:128`) + ledger `:140` +
  `LAC-1E-SKV17-02` (`:179`), 1C `RT17-006` (`:80`) + `U-RT17-003` (`:121`),
  1A `:120`,`:133`, 1f-anti-pattern `:73`, 1f-coherence `:93`. The fence —
  "no per-leaf `StructRegistry::layout(rule_id)` lookup; the `FieldSource`
  walk is COMPILE-TIME projection-emission resolved once" — matches the
  live shape: `begin_compound` takes a pre-resolved `&StructLayout` and
  reads only `layout.rule_id & 0x1F` (`:186`), no internal lookup. Fenced,
  not re-opened.
- **fact-stream (`:796`):** 1D `L-SK17-03` (`:124`) catalogues the CSS
  fact-stream String PERMANENT-PRE-BLOCK as an admission plane, with the
  V1 Lock-1 `FactStream` category (`LOCKS.md:100-116`) reconciled (1D
  `SK17L-005` `:92`, 1E `L01`/`:142`) as surviving only for typed-schema
  output planes — NOT a contradiction, NOT a revival of the String CSS
  admission plane. `W5C_REQUEST_FACT_PROFILES` (`codegen/src/lib.rs:336`)
  catalogued RETIRE-to-diagnostic.
- **x86 (`:806`) + lo6 + aarch64 CollapsedStage:** 1A `SUB17-005` (`:96`,
  x86 bar correctly at `:806`, second-substrate block separate `:807-811`)
  / `SUB17-009` (`:133`), 1B `BSHAPE17-005` (`:73`,`:98`), 1C `RT17-007`,
  1D `SK17L-006` (`:93`) + ledger `L-SK17-06` (`:127`), 1E `L10`/
  `LAC-1E-SKV17-05` (`:118`,`:182`), 1f-coherence `COH17-004` (`:81`,
  `:105`), 1f-past all classify §7.3 CollapsedStage as x86/AVX-512-pinned
  and the aarch64 candidate as the SPEC-named **UNKNOWN-2D-05**
  (`ARCHITECTURE.md:1206`) — NOT a fresh gap, NOT an aarch64-CollapsedStage
  re-derivation. The lo6 `classify_tbl4`-on-CSS-alphabet pre-block resolves
  at SPEC `:826`/`:854` (1f-coherence `:81`). The 5-shape Lock-10 canon
  (`LOCKS.md:107-108`) holds; NEON sits under the four LLVM shapes' scan-
  leaf FFI; no 6th shape proposed. The multi-arch `crates/simd-scan`
  (neon/avx2/avx512/wasm/scalar) is catalogued impl-exceeds-spec scope
  pressure deferred to T-P2 (1E `D-1E-SKV17-06` `:131`, `LAC-1E-SKV17-06`
  `:183`), NOT an x86 admission.
- **D6 second substrate (`:807-811`):** the single most important CH3
  inversion guard. The §9 block names skinny `StructLayout`/
  `TapeStructBuilder`/`TapeCursor` as FORBIDDEN-IN-SKINNY — precisely the
  crates/core fold-target names. 1D `L-SK17-07` (`:128`), 1E ledger `:143`,
  1f-past `:63` all correctly state the fold "adopts the PROVEN skinny
  `Tape`/`ValueRef` INTO crates/core, never relocates crates/core
  `TapeStructBuilder`/`TapeCursor` INTO skinny" (monotonic skinny→totality,
  SPEC `:110-114`, verified). No inversion. §9 REJECTed D6 resolves exact
  at `:854`.

**V5 DELTA (a) — 1B BSHAPE17-006 anchor fix (`:74`)** [re-confirmed]. The
cell reads "the lookup method is `StructRegistry::layout(rule_id)` (`:331`)
/ `layout_by_name` (`:337`)" with the note "**V5 anchor fix:** the lookup
symbol is `layout`/`layout_by_name` (`:331`/`:337`), not `lookup` (V4
mis-cited the struct-decl line `:313`)." Re-verified line-exact: `:331`
`pub fn layout(&self, rule_id: RuleId) -> Option<&StructLayout>`, `:337`
`pub fn layout_by_name`, `:314` `layouts: BTreeMap<RuleId, StructLayout>`,
`:326` `insert`, `:313` the struct decl. A citation-floor improvement on a
load-bearing pre-block; no verdict change, no route re-opened.

**V5 DELTA (b) — 1B "NEW V5" NEON shared-classifier claim (`:59-63`)**
[re-confirmed]. The cell asserts the NEON shared classifier
`select_classifier(alphabet)` is "ALREADY grammar-general in core
(`simd_scan::scan_structural(input,&StructuralAlphabet)` lib.rs:80;
`KernelShape::select` alphabet.rs:118; all 8 generated grammars emit
`GRAMMAR_STRUCTURAL_ALPHABET`) — NOT JSON-only; impl-exceeds-spec. **No new
shape, directive, or substrate.**" Re-verified live: `scan_structural` at
`crates/simd-scan/src/lib.rs:80`; `KernelShape::select(alphabet)` at
`alphabet.rs:118` under `impl KernelShape` (`:116`); `select_classifier`
at skinny `bbnf-simd/src/dispatch.rs:42`. The "all 8 grammars emit
`GRAMMAR_STRUCTURAL_ALPHABET`" claim is **precisely true** — grep-count 3
each across json/ebnf/bnf/csv/css_l4/css_pretty/google_sheets/bbnf, 0 in
math.rs; `scan_structural(` grep-count 1 each across the 8, 0 in math.rs.
A grammar-generality observation, explicitly disclaiming any new shape/
directive/substrate and explicitly aarch64-NEON. From a CH3 view it
neither re-opens the x86 (`:806`) nor D6 (`:807-811`) pre-block nor
proposes a 6th shape. Clean.

The AoS-vs-SoA tape divergence (core 16-byte `TapeRec` `record.rs:103`,
`#[repr(C,align(4))]` `:102`, verified; vs skinny SoA `Tape<'input>`
`tape/mod.rs:94`, verified) is correctly catalogued as a fold-convergence
question (1C `RT17-001`, 1E `L01`/`D-1E-SKV17-01` `:116`/`:126`, 1D
`SK17L-001` `:88`), NOT a same-tree parallel substrate and NOT a re-opened
route — both encodings are Lock-1-admitted offset tapes.

## §3 — Admitted-REDRESS-Row Mis-Catalogue Scan

Verdict: **no admitted REDRESS row is mis-catalogued as unimplemented.**
The "unimplemented" verdicts across 1A–1F attach to the SKINNY-PROVEN
generalizations (SoA `Tape`, lazy `ValueRef<G>`, the `BackendRule`/
`FieldSource`-walk projection) ABSENT in crates/core — the correct sense of
"unimplemented in the fold target." `grep -rn 'enum BackendShape' crates/`
= ZERO (exit 1); `grep -rn derive_backend_shape crates/` = ZERO — both
confirming BSHAPE17-001's "unimplemented" verdict for the 5-shape selector
is a true absence in the fold target (the enum lives only at skinny
`ir/src/lib.rs:340`, the selector at skinny `passes/src/lib.rs:392`), not a
pre-block mis-flip.

The pre-blocked shapes (eager builder, AoS `TapeRec`, `StructRegistry`/
`StructLayout` indirection) are catalogued as PRESENT fold-targets, not as
"missing." The UNWIRED tape is correctly labelled spec-claims-implemented
(1C `RT17-003` `:77` "spec-claims-implemented (UNWIRED confirmed as
stated)") — the §0.1.11 exists-but-unwired claim is verified true
(`TapeStructBuilder` grep-zero outside `runtime/tape/`; live JSON uses
`JsonStructBuilder::new()` at `json/parse_with.rs:11,34`, live CSS
`CssStructBuilder::new()`), not mislabelled.

The core CSS structural scan is correctly catalogued WIRED (1C `RT17-003`
`:77` + 1D `SK17L-008` `:95`): `crates/core/src/grammar/generated/
css_l4.rs:15982` calls `::simd_scan::scan_structural(input, &alphabet)`
over a `StructuralAlphabet` built from `GRAMMAR_STRUCTURAL_ALPHABET`
(verified `:15976-15982`); all 8 generated grammars carry `scan_structural`
+ `OnceCell<StructuralIndex>`, `math.rs` holds neither (scan-count 0,
alphabet-count 0) → 8 wired, not 9. The residual gap is correctly
re-classified as the absent tape CONSUMER, not the scan. The retained
`OnceCell<StructuralIndex>` (8 carriers) is correctly bound to REDRESS-53
(1C `RT17-005` `:79`,`:102`, SPEC `:825`,`:839`) with the index-IS-tape-or-
`local_temp_only` fork, NOT catalogued as a permissible parallel substrate.
A `grep -rni` for self-reported "now resolved"/"fully wired"/"no longer a
divergence"/"now wired" returns ZERO across all 1X inventories — no paper-
close self-report.

## §4 — Dispositions

### ACCEPT (8)

| ID | Section | Basis |
|---|---|---|
| CH3-A1 | 1B BSHAPE17-006 V5 anchor fix (`:74`) + 1A StructRegistry fence (`:120`,`:133`) | The V5 re-anchor of the StructRegistry lookup symbol to `layout` (`:331`)/`layout_by_name` (`:337`) re-verified line-exact against `crates/ir/src/registry/struct.rs`; `:314` BTreeMap, `:326` insert, `:313` struct decl confirmed. Fence ("registry lookup compile-time, runtime carries resolved `&StructLayout`") unchanged; `begin_compound` reads only `layout.rule_id & 0x1F` (`mod.rs:186`). Correct + complete; no route re-opened. |
| CH3-A2 | 1B "NEW V5" NEON shared-classifier claim (`:59-63`) | `select_classifier`/`scan_structural`/`KernelShape::select` re-verified at `dispatch.rs:42`/`simd-scan/lib.rs:80`/`alphabet.rs:118`; the "all 8 grammars emit GRAMMAR_STRUCTURAL_ALPHABET" sub-claim verified exact (grep 3-each ×8, 0 in math.rs). Correct grammar-general reading (impl-exceeds-spec, aarch64-NEON), disclaiming any new shape/directive/substrate; re-opens neither x86 (`:806`) nor D6 (`:807-811`). Clean. |
| CH3-A3 | 1D `L-SK17-02`/`L-SK17-02b` + 1E `D-1E-SKV17-03`/ledger `:140` + 1C `RT17-006` (StructRegistry fence) | The 28-65×/983×/10583× pre-block is in both canonical 1D/1E ledgers with the no-per-leaf-lookup fence; SPEC `:793-795`,`:824` verified exact; `begin_compound:185` reading only `layout.rule_id & 0x1F` (`:186`) verified — fence matches live code. No re-opening. |
| CH3-A4 | 1D `L-SK17-02b` + 1E `LAC-1E-SKV17-02` (`:179`) + 1f-anti-pattern `:73` (FieldSource compile-time) | The `FieldSource{TypedLeaf,BranchTag,SeqPosition,RepeatElement,RuleReference}` walk fenced "COMPILE-TIME projection-emission resolved once … NOT a per-leaf StructRegistry indirection"; `FieldSource` verified `struct.rs:84`; SPEC `:794-795` anchored; the generalization is fenced against the regression it abuts. |
| CH3-A5 | 1D `L-SK17-07` (`:128`) + 1E ledger `:143` + 1f-past `:63` (D6 inversion guard) | FORBIDDEN-in-skinny vs fold-target-in-core distinction exactly right; SPEC `:807-811` verified to name `StructLayout`/`TapeStructBuilder`/`TapeCursor`; monotonic direction `:110-114` verified; §9 REJECTed D6 `:854` verified; prevents the worst CH3 failure (relocating core substrate into skinny). |
| CH3-A6 | 1A `SUB17-005` (`:96`) + 1B `BSHAPE17-005` (`:73`,`:98`) + 1D `L-SK17-06` (`:127`) + 1E `L10`/`LAC-1E-SKV17-05` (`:118`,`:182`) + 1f-coherence `COH17-004` (x86/lo6/CollapsedStage not target) | x86 path classified non-target (x86 bar SPEC `:806`, second-substrate block separate `:807-811`), lo6 + aarch64 CollapsedStage classified the SPEC-named UNKNOWN-2D-05 / §9-REJECTed not fresh gaps; SPEC `:258`,`:806`,`:826`,`:852-854` + `ARCHITECTURE.md:1206` verified; 5-shape canon held (Lock 10 `:107-108`); no 6th shape; `enum BackendShape` + `derive_backend_shape` grep-zero in `crates/` confirms the unbuilt-in-core verdict. |
| CH3-A7 | 1D `L-SK17-01` (`:121`) + 1E ledger `:142` (AZ-IV eager fold-deletion) | Eager `CssTypedValue` (`value.rs:414`) + `pending_value:Option` (`builder.rs:71`) + six pending Vecs (`:74-79`) catalogued as deletion target with lazy-`ValueRef<G>` replacement, not carried forward; live anchors verified. No revival. |
| CH3-A8 | 1C `RT17-005` (`:79`,`:102`) REDRESS-53 anchor + tape-unwired/scan-wired split (`RT17-003` `:77`) | Retained `OnceCell<StructuralIndex>` (8 carriers) correctly bound to REDRESS-53 at SPEC `:825`/`:839` with the index-IS-tape-or-`local_temp_only` fork; UNWIRED tape correctly labelled spec-claims-implemented; scan correctly WIRED (`css_l4.rs:15982`); no admitted row mis-flipped to unimplemented. |

### REVISE (0)

No V5 inventory carries an unfenced pre-block, a mis-identified resolved
pre-block, an off-by-one §9 anchor, or an admitted REDRESS row mis-
catalogued as unimplemented. The two V5 inventory deltas (1B BSHAPE17-006
anchor fix; 1B NEW-V5 NEON classifier claim) are both re-verified clean
from a regression view. No new defect surfaced under this confirming
cycle's line-exact re-resolution.

### REJECT (0)

No V5 inventory re-opens a pre-blocked route, mis-identifies a resolved
pre-block, mis-catalogues an admitted REDRESS row as unimplemented, or
inverts the monotonic direction. Nothing rises to REJECT.

## §5 — Continuity + Convergence Accounting

V5 CH3 returned 8 ACCEPT / 0 REVISE / 0 REJECT (100%); this confirming
cycle returns **8 ACCEPT / 0 REVISE / 0 REJECT** — ACCEPT-rate **8/8 =
100%**, at/above the §3Z ≥95% bar. Every load-bearing anchor was re-
resolved live at master HEAD `445925167`, not taken on the V5 author's
frontmatter. The subject set is complete (1A–1F all at `cycle: V5`).
Against §3Z, CH3 now holds **three consecutive above-bar cycles** (V4 100%,
V5 100%, V6-confirm 100%); the formal 2nd-consecutive ≥95% the orchestrator
sought for §3Z is satisfied for this lens. No CH3 finding blocks
convergence.

## §6 — Verdict

CH3 confirming ACCEPT-rate on the V5-folded subject set: **8 ACCEPT / 8
dispositioned = 100%**, with **0 REVISE / 0 REJECT**. The re-opened-route
firewall is clean: no inventory revives AZ-IV eager (`:791`), StructRegistry
indirection (`:793-795`), fact-stream (`:796`), x86 (`:806`), lo6
(`:826`/`:854`), or the D6 second substrate (`:807-811`); every crates/core
construct is the SK-V18 fold TARGET with the monotonic skinny→totality
direction guarded (`:110-114`); the D6 inversion trap is correctly fenced;
the StructRegistry/FieldSource fence matches the live `begin_compound
(&StructLayout)` no-per-leaf-lookup shape (`mod.rs:185-186`). The two V5
inventory deltas re-confirm clean — the 1B BSHAPE17-006 anchor fix
(`layout`/`layout_by_name` at `:331`/`:337`) is a citation-floor improvement
on a load-bearing pre-block, and the 1B NEW-V5 grammar-general NEON
`select_classifier` claim is verified exact (8-grammar
`GRAMMAR_STRUCTURAL_ALPHABET` emission) and disclaims any new shape/
directive/substrate. The pre-block list is correctly identified in both
canonical 1D/1E ledgers (§3-CH3 charge discharged). No admitted REDRESS row
is mis-catalogued as unimplemented; the UNWIRED tape is correctly labelled
spec-claims-implemented and the core CSS scan correctly WIRED across all 8
grammars. The V5-folded excavation is correct + complete from the CH3
REGRESSION lens; this confirming cycle ACCEPTs in full, securing the formal
2nd-consecutive ≥95% for §3Z.
