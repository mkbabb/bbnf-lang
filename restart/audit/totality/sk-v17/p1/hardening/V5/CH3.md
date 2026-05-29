---
lens: CH3-REGRESSION
pass: T-P1-excavation
cycle: V5
generated_at: 2026-05-29T26:10:00Z
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
verification_method: "Read over all eight V5 1X artefacts + prior V4 CH3.md; sed -n + grep -n/-rn over restart/skinny/tranches/sk-v17/SPEC.md (anchors verified live: :110-114, :258, :481-483, :791, :793-795, :796, :806, :807-811, :824, :825, :839, :852-854); restart/locks/LOCKS.md (:75, :100-116, :107-108, :160); restart/ARCHITECTURE.md (:1088, :1206); grep -n over crates/ir/src/registry/struct.rs (FieldSource:84, StructLayout:202, StructRegistry:313, layouts BTreeMap:314, insert:326, layout():331, layout_by_name:337), crates/core/src/runtime/tape/{mod.rs (begin_compound:185, layout.rule_id & 0x1F:186), record.rs (TapeRec:103)}, crates/core/src/runtime/css_l4/{builder.rs:71-79, value.rs (CssTypedValue:414)}, crates/simd-scan/src/{lib.rs (scan_structural:80), alphabet.rs (KernelShape:103, select:118)}, crates/core/src/grammar/generated/css_l4.rs:15982, skinny/crates/{ir/src/lib.rs (BackendShape:340), passes/src/lib.rs (derive_backend_shape:392), runtime/src/tape/mod.rs (Tape:94), runtime/src/grammars/json/value.rs (value_from_ref:143), bbnf-simd/src/dispatch.rs (select_classifier:42)}; grep -rn 'enum BackendShape' crates/ = ZERO (exit 1); StructLayout refs across crates/ = 960; grep -rn over sk-v17/p1/1[a-f]*.md for revival verbs + ':807'-as-x86-bar + self-reported 'resolved/wired' without evidence; no cargo/build mutation"
disposition_counts:
  accept: 8
  revise: 0
  reject: 0
prior_cycle_dispositions_status:
  V4-CH3-all-ACCEPT-100pct: HELD (no orphan REVISE carried; re-verified live, not on author assertion)
---

## §0 — Lens Charge

CH3 REGRESSION scans the SK-V17 T-P1 **V5** excavation for three failure
modes (PASS-1-EXCAVATION §3 CH3): (1) an inventory **re-opens a route
already pre-blocked** in the SK-V17 corpus (the `skinny/REDRESS.md`-derived
pre-block ledger crystallised into SPEC §9 `:789-857`, restated for the W1
context at `:481-483`); (2) the **rejected-route pre-block list is
correctly identified** — §3 CH3 names **1D and 1E** the canonical
identifiers; (3) an **admitted REDRESS row is mis-catalogued as
unimplemented**. The four load-bearing pre-blocks the fold must respect,
per the V5 charge: **AZ-IV eager** (SPEC `:791`), **StructRegistry
indirection** (`:793-795`), **fact-stream** (`:796`), **x86** (`:806`).
No revival implied: every crates/core construct excavated is the SK-V18
fold TARGET (totality tree), not a proposal; the monotonic direction is
skinny-proven → crates/core.

SUBJECT-SET COMPLETENESS. The full 1A–1F inventory set is present under
`restart/audit/totality/sk-v17/p1/` at `cycle: V5` — frontmatter `cycle:
V5` verified for all eight artefacts (1a/1b/1c/1d/1e + 1f-coherence/
1f-anti-pattern/1f-past). The canonical pre-block identifiers 1D
(`1d-skinny-lessons.md`) and 1E (`1e-locks-evidence.md`) both exist and
both carry a Do-Not-Redrive / pre-block ledger (1D "Past-Corpora
Do-Not-Redrive Ledger (CH3 REGRESSION + CH3-R1 fence)" `:113-128`; 1E "CH3
Do-Not-Redrive Ledger" `:133-138`). CH3 discharges its §3 CH3 "1D and 1E
correctly identify the list" charge against the canonical artefacts.

PRIOR-CYCLE FOLD VERIFICATION. V4 CH3 returned 8 ACCEPT / 0 REVISE / 0
REJECT (100%), carrying no orphan REVISE into V5. CH3 does not take that
clean on the author's frontmatter — it re-resolves every load-bearing
anchor line-exact this cycle. Two V5 inventory deltas touch the CH3
firewall surface and are dispositioned explicitly below: (a) **1B
BSHAPE17-006 V5 anchor fix** (the StructRegistry lookup symbol re-anchored
from the struct-decl line to `layout`/`layout_by_name`); (b) **1B "NEW V5"
NEON shared-classifier claim** (`select_classifier` grammar-general in
core). Both are verified clean from a regression view (§2).

## §1 — Pre-Block Citation Resolution (do the §9 cites resolve)

Every §9 pre-block citation made by the V5 inventories was resolved
against `restart/skinny/tranches/sk-v17/SPEC.md` by `sed -n` + `grep -n`.
The four load-bearing pre-blocks resolve to exact lines, identical to the
V4-verified anchor set (no drift):

| Pre-block | SPEC anchor (verified V5) | Verified line text | Resolves |
|---|---|---|---|
| AZ-IV eager value tree (118×) | `:791` | "**AZ-IV eager value tree** (the 118× regression): eager per-leaf payload, f64-alloc-per-number, per-color `Box<CssColor>`. Materialization stays lazy-by-default." | YES |
| StructRegistry / Arena<G> / Builder<G> hot-path indirection | `:793-795` | "**StructRegistry / Arena<G> / Builder<G> hot-path indirection** (28-65× bbnf/sheets, 983× css bootstrap, 10583× WATCHDOG tailwind). No registry lookup in the per-leaf hot path." | YES |
| CSS fact-stream String (admission plane) | `:796` | "**CSS fact-stream String** as a live admission plane (`emit_fact_stream`/`emit_full_parse`/…): diagnostic-only." | YES |
| x86 / AVX-512 / SVE | `:806` | "**x86 / AVX-512 / SVE:** aarch64 only (Apple cores have no SVE)." | YES |
| Second substrate (D6) | `:807-811` | "**Second substrate:** skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor`, public `UnionTape`, … sixth `BackendShape`, retained sidecars …" | YES |
| W1 attribution (StructRegistry indirection) | `:824` | "W1 | AZ-IV eager; fact-stream as admission; W5C array …; StructRegistry indirection; 24-row broadcast; second substrate; REDRESS 50-55, 60-72" | YES |
| W1-context restatement | `:481-483` | "Pre-blocked routes: AZ-IV eager value tree; the fact-stream String …; StructRegistry/Arena<G> hot-path indirection; … a second substrate" | YES |
| REDRESS-53 (W2 row / shortlist cond 1) | `:825` / `:839` | `:825` "L1/L4 index as parallel retained vector (REDRESS-53)"; `:839` "A retained parallel index collapses into REDRESS-53." | YES |
| §9 REJECTed set (x86 collapsed-stage / lo6 / D6) | `:852-854` | "asmjson collapsed-stage FSM (x86, host-blocked) · lo6 `classify_tbl4` on the CSS alphabet · D6 second substrate." | YES |
| aarch64 primary x86 bar | `:258` | "aarch64 only. No x86, no AVX-512, no SVE." | YES |
| Monotonic skinny→totality direction | `:110-114` | item 11 "**Foldable into TOTALITY.** … the TOTALITY tree (`crates/core/src/runtime/tape/`) can adopt them in SK-V18." | YES |

The live-code anchors of the StructRegistry fence resolve exactly:
`crates/ir/src/registry/struct.rs:84` `pub enum FieldSource`, `:202`
`pub struct StructLayout`, `:313` `pub struct StructRegistry`, `:314`
`layouts: BTreeMap<RuleId, StructLayout>`, `:326` `pub fn insert`, `:331`
`pub fn layout(&self, rule_id: RuleId) -> Option<&StructLayout>`, `:337`
`pub fn layout_by_name`; consumer `crates/core/src/runtime/tape/mod.rs:185`
`fn begin_compound(&mut self, layout: &StructLayout)` reading **only**
`(layout.rule_id & 0x1F) as u8` at `:186` — confirming the fold inherits a
no-per-leaf-lookup `begin_compound`. The fence is anchored to real code,
not recalled.

No phantom REDRESS id; no recalled LOC. The citation floor is **sound with
zero residual off-by-one** — the V3 CH3-R4 `:807`→`:806` defect closed in
V4 remains closed (a tree-wide `grep -rn :807` filtered for x86/AVX/SVE
returns ONLY the corrected SUB17-005 cell at 1A `:96` and its frontmatter
record at `:20`), and no new mis-anchor surfaced under this cycle's
line-exact re-resolution.

## §2 — Re-Opened-Route Scan (does any V5 inventory revive a pre-blocked route)

CH3's central firewall. Verdict: **no inventory re-opens a pre-blocked
route.** A `grep -rni` over `sk-v17/p1/1[a-f]*.md` for revival verbs
(`re-introduce`, `revive`, `reopen`/`re-open`, `add StructRegistry`, `new
substrate`, `sixth BackendShape`/`6th shape`, `adopt x86`, `relocate …
into skinny`) returns only guarded negations — every hit is a fence, a
do-not-redrive ledger row, or an explicit "without a 6th shape" / "no new
shape, directive, or substrate" disclaimer. Each crates/core construct
that NAMES a pre-blocked shape is catalogued as the fold-deletion /
fold-fence TARGET, with the monotonic skinny→totality direction guarded
(guard present in 1A `:66`, 1D `L-SK17-07` `:128`, 1E ledger `:136`,
1f-coherence `:71`, 1f-past `:63,:69`):

- **AZ-IV eager (`:791`):** the live eager `CssTypedValue` enum
  (`crates/core/src/runtime/css_l4/value.rs:414`, verified `pub enum
  CssTypedValue<'p>`) + the six `pending_*` Vecs + `pending_value: Option`
  (`builder.rs:71-79`, verified `pending_value: Option<CssTypedValue<'p>>`
  at `:71`, `pending_rules/decls/selectors/values/blocks/components` at
  `:74-79`) is catalogued by 1D `SK17L-003` (`:101`) + ledger `L-SK17-01`
  (`:120`), 1E ledger `:135`, 1f-past as the eager-value-tree
  fold-DELETION target — "replace with lazy `ValueRef<G>`, do not carry
  forward." The lazy projection is catalogued unimplemented; the eager
  tree is the deletion target. No revival. (The V2 "nine pending Vecs"
  overcount remains correctly folded to six-Vec + `pending_value: Option`
  across 1D/1E and the live `builder.rs:71-79` text.)
- **StructRegistry indirection (`:793-795`):** catalogued as the
  fold-FENCE target across 1D `SK17L-004` (`:103`) + ledger `L-SK17-02`/
  `L-SK17-02b` (`:121-122`), 1E `D-1E-SKV17-03` (`:128`) + ledger `:137` +
  `LAC-1E-SKV17-02` (`:179`), 1C `RT17-006` (`:80`) + `U-RT17-003` (`:121`),
  1A `:120,:133`, 1f-anti-pattern `:73`. The fence — "no per-leaf
  `StructRegistry::layout(rule_id)` lookup in the hot path; the
  `FieldSource` walk is COMPILE-TIME projection-emission resolved once" —
  matches the live shape: `begin_compound` takes a pre-resolved
  `&StructLayout` and reads only `layout.rule_id & 0x1F` (`:186`),
  performing NO internal registry lookup. No per-leaf indirection
  proposed; the regression is fenced, not re-opened.
- **fact-stream (`:796`):** 1D `L-SK17-03` (`:123`) catalogues the CSS
  fact-stream String as PERMANENT-PRE-BLOCK as an admission plane, with the
  V1 Lock-1 `FactStream` category (`LOCKS.md:100-116`) reconciled (1E
  `:108`) as surviving only for typed-schema output planes — NOT a
  contradiction, NOT a revival of the String CSS admission plane. The live
  skinny `W5C_REQUEST_FACT_PROFILES` (`codegen/src/lib.rs:336`) is
  catalogued RETIRE-to-diagnostic, not extend.
- **x86 (`:806`) + aarch64 CollapsedStage:** 1A `SUB17-005` (`:96`,
  corrected anchor cites the x86 bar at SPEC `:806` with `:807-811` the
  separate second-substrate block) / `SUB17-009` (`:133`), 1B
  `BSHAPE17-005` (`:73,:98`), 1C `RT17-007`, 1D `SK17L-006` (`:138`) +
  ledger `L-SK17-06` (`:126`), 1E `L10` row `:118` + `LAC-1E-SKV17-05`
  (`:182`), 1f-coherence `COH17-004` (`:81,:105`), 1f-past `:62` all
  classify §7.3 CollapsedStage as x86/AVX-512-pinned and the aarch64
  candidate as the SPEC-named **UNKNOWN-2D-05** (`ARCHITECTURE.md:1206`),
  NOT a fresh gap and NOT an aarch64-CollapsedStage re-derivation. The
  5-shape Lock-10 canon (`LOCKS.md:107-108`) is held; NEON sits under the
  four LLVM shapes' scan-leaf FFI. No 6th shape proposed. The multi-arch
  `crates/simd-scan` (neon/avx2/avx512/wasm/scalar) is catalogued
  impl-exceeds-spec scope pressure deferred to T-P2 (1E `D-1E-SKV17-06`
  `:131`, 1f-past `:62`), NOT an x86 admission.
- **D6 second substrate (`:807-811`):** the single most important CH3
  inversion guard. The §9 block names skinny `StructLayout`/
  `TapeStructBuilder`/`TapeCursor` as FORBIDDEN-IN-SKINNY — precisely the
  crates/core fold-target names. 1D `L-SK17-07` (`:128`), 1E ledger `:136`,
  1f-past `:63` all correctly state the fold "adopts the PROVEN skinny
  `Tape`/`ValueRef` INTO crates/core, never relocates crates/core
  `TapeStructBuilder`/`TapeCursor` INTO skinny" (monotonic skinny→totality,
  SPEC `:110-114`, verified). No inversion. The §9 REJECTed set resolves
  exact at `:852-854` ("asmjson collapsed-stage FSM (x86, host-blocked) ·
  lo6 `classify_tbl4` on the CSS alphabet · D6 second substrate").

**V5 DELTA (a) — 1B BSHAPE17-006 anchor fix (`:74`).** The cell now reads
"the lookup method is `StructRegistry::layout(rule_id)` (`:331`) /
`layout_by_name` (`:337`)" and the note records "**V5 anchor fix:** the
lookup symbol is `layout`/`layout_by_name` (`:331`/`:337`), not `lookup`
(V4 mis-cited the struct-decl line `:313`)." Verified line-exact: `:331`
`pub fn layout(&self, rule_id: RuleId) -> Option<&StructLayout>`, `:337`
`pub fn layout_by_name`, `:314` `layouts: BTreeMap<RuleId, StructLayout>`,
`:326` `insert`, `:313` is the `StructRegistry` struct decl. The fix is
TOWARD citation truth on the StructRegistry pre-block fence and changes no
verdict — the fence remains "registry lookup is compile-time
(`project_types` population), runtime carries the resolved layout by
reference." From a CH3 view this is a citation-floor improvement on a
load-bearing pre-block, not a route re-opening.

**V5 DELTA (b) — 1B "NEW V5" NEON shared-classifier claim (`:59-63`).**
The cell asserts "the NEON shared classifier `select_classifier(alphabet)`
is ALREADY grammar-general in core (`simd_scan::scan_structural(input,
&StructuralAlphabet)` lib.rs:80; `KernelShape::select` alphabet.rs:118; all
8 generated grammars emit `GRAMMAR_STRUCTURAL_ALPHABET`) — NOT JSON-only;
impl-exceeds-spec. **No new shape, directive, or substrate.**" Verified
live: `crates/simd-scan/src/lib.rs:80` `pub fn scan_structural(input:
&[u8], alphabet: &StructuralAlphabet) -> StructuralIndex`;
`crates/simd-scan/src/alphabet.rs:118` `pub fn select(alphabet:
&StructuralAlphabet) -> Self` under `impl KernelShape` (`:116`);
`select_classifier` at skinny `bbnf-simd/src/dispatch.rs:42` `pub fn
select_classifier(alphabet: &'static [u8; 64]) -> SelectedClassifier`. This
is a GENERALITY observation (the classifier is grammar-general by
alphabet, not JSON-pinned), explicitly disclaiming any new shape/
directive/substrate and explicitly aarch64-NEON, not x86. From a CH3 view
it neither re-opens the x86 pre-block (`:806`) nor the D6 second-substrate
pre-block (`:807-811`) nor proposes a 6th shape — it is the correct
grammar-neutral reading of an existing core surface. Clean.

The AoS-vs-SoA tape divergence (core 16-byte `TapeRec`, `record.rs:103`
verified `pub struct TapeRec`; vs skinny SoA `Tape<'input>` `mod.rs:94`
verified `pub struct Tape<'input>`) is correctly catalogued as a
fold-convergence question (1C `RT17-001` `:72`, 1E `L01` row, 1D
`SK17L-001` `:135`), NOT a same-tree parallel substrate and NOT a
re-opened route — both encodings are admitted offset-tape shapes under
Lock 1, and 1C `:72` explicitly distinguishes them from the AV.04
columnar-SoA that Lock 1 buries.

## §3 — Admitted-REDRESS-Row Mis-Catalogue Scan

Verdict: **no admitted REDRESS row is mis-catalogued as unimplemented.**
The "unimplemented" verdicts across 1A–1F attach to the SKINNY-PROVEN
generalizations (SoA `Tape`, lazy `ValueRef<G>`, the `BackendRule`/
`FieldSource`-walk projection) that are ABSENT in crates/core — the
correct sense of "unimplemented in the fold target." A `grep -rn 'enum
BackendShape' crates/` returns ZERO (exit 1), confirming BSHAPE17-001's
"unimplemented" verdict for the 5-shape selector is a true absence in the
fold target (the enum lives only at skinny `ir/src/lib.rs:340`), and
`derive_backend_shape` resolves only to skinny `passes/src/lib.rs:392` —
both correctly "unimplemented in core," not a pre-block mis-flip.

The pre-blocked shapes (eager builder, AoS `TapeRec`, `StructRegistry`/
`StructLayout` indirection) are catalogued as PRESENT fold-targets, not as
"missing." The UNWIRED tape is correctly labelled spec-claims-implemented
(1C `RT17-003` `:77` "spec-claims-implemented (UNWIRED confirmed as
stated)") — the §0.1.11 claim that the tape exists-but-unwired is verified
true (`TapeStructBuilder` grep-zero outside `runtime/tape/`; live JSON uses
`JsonStructBuilder::new()` at `json/parse_with.rs:11,34`), not mislabelled.

The core CSS structural scan is correctly catalogued WIRED (1C `RT17-003`
`:77` + 1D `SK17L-008` `:110`): `crates/core/src/grammar/generated/
css_l4.rs:15982` calls `::simd_scan::scan_structural(input, &alphabet)`
over a `StructuralAlphabet` built from `GRAMMAR_STRUCTURAL_ALPHABET`
(verified `:15976-15982`); all 8 generated grammars carry `scan_structural`
+ `OnceCell<StructuralIndex>`, `math.rs` holds neither (scan-count 0) → 8
wired, not 9. The residual gap is correctly re-classified as the absent
tape CONSUMER, not the scan. No admitted row flipped to unimplemented in
the wrong direction; the retained `OnceCell<StructuralIndex>` (8 carriers)
is correctly bound to REDRESS-53 (1C `RT17-005` `:79,:102`, SPEC `:825`,
`:839`) with the index-IS-tape-or-`local_temp_only` fork, NOT catalogued as
a permissible parallel substrate. A `grep -rni` for self-reported "now
resolved"/"fully wired"/"no longer a divergence" returns ZERO across all
1X inventories — no paper-close self-report.

## §4 — Dispositions

### ACCEPT (8)

| ID | Section | Basis |
|---|---|---|
| CH3-A1 | 1B BSHAPE17-006 V5 anchor fix (`:74`) + 1A StructRegistry fence (`:120,:133`) | The V5 re-anchor of the StructRegistry lookup symbol to `layout` (`:331`) / `layout_by_name` (`:337`) is verified line-exact against `crates/ir/src/registry/struct.rs`; `:314` `layouts: BTreeMap`, `:326` `insert`, `:313` struct decl all confirmed. The fence ("registry lookup is compile-time, runtime carries resolved `&StructLayout`") is unchanged; `begin_compound` reads only `layout.rule_id & 0x1F` (`mod.rs:186`). A citation-floor improvement on a load-bearing pre-block; no route re-opened, no verdict change. |
| CH3-A2 | 1B "NEW V5" NEON shared-classifier claim (`:59-63`) | `select_classifier`/`scan_structural`/`KernelShape::select` verified at `dispatch.rs:42`/`simd-scan/lib.rs:80`/`alphabet.rs:118`. The claim is the correct grammar-general reading (impl-exceeds-spec, aarch64-NEON), explicitly disclaiming any new shape/directive/substrate. Does NOT re-open the x86 (`:806`) or D6 (`:807-811`) pre-block nor propose a 6th shape. Clean. |
| CH3-A3 | 1D `L-SK17-02` + `L-SK17-02b` + 1E `D-1E-SKV17-03`/ledger `:137` + 1C `RT17-006` (StructRegistry fence) | The 28-65×/983×/10583× pre-block is in both canonical 1D/1E ledgers with the no-per-leaf-lookup fence; SPEC `:793-795`,`:824` verified exact; `begin_compound:185` reading only `layout.rule_id & 0x1F` (`:186`) verified — the fence matches live code. No re-opening. |
| CH3-A4 | 1D `L-SK17-02b` + 1E `LAC-1E-SKV17-02` (`:179`) + 1f-anti-pattern `:73` (FieldSource compile-time) | The `FieldSource{TypedLeaf,BranchTag,SeqPosition,RepeatElement,RuleReference}` walk is fenced "COMPILE-TIME projection-emission, resolved once … NOT a per-leaf StructRegistry indirection"; `FieldSource` verified at `struct.rs:84`; SPEC `:794-795` anchored; the generalization is fenced against the regression it abuts. |
| CH3-A5 | 1D `L-SK17-07` (`:128`) + 1E ledger `:136` + 1f-past `:63` (D6 inversion guard) | The FORBIDDEN-in-skinny vs fold-target-in-core distinction is exactly right; SPEC `:807-811` verified to name `StructLayout`/`TapeStructBuilder`/`TapeCursor`; monotonic direction `:110-114` verified; §9 REJECTed D6 at `:854` verified; prevents the worst CH3 failure (relocating core substrate into skinny). |
| CH3-A6 | 1A `SUB17-005` (`:96`) + 1B `BSHAPE17-005` (`:73,:98`) + 1D `L-SK17-06` (`:126`) + 1E `L10`/`LAC-1E-SKV17-05` (`:118,:182`) + 1f-coherence `COH17-004` (x86/CollapsedStage not target) | x86 path classified non-target (x86 bar cited at SPEC `:806`, second-substrate block separate `:807-811`), aarch64 CollapsedStage classified the SPEC-named UNKNOWN-2D-05 not a fresh gap; SPEC `:258`,`:806`,`:852-854` + `ARCHITECTURE.md:1206` verified; 5-shape canon held (Lock 10 `:107-108`); no 6th shape, no aarch64-CollapsedStage re-derivation; `enum BackendShape` grep-zero in `crates/` confirms the unbuilt-in-core verdict. |
| CH3-A7 | 1D `L-SK17-01` (`:120`) + 1E ledger `:135` (AZ-IV eager fold-deletion) | The eager `CssTypedValue` (`value.rs:414`) + six pending Vecs + `pending_value: Option` (`builder.rs:71-79`) is catalogued as deletion target with lazy-`ValueRef<G>` replacement, not carried forward; live anchors verified. No revival. |
| CH3-A8 | 1C `RT17-005` (`:79,:102`) REDRESS-53 anchor + 1A Sidecar firewall row (`:118`) | The retained `OnceCell<StructuralIndex>` (8 carriers) is correctly bound to REDRESS-53 at its naming lines SPEC `:825`/`:839` with the index-IS-tape-or-`local_temp_only` fork; the UNWIRED tape is correctly labelled spec-claims-implemented; the scan is correctly catalogued WIRED (`css_l4.rs:15982`); no admitted row mis-flipped to unimplemented. |

### REVISE (0)

No V5 inventory carries an unfenced pre-block, a mis-identified resolved
pre-block, an off-by-one §9 anchor, or an admitted REDRESS row
mis-catalogued as unimplemented. The two V5 inventory deltas (1B
BSHAPE17-006 anchor fix; 1B NEW-V5 NEON classifier claim) are both verified
clean from a regression view. No new defect surfaced under this cycle's
line-exact re-resolution.

### REJECT (0)

No V5 inventory re-opens a pre-blocked route, mis-identifies a resolved
pre-block, mis-catalogues an admitted REDRESS row as unimplemented, or
inverts the monotonic direction. Nothing rises to REJECT.

## §5 — Continuity + Convergence Accounting

V4 CH3 returned 8 ACCEPT / 0 REVISE / 0 REJECT (100%), carrying no orphan
REVISE into V5. V5 CH3 returns **8 ACCEPT / 0 REVISE / 0 REJECT** —
ACCEPT-rate **8/8 = 100%**, at/above the §3Z ≥95% bar. The subject set is
complete (1A–1F all present at `cycle: V5`). Against the §3Z
two-consecutive-cycle rule: V4 CH3 was 100% (above bar) and V5 CH3 is 100%
(above bar) — CH3 alone now holds **two consecutive above-bar cycles**; the
two-consecutive close requires the *aggregate* wave to hold ≥95% across two
consecutive cycles, which the aggregator accounts. No CH3 finding blocks
convergence.

## §6 — Verdict

CH3 ACCEPT-rate on the V5 subject set: **8 ACCEPT / 8 dispositioned =
100%**, with **0 REVISE / 0 REJECT**. The re-opened-route firewall is
clean: no inventory revives AZ-IV eager (`:791`), StructRegistry
indirection (`:793-795`), fact-stream (`:796`), x86 (`:806`), or the D6
second substrate (`:807-811`); every crates/core construct is the SK-V18
fold TARGET with the monotonic skinny→totality direction guarded
(`:110-114`); the D6 inversion trap is correctly fenced; the StructRegistry/
FieldSource fence matches the live `begin_compound(&StructLayout)`
no-per-leaf-lookup shape (`mod.rs:185-186`). The two V5 inventory deltas
are both clean — the 1B BSHAPE17-006 anchor fix (`layout`/`layout_by_name`
at `:331`/`:337`, correcting the V4 struct-decl mis-cite) is a citation-
floor improvement on a load-bearing pre-block, and the 1B NEW-V5
grammar-general NEON `select_classifier` claim explicitly disclaims any new
shape/directive/substrate and is aarch64-NEON, not x86. The four
load-bearing pre-blocks are correctly identified in both canonical 1D/1E
ledgers. No admitted REDRESS row is mis-catalogued as unimplemented; the
UNWIRED tape is correctly labelled spec-claims-implemented and the core CSS
scan correctly WIRED across all 8 grammars. CH3 converges at 100% on this
cycle, holding two consecutive above-bar cycles (V4 + V5); nothing to fold
into a successor cycle.
