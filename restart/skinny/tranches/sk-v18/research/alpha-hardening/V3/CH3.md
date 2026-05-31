# CH3 REGRESSION (V3) — SK-V18 Pass-Alpha hardening

**Lens:** CH3 Regression per `PASS-ALPHA §3` ("does any proposed intervention re-open a
route in REDRESS? Cross-check the shortlist against entries 1-N. Has α-C correctly
identified the pre-block list?") + `ORCHESTRATOR §3W/§3Z`.
**Subject under review (V3):** `restart/skinny/tranches/sk-v18/research/alpha/{alphaA..E}.md`
(there is no `alphaF-*.md`; per `PASS-ALPHA §2/§6` the α-F deliverable IS `SYNTHESIS.md` +
`HANDOFF.md`, both reviewed — the V1 and V2 CH3 cycles read the contract identically).
**Host:** aarch64 Apple M5 Max ONLY (x86 OUT). **HEAD of record:** `318d9c046` (unchanged
since V1/V2; the entire `sk-v18/` tree is untracked working-state — `git status` = `?? …/sk-v18/`).
**Method (V3 confirming cycle):** the V2 CH3 returned 100% ACCEPT (7A/0R/0R). The V3 duty is
NOT to trust the V2 log: (a) re-grep every pre-block ground-truth LIVE at HEAD, independently;
(b) confirm the V2→V3 fold (alphaC/alphaE/SYNTHESIS now carry "cycle V3" + the F9-F12 / Note
folds) introduced NO new candidate and NO re-open vector; (c) re-confirm the three CH3 axes
(no pre-block re-open · PRUNE-before-GENERALIZE · prune-does-not-strand->SOTA) hold on the
V3 artefacts. Every disposition cites `path:line`/SHA.

---

## Verification log (re-grepped LIVE at HEAD `318d9c046`, V3 independent re-run)

| Pre-block / claim | Command | Result | Artefact agreement |
|---|---|---|---|
| x86 tree present (P1 target) | `find skinny/crates/bbnf-simd/src/x86_64 -type f \| wc -l` | **24** | SYNTHESIS:115/194, HANDOFF:64/90, alphaC:90, alphaE:72 ✓ |
| x86 `.rs` LOC (P1 dual-figure fold) | `find …/x86_64 -name '*.rs' \| xargs wc -l \| tail -1` | **742** total | alphaC:50-51/91 (742 `.rs` + 105 `.asm` = 847), alphaE F11 ✓ |
| fact-stream RETIRED | `grep -c emit_fact_stream …css_l4_declaration_values/generated.rs` | **0** | SYNTHESIS:269, alphaC §0.B/§2.3, alphaD F2 ✓ |
| CSS const-`&str` courier (G2 / verbatim-blob) | `grep -n 'const CSS_GENERATED_RS' …runtime_generator.rs` | **`:701`** `const … &str = r#"` | SYNTHESIS:200/285, alphaC:322, alphaE B2:116/133 ✓ |
| `RuntimeEmitterKind` fork (G3) | `grep -n 'enum RuntimeEmitterKind' …grammar_provider.rs` | **`:40`** (dispatched `runtime_generator.rs:17,25`; `lib.rs:282,291`) | SYNTHESIS:201, HANDOFF:108, alphaC:324 ✓ |
| phantom `<G>` (G4) — full decl | `grep -n 'struct ValueRef' tape/mod.rs` | **`:175`** `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` | SYNTHESIS:202/425, alphaC:240/495, alphaE B3:145 ✓ |
| Lock-14 x86 tag + allowlist assert (P4 same-commit coupling) | `grep -n 'diagnostic-x86\|accepts_current_allowlist' lock14_baseline.rs` | `:2463` tag · `:2729` `fn accepts_current_allowlist` | SYNTHESIS:197, alphaC:102-104 V2-FOLD held, alphaE P4 ✓ |
| metalang leak (P5) | `grep -c parse_w11_1_number …json/generated.rs` | **7** | SYNTHESIS:244, alphaC:204/§2.5, alphaE P5:76 ✓ |
| 7 CSS replicas byte-identical (P3) | `find …css_l4_*/generated.rs \| xargs md5 \| sort -u \| wc -l` | **1** (over **7** dirs) | SYNTHESIS:292, alphaC:142, alphaE P3:74 ✓ |
| Sheets `google-sheets.bbnf` EXISTS (PROVE adopt-not-stub) | `find . -name google-sheets.bbnf` | `grammar/google-sheets/google-sheets.bbnf` (totality tree) | SYNTHESIS §0.5/330, HANDOFF:10, alphaD S12, alphaE F2:172 ✓ |
| Sheets witness is a stub (PROVE target) | `find skinny -path '*sheets_witness*' -name '*.rs'` | `event_grammar_witness.rs` + `mod.rs` (~25 LOC) | alphaD S12, alphaE B4:175 ✓ |
| 16-lock count | `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | **16** | SYNTHESIS:208, HANDOFF:227, alphaC:465 ✓ |
| HEAD still `318d9c046` | `git rev-parse HEAD` | `318d9c046…` (V3 audit + alpha cohort untracked) | unchanged since V1/V2 ✓ |

Every CH3-load-bearing ground-truth claim resolves as stated at HEAD, identical to the V2
independent re-run. No fabricated citation. The x86 file census (24) and `.rs` LOC (742)
match the alphaC §0.A.1 / alphaE F11 dual-figure fold (742 `.rs` + 105 `.asm` = 847 across 24
files); the close gate is `find …/x86_64 -type f` = 0, which deletes all 24 regardless of LOC
framing, so the dual-figure is an accuracy improvement, not a gate change. The phantom `<G>`
decl confirms FOUR type params (`'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar`) where `K`
is the real axis and `G` the phantom — exactly the SYNTHESIS:202 / alphaE B3 two-axis framing.

---

## The three CH3 axes — V3 global findings

### Axis 1 — does any proposed intervention re-open a REDRESS pre-block?

**Finding: NO (confirmed; the V3 fold introduced no regression vector).** The V3 contract
carries exactly **5 candidates** (`grep -c '^### CANDIDATE' alphaE` = 5: A, B1, B2, B3, B4),
identical to V1/V2; the shortlist remains "additive-by-deletion" (alphaE CC#6, :216; CC#8
:218 "No candidate was added or removed … still exactly 5"). Each candidate carries an
explicit Pre-blocks line (alphaE A:88, B1:108, B2:137, B3:162, B4:191). Cross-checking all 5
candidates + the 13 still-open S1–S13 (alphaD §4) against the six pre-block families
re-confirms the clean result. The six families, each re-verified LIVE:

- **AZ-IV eager-value-tree (118×):** SYNTHESIS:259-263 + HANDOFF:183-185 keep G4's shared
  trait "LAZY over the tape — it does NOT re-introduce an eager value tree"; alphaC §2.1
  keys the re-open test to the THREE new surfaces (G1 projection, G2 derived recognizer, G4
  trait boundary) and anchors it to the *construct* (per-leaf eager payload), not a fixed
  symbol list. The richness seam is closed by `json_rich_navigation_preserved`
  (SYNTHESIS §0.5; alphaE B3 gate #3 FOLD-F7) so a ≥2-impl LCD-flatten cannot slip past a
  green ≥2 count. **No re-open.**
- **StructRegistry / Arena/Builder per-leaf (28-65× / 983× / 10583× WATCHDOG):**
  SYNTHESIS:264-267 + HANDOFF:186-187 keep the permanent pre-block on per-leaf indirection;
  the no-second-substrate clause (SYNTHESIS:312-316, HANDOFF:206-209) REJECTs an introduced
  `StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside the landed `Tape`/`ValueRef`.
  alphaC §2.2 splits the PERMANENT pre-block (per-leaf indirection) from the
  ADMIT-UNDER-DIFFERENT-FRAMING layout-built-once-per-rule. **No re-open.**
- **CSS fact-stream String-as-output:** verified RETIRED at HEAD (`emit_fact_stream` = 0).
  SYNTHESIS:268-271 + HANDOFF:188-190 explicitly pre-block G2 from "replace the const-string
  courier with a fact-stream String." alphaC §0.B narrows the pre-block to its residual
  (`CSS_GENERATED_RS` + `RuntimeEmitterKind::RequestFacts`) — the correct refinement of a
  dead route, preventing a dead-route fight. alphaD F2:260 hardens it to "RETIRED — re-land
  is REJECT." **No re-open.**
- **24-row broadcast:** SYNTHESIS:276-277 + HANDOFF:194 retire it; alphaC §2.4 (PERMANENT)
  binds the new Sheets corpus to per-corpus N≥50 cold + median (Lock 8), forbidding a single
  aggregate loop projecting one number across N row-ids; alphaE B4:191 binds the Sheets
  cell-corpus to "cold per-parse, not 24-broadcast." **No re-open.**
- **FNV / fixture contrivances:** SYNTHESIS:278-281 + HANDOFF:195 keep FNV bench-only; alphaC
  §2.5 binds the NEW re-entry seam — the generator's tape pre-sizing — to derive from
  `input.len()` + `BackendRule`/`LayoutFacts` grammar-generally, never per-corpus, and tags
  P5 (the `parse_w11_1_number` bench-wave-id leak) as the live instance. alphaD F1:259
  tightens the phantom-`G` test-only finding so the gate cannot accept a test-only use.
  **No re-open.**
- **x86 / AVX / SVE:** SYNTHESIS:282-284 + HANDOFF:196-197 keep aarch64-only; P1 enforces by
  deletion of all 24 files. alphaC §2.6 keys the re-open test to G5/G6 (the only acceleration
  waves) and binds the G6 ASM backlog to aarch64-only NEON. **No re-open.** (P1 census
  re-verified: 24 files present, 742 `.rs` + 105 `.asm`, awaiting deletion.)

The "no second substrate" Lock-1 guard (the subtlest CH3 re-entry for G3/G4) is held in every
artefact: SYNTHESIS:312-316, HANDOFF:206-209, alphaC §3, alphaD V1:82/§5, alphaE B2:137/B3:162.

**The single most important CH3 distinction — correctly held:** the SK-V17 residual
**REDRESS-W2-1 single-emitter** is a REDRESS entry, and G3 un-forks `RuntimeEmitterKind`. A
naïve CH3 read would flag this as re-opening a REDRESS route. The artefacts correctly frame it
the opposite way: REDRESS-W2-1 is the SK-V18 G3 **SUBJECT**, "admitted to be discharged here —
NOT re-opens" (HANDOFF:213-215; SYNTHESIS:303-304; alphaE B1:108). This is the binding-principle
backtrack — the inflection — not a regression. **Verdict: Axis 1 HELD.**

### Axis 2 — PRUNE-before-GENERALIZE

**Finding: HELD.** The sequencing A → B1 → B2 → B3 → B4 with P4 (Lock-14 gate) landing BEFORE
the B1/G2/G3 emitter rebuild is binding across SYNTHESIS §0.5 + §3 (lines 244-248), HANDOFF
Next-Move (lines 269-275), alphaE §0 ordering rule (:54) + CC#1 (:211) "A green Lock-14 gate
(P4) must land BEFORE B1, so the un-forked emitter is actually scanned for neutrality as it is
built." The entry-gate dependency is carried as an EXPLICIT exit-gate-blocks-successor clause
into S-P3 (alphaE CC#1, the CH6 §5 fold). The P1/P4 same-commit coupling subtlety (the x86-tag
removal at `lock14_baseline.rs:2463` must land in the SAME commit as the
`accepts_current_allowlist` assertion drop, else the gate desyncs) is folded into alphaC
:102-104 as a held V2-FOLD; the HEAD greps confirm both edit sites exist (`:2463` tag, `:2729`
`fn accepts_current_allowlist`). **Verdict: Axis 2 HELD.**

### Axis 3 — prune does not strand `>`SOTA

**Finding: HELD (confirmed).** No deletion removes `>`SOTA-bearing code:
- **P1 (x86):** 0 real intrinsics, 14 `unimplemented!` (alphaC:94) — zero admission weight.
- **P2 (old CSS bench):** the headline numbers came from `css_canon_bench` (KEPT), NOT
  `nonjson_css_l4.rs measure_mbps` (SYNTHESIS:244; alphaC §1-P2). alphaE A:88 + B2:121 + CC#3
  :213 explicitly forbid deleting `css_canon_bench.rs`/`w2_rich_cssom_bench.rs` + the 9-field
  `assert_rich_strict_equality` oracle ("the ONE honest artefact KEPT from the old file").
  **Confirmed.**
- **P3 (replicas):** md5 = 1 over 7 dirs (re-verified) — collapsing 6 of 7 strands no unique
  capability; the distinct-grammar-output gate is bound to *provenance* (distinct `.bbnf`),
  not cosmetics (alphaC §1-P3 / §2; alphaE B2:134).
- **P5 (metalang):** symbol-name purge only; function bodies stay (alphaC §1-P5:211 "the
  function bodies stay, the names lose the wave label").

The standing-order invariant — "a derived parser that loses the speed is not done — surface
honestly as a named validated grammar-parameterized primitive, do NOT paper-close" — is
GATED, not bare: the PASS-IMPL honest-finding escape requires the primitive be
`.bbnf`-INVOKED by name + parameterized by grammar-derived DATA + carry a checkasm/scalar
reference; "a 'primitive' that is a relabeled blob without `.bbnf`-invocation + parameterization
+ a reference is REJECTED to REDRESS" (alphaE CC#2:212; SYNTHESIS §0.5 fallback column:328-331;
HANDOFF §6). This converts the single largest paper-close surface in a generalization cycle
into a checked condition. **Verdict: Axis 3 HELD; the prune cluster strands nothing; the
generalize clusters carry a GATED honest-finding escape.**

---

## V2→V3 fold verification (no CH3-axis regression introduced)

The V3 artefacts declare themselves the confirming cycle ("cycle V3", alphaC:3, alphaE:1). The
V2 CH3 returned 100% ACCEPT, so there were no CH3-axis REVISEs to orphan; the V3 folds touching
adjacent lenses (alphaE F9-F12, alphaC Notes 1-3) are research-recipe sharpening that I confirm
introduce NO re-open vector:

| Fold | What it changes | CH3 impact |
|---|---|---|
| **F9** (alphaE:18) | shared-trait grep is now test-excluded + canonical-trait-named | TIGHTENS the G4 no-LCD-flatten / no-eager-tree seam; cannot re-open AZ-IV — closes a false-green. No new candidate. |
| **F10** (alphaE:19) | neutrality grep uses canonical 4-grammar alphabet (`Json\|CssL4\|(GoogleSheets\|Sheets)\|Bbnf`) + widened scan roots (codegen + xtask) + type census | TIGHTENS the G3 relocated-overfit-seam guard; a `match grammar` branch relocated into an xtask `RuntimeTarget` data-table can no longer escape. Pure strengthening. |
| **F11** (alphaE:20) | P1 LOC label −742 → −847 (incl `.asm`) | Accuracy only; the close gate is file-count = 0. No re-open. |
| **F12** (alphaE:21) | G6 owner path `aarch64/dispatch.rs` → `dispatch.rs` (disk-verified) | Accuracy only; G6 stays aarch64-only. No x86 re-entry. |
| **Note-1** (alphaC:48-51) | x86 dual-figure 742 `.rs` / 847 all-24 | Accuracy; close gate unchanged. |
| **Note-3** (alphaC:54-62) | drops the stale "no V1 CONSOLIDATED" assertion | Process accuracy; no gate touched. |

All folds are sharpening of the re-open *tests* (making them un-false-greenable) or accuracy
corrections — none weakens a pre-block, adds a candidate, or relocates a re-open vector. The
three load-bearing generalization gates (PRESERVED->SOTA / GRAMMAR-DERIVATION-PROOF /
DISTINCT-GRAMMAR-OUTPUT) are unchanged in substance (alphaE CC#8:218). **The V3 fold is
orphan-free and regression-free on the CH3 axis.**

---

## Per-section dispositions (V3)

### alphaA-results-extraction.md — **ACCEPT**
CH3-relevant content re-verified: the retired 24-row broadcast is named a pre-blocked route
NOT the `>`SOTA; the pre-block list is verbatim from the seed; PRUNE close conditions preserve
the headline harness; the x86 census reads 24 files. No intervention here re-opens a pre-block.
**ACCEPT.**

### alphaB-competitor-deltas.md — **ACCEPT**
The CSS bar is framed ASYMMETRIC lazy-vs-eager (discharging the timed-plane-symmetry pre-block
honesty); asmjson AVX-512 held permanently OUT (aarch64 mandate); JSON comparators strict,
cold, no-broadcast. The yyjson/asmjson/RapidJSON honest-`None`-on-aarch64 (FFI not wired)
discipline is reflected in alphaE CC#4:214 — a fabricated competitor column is REJECTed, a
*strengthening* of the no-more-work-competitor pre-block. **ACCEPT.**

### alphaC-redress-digest.md — **ACCEPT** (the load-bearing CH3 artefact)
This is the artefact CH3 most directly reviews. It is "cycle V3" (alphaC:3), correct, complete,
and tightened:
- §1 (P1–P5) frames every prune as a wave with owner paths, delete-or-fix obligation, and a
  close gate; each verified LIVE at HEAD.
- §2.1–§2.6 enumerate exactly the six pre-block families, each PERMANENT-vs-ADMIT classified,
  with a re-open test keyed to the THREE new SK-V18 surfaces (generator / shared trait /
  instantiated-`<G>`), and a different-framing admission.
- §0.B correctly narrows the fact-stream pre-block to its residual after `emit_fact_stream` = 0.
- §3 "checked TWICE — against runtime output AND the emitter that produces it" is the
  exactly-right framing for a generalization cycle (the generator can re-open a pre-block at
  its SOURCE); the §1-P4 V2-FOLD adds the `EventGrammar`-type-literal seam to the emitter scan.
- The three V2 accuracy notes are folded (§0.A.1-3); zero orphan REVISE entering V3.
**ACCEPT.**

### alphaD-validated-invalidated.md — **ACCEPT**
§5 PRE-BLOCKED asserts "NONE of S1–S13 re-opens any" of the six families with the new-surface
binding (G4 over EXISTING lazy `ValueRef`; G2 toward lowering NOT fact-stream; P5 a symbol-name
purge; P1 DELETES x86). §1 VALIDATED marks `css_canon_bench`/substrate (Lock 1)/the two
`>`SOTA proofs as "do NOT re-prove; preserve" — the prune-does-not-strand guard. F1 tightens
the phantom-`G` test-only finding; F2 tightens the fact-stream pre-block to RETIRED. S12 names
`google-sheets.bbnf` as a REAL 185-LOC Pratt grammar (adopt-not-stub). **ACCEPT.**

### alphaE-candidate-shortlist.md — **ACCEPT**
Re-verified against the `PASS-ALPHA §3` "cross-check against entries 1-N" instruction:
- Candidate count = 5 (unchanged — no new candidate, no regression vector); each carries an
  explicit Pre-blocks line; CC#6:216 "No candidate re-opens a pre-blocked route: verified
  against the V3 pre-block list."
- Sequencing (§0:54, CC#1:211) enforces PRUNE-before-GENERALIZE with P4 before B1 and the
  exit-gate-blocks-successor dependency carried into S-P3.
- The honest-finding escape (CC#2:212) requires `.bbnf`-invocation + parameterization +
  checkasm/scalar reference — the prune-does-not-strand-`>`SOTA guard as a checked condition.
- B4 binds the same-wave-consumer rule (V5 orphan-kernel) + acceleration-at-admission (G6),
  with the G6 NEON-body count BOUNDED (PMULL first; every other kernel retired/relabelled
  unless a same-wave consumer exists) — no open ASM-body budget that could smuggle x86.
- The V2→V3 folds (F9-F12) sharpen the re-open greps without adding scope.
**ACCEPT.**

### SYNTHESIS.md (the §0 contract / goalset, standing for alphaF) — **ACCEPT**
§0.4 Pre-blocks (lines 254-316) carries all six families verbatim + the verbatim-blob /
phantom-generic / distinct-grammar-output re-entries + the inherited REDRESS family ids + the
hidden-coupling escape list + the no-second-substrate Lock-1 clause. §0.1 G3 binds the
canonical Lock-14 three-surface model + the arm-census/type-census co-greps ("md5-distinctness
alone is necessary-not-sufficient"); G4 names the `G` axis vs the real `K` axis with
DELETE-default (abrogate-before-patch); §0.5 PROVE adopts the existing Pratt `.bbnf` and the
fallback column carries the GATED honest-finding escape. §2 telemetry makes every pre-block
machine-checkable per row (17 generalization-status columns: `verbatim_blob_present`,
`emitter_fork_present`, `generator_grammar_branch_count`, `phantom_generic_resolved`,
`shared_value_trait_instantiations`, `json_rich_navigation_preserved`,
`acceleration_at_admission`, `x86_tree_deleted`, `lock14_gate_scans_codegen`,
`metalang_leak_present`, …). No goalset gate re-opens a pre-block; PRUNE-first is binding.
**ACCEPT.**

### HANDOFF.md — **ACCEPT**
Pre-Blocked Routes (lines 179-223) carries the six families + the CHALLENGE addenda + inherited
REDRESS family ids + the hidden-coupling escape list + the no-second-substrate clause.
Invariant 5 (lines 231-251) binds the canonical three-surface Lock-14 model + the
`match grammar`-arm census over the FULL alphabet (`Json|CssL4|(GoogleSheets|Sheets)|Bbnf`,
`GoogleSheets` un-abbreviated) across codegen AND xtask + the grammar-named-type census + the
`EventGrammar`-literal witness-seam note. Invariant 6 names the pre-block families. The SK-V17
residual REDRESS-W2-1 single-emitter is correctly declared the SK-V18 G3 SUBJECT, "admitted to
be discharged here — NOT re-opens" (lines 213-215). Next-Move sequences PRUNE→GENERALIZE→PROVE
with P4-before-emitter. **ACCEPT.**

---

## Summary

All seven reviewed sections (alphaA, alphaB, alphaC, alphaD, alphaE, SYNTHESIS §0, HANDOFF)
pass the CH3 lens at V3. Every CH3-load-bearing ground-truth re-verifies LIVE at HEAD
`318d9c046` (unchanged since V1/V2): x86 tree = 24 files / 742 `.rs` LOC; `CSS_GENERATED_RS`
const-`&str` at `runtime_generator.rs:701`; `RuntimeEmitterKind` enum at `grammar_provider.rs:40`;
phantom `ValueRef<…,G:EventGrammar=AnyGrammar>` at `tape/mod.rs:175`; `parse_w11_1_number` ×7 in
`json/generated.rs`; 7 CSS replicas md5 = 1; `google-sheets.bbnf` exists; `fact_stream` = 0;
16 locks; `diagnostic-x86` tag `:2463` + `accepts_current_allowlist` `:2729`. The candidate
count is unchanged at 5 (additive-by-deletion); the V2→V3 fold (F9-F12 grep sharpening +
Note 1-3 accuracy corrections) introduced NO new candidate and NO re-open vector — it only made
the re-open tests un-false-greenable. All three CH3 axes HELD: (1) no candidate re-opens any of
the six pre-block families (AZ-IV / StructRegistry / fact-stream / 24-broadcast / FNV / x86),
and REDRESS-W2-1 single-emitter is correctly framed as the G3 SUBJECT not a re-open; (2)
PRUNE-before-GENERALIZE is binding with P4-before-B1 and the P1/P4 same-commit coupling;
(3) the prune cluster strands no `>`SOTA (headline `css_canon_bench`/`w2_rich_cssom_bench` KEPT
and explicitly protected; x86/replicas/metalang carry zero admission weight), and the
generalize clusters carry a GATED honest-finding escape (`.bbnf`-invocation + parameterization +
reference required).

**Convergence posture:** 7 ACCEPT / 0 REVISE / 0 REJECT = 100% ACCEPT on the CH3 axis (≥95%
threshold met, third consecutive cycle). Zero orphan REVISE. No re-opened REDRESS pre-block.

TALLY accept=7 revise=0 reject=0
