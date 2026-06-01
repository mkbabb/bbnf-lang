---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation (SK-V18 totality)
cycle: V7
disposition: ACCEPT
verification_head: dirty working tree at master (post 3ac131c45)
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

# CH1 — CORRECTNESS (SK-V18 T-P1 V7)

## Verdict

ACCEPT. Zero REVISE, zero REJECT.

My lens is citation correctness: every spec-claim<->impl row must resolve — the
spec path:line carries the claimed text, the impl path:line carries the claimed
symbol, the verdict matches the evidence; 1D RESULTS/REDRESS citations resolve to
real ledger entries; no recalled LOC/symbol/orphan citation. I read all six live
inventories plus both 1F auxiliaries, re-spot-verified the most load-bearing cited
rows in each against the V1 spec (ARCHITECTURE.md, MASTER-PLAN.md, LOCKS.md), the
SK-V18 plan (sk-v18/SPEC.md), and the live code in both trees (totality `crates/`
at repo root, `skinny/crates/`). Every figure I tested matches disk.

This V7 cycle ALSO discharges the two residual precision defects open from V6 (CH2
and CH4 each carried one REVISE). Both are now repaired on disk AND mutually
consistent across the two live 1F files — verified below. The inventories are sound.

Per the corrected REJECT convention: a reject is admissible ONLY when an inventory
STATES SOMETHING FALSE ON DISK with a live falsifying path:line. I found NO such
case. The one suspicion I raised (a god-file LOC mismatch on `gate.rs`) SELF-
FALSIFIED in the inventory's favour — the inventory's `src/bin/gate.rs` path
disambiguation is exactly correct (6175 LOC), and my first probe had checked the
wrong sibling `src/gate.rs` (545 LOC). A self-falsified suspicion that resolves for
the inventory is an ACCEPT, never a reject. The honest tally is reject=0.

## Prior-Cycle Fold Discharge (V6 REVISEs verified CLOSED this pass)

V6 left CH1 clean but CH2 and CH4 each open one residual precision REVISE. Both are
now repaired in the live inventories. CH1's lens owns whether the repaired text is
CORRECT on disk; I verified both:

| V6 finding | locus | required correction | live discharge this pass |
|---|---|---|---|
| CH2-V6-R01 | `1F-anti-pattern.md:65` | change "catches only **5** ident sites (CH2-V2-009 — leak 9-wide, NOT 4)" to the **4**-catch wording with `:137,:143,:149,:155` + the CH2-V3-008 supersession note | `1F-anti-pattern.md:65` now reads "the strict 4-name leak regex catches only **4** of the 9 idents rows (`:137,:143,:149,:155`; per CH2-V3-008, superseding the CH2-V2-009 '5' wording); the other 5 (`:161,:167,:173,:179,:185`) escape." EXACT match. Live `rg 'idents:.*(JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser)' crates/ir/src/registry/strategy.rs` = exactly `:137,:143,:149,:155` (4). Companion `1F-coherence-scan.md:79,:86` already carried **4**; the two live 1F files now AGREE on the narrow-regex catch count. `rg 'catches only 5\|only 5 ident'` across all 8 inventories = ZERO. |
| CH4-V6-001 | `1E-locks-evidence.md:108` | prefix both `find` operands with `skinny/` so the x86 cost-carrier command resolves from root: `find skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86 -type f = 28` | `1E-locks-evidence.md:108` now carries the skinny-prefixed form. Live: skinny-prefixed `find` = 28 files / 4401 LOC (exact); bare-from-root `find crates/bbnf-simd/...` = 2 errors / 0; `rg 'find crates/bbnf-simd' 1*.md` = ZERO (no bare-from-root form survives anywhere). The `28`/`4401`/`≈ −4500` figures stay TRUE. |

## Load-Bearing Rows Re-Verified On Disk (V7)

### 1A (substrate)
- `tape/mod.rs:94` = `pub struct Tape<'input>`; `:170` = `pub fn id(&self) -> TapeId`;
  `:175` = `pub struct ValueRef<'doc,'input:'doc,K=AnyKind,G:EventGrammar=AnyGrammar>`;
  `:178/:179` = the two `PhantomData` fields (`_kind`/`_grammar`); `:191` = the impl
  block. All verbatim.
- `ir/src/lib.rs:340-345` = the five `BackendShape` variants (EagerTape/OffsetTape/
  EventTape/SinkOnly/CollapsedStage); `cost.rs:57` = `pub enum SubstrateTarget`. Verbatim.
- 1A-SUB-025 / 1A-LOCK1-AMEND-001 contradiction REAL on disk: `LOCKS.md:620` carries
  verbatim "The `G:EventGrammar` type parameter is the generality vehicle";
  `sk-v18/SPEC.md:1202-1207` carries the G4a phantom-`<G>` DELETE (K-axis PRESERVED).
  The lock NAMES `<G>` as the generality vehicle while SPEC DELETEs it — catalogued
  correctly with the re-anchor disposition (`1A-substrate-evidence.md:97,:180`).

### 1B (codegen)
- `passes/src/lib.rs:329 mod recognizers`, `:392 fn derive_backend_shape`, `:473 fn
  choose_backend_shape`; `codegen/src/lower/mod.rs` `match cost.chosen` over the five
  `BackendShape` variants; `grammar_provider.rs:40-42 enum RuntimeEmitterKind{
  CompiledLowering,RequestFacts}`; `backend_egraph.rs:9 REWRITE_SET`. All verbatim.
- The four scaffold lowerers are `eager_tape.rs / offset_tape.rs / event_tape.rs /
  collapsed_stage.rs` — each EXACTLY 17 LOC (matches "17-LOC scaffolds"); the
  brace-shorthand `lower/{eager,offset,event}_tape.rs + collapsed_stage.rs` is cited
  with the CH1-V2-F2 correction that `collapsed_tape.rs` does NOT exist
  (`1B-codegen-evidence.md:61,:84`) — `ls collapsed_tape.rs` = No such file. The
  marker-string lowerer rows cite `lower/eager_tape.rs:16` etc., all live.

### 1C (runtime)
- 7× `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs` md5 = `b654562ccff46
  ed62dd48e9ace325830` (verified all 7 identical; the files are git-MODIFIED yet still
  byte-match the inventory figure — the dirty tree did not perturb the identity).
- 67 `@generated` markers in `crates/core/src/runtime` (matches the D4 stale-marker
  claim that `ARCH §9:1932` asserts 0/9 — the divergence is real).

### 1D (skinny lessons) — RESULTS/REDRESS resolution (lens-critical)
- `RESULTS.md` cold Track-1 rows confirm verbatim: twitter parse_only 8349.290 >
  sonic 4913.095; citm_catalog 9079.838 > 8335.772; canada 16709.901 > 12970.929;
  each row carries "per-iter equality PASS". Real measured ledger rows, not recalled.
- `REDRESS.md:742` = item 51 "SK-V5 event-cursor redress: byte-class whitespace cursor
  is REJECTED."; `:769` = item 52 "SK-V5 baseline reassay after the event-cursor
  rejection." (NOT a reject — correctly carved out); `:784` = item 53 "SK-V5
  structural-mask parser-local cursor is REJECTED." The item-51/52/53 reject-span
  distinction in 1B/1D resolves correctly; item 52 is NOT conflated into the reject span.
- `:126` = "Tape/direct-to-struct remains one substrate."; `:6326/:6356/:6416/:6446`
  = the W7/W8/W10/W11 admit headings. All verbatim.
- Live witnesses: `find_component_delim` at css `generated.rs:657`; `parse_w11_1_number`
  ×7 in `json/generated.rs`. Verbatim.

### 1E (locks) — sharpest falsifications
- 16-lock headings resolve to EXACTLY the cited lines: 75, 160, 170, 179, 181, 183,
  200, 202, 260, 269, 319, 328, 336, 349, 436, 453 (Lock 1..16 verbatim).
- L14 self-gate falsification (D-1E-V5-14) TRUE on disk: `LOCKS.md:349` carries the
  verification command asserting `rg ... crates/{ir,...}` "returns ZERO"; live `rg
  'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/
  crates/analysis/src/` = 13 (11 ir + 2 analysis). The lock's OWN gate is falsified/RED.
- x86 cost row (D-1E-V5-04) post-fix: skinny-prefixed `find` = 28 / 4401 LOC, exact.

### 1F (coherence + auxiliaries)
- The 9 `idents` rows resolve to EXACTLY `:137,:143,:149,:155,:161,:167,:173,:179,:185`
  (`:318` is the field decl, not a row); consumer `for_grammar_with_manifest(...,
  PRODUCTION_MANIFEST_TABLE)` at `strategy.rs:216`. COH18-005/COH18-012 carry the
  corrected **4**-catch / 9-wide breadth consistently with 1F-anti-pattern.
- COH18-001 scope drift REAL: `HANDOFF.md:17-19` says SK-V18 "adopts ... into the
  totality `crates/core/` tree"; `sk-v18/SPEC.md:19-21` says SK-V18 is "the
  GENERALIZATION cycle." Framing drift on disk.
- 1F-anti god-file LOC all EXACT: `report.rs` 11863, `src/bin/gate.rs` 6175 (the row's
  `/bin/` path disambiguation is correct — the sibling `src/gate.rs` is a DIFFERENT
  545-LOC file), `lock14_baseline.rs` 5095, `runtime_generator.rs` 1611,
  `codegen/lib.rs` 1473. `simd-scan/src/lib.rs:68` = `pub use index::{StructuralIndex,
  next_structural_at_or_after};` verbatim.

## Mechanical CH1 Sweeps

```sh
rg -n 'catches only \*\*5\*\*|catches only 5|only 5 ident' 1*.md        # ZERO
rg -n 'find crates/bbnf-simd' 1*.md                                      # ZERO
rg -n 'cycle: V6|this V6 inventory'                  1*.md              # ZERO
```

The post-fix sweeps confirm both V6 REVISEs are fully purged (no residual "5"-catch
claim, no bare-from-root `find crates/bbnf-simd`). The previously-accepted second-
anchor abbreviation convention (e.g. `runtime_generator.rs:195`…`:665`; `regen.rs:5`
then `:17-18`; `LOCKS.md:349` then `:620`) holds: each abbreviated path re-cites a
file whose full root-relative path is named earlier in the same row, and every one
resolves to a live file. None is an orphan citation. This matches the V5/V6 dispositions.

## Findings

| id | disposition | severity | finding | evidence |
|---|---|---|---|---|
| CH1-V7-001 | ACCEPT | none | The V6 CH2-V6-R01 REVISE is discharged on disk: `1F-anti-pattern.md:65` now carries the **4**-catch wording (`:137,:143,:149,:155` + CH2-V3-008 supersession), agreeing with the companion `1F-coherence-scan.md:79,:86`. The two live 1F files no longer contradict on the narrow-regex catch count; live strategy.rs proves exactly 4 catches. | `1F-anti-pattern.md:65`; `1F-coherence-scan.md:79,:86`; live `rg 'idents:.*(JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser)' crates/ir/src/registry/strategy.rs` = 4 (`:137,:143,:149,:155`); `rg 'catches only 5' 1*.md` = 0. |
| CH1-V7-002 | ACCEPT | none | The V6 CH4-V6-001 REVISE is discharged on disk: `1E-locks-evidence.md:108`'s x86 cost-carrier `find` is now skinny-prefixed and resolves to 28 files / 4401 LOC; no bare-from-root `find crates/bbnf-simd` survives anywhere. The `28`/`4401`/`≈ −4500` figures stay true. | `1E-locks-evidence.md:108`; live skinny-prefixed `find` = 28 / `cat\|wc -l` = 4401; bare-root = 2 errors / 0; `rg 'find crates/bbnf-simd' 1*.md` = 0. |
| CH1-V7-003 | ACCEPT | none | Every 1A load-bearing row resolves verbatim; the 1A-SUB-025 / 1A-LOCK1-AMEND-001 contradiction is REAL on disk (`LOCKS.md:620` names `<G>` the generality vehicle while `sk-v18/SPEC.md:1202-1207` DELETEs it). | `tape/mod.rs:94,:170,:175,:178,:179,:191`; `ir/src/lib.rs:340-345`; `cost.rs:57`; `LOCKS.md:620`; `sk-v18/SPEC.md:1202-1207`; `1A-substrate-evidence.md:97,:180`. |
| CH1-V7-004 | ACCEPT | none | Every 1B codegen row resolves; the four scaffold lowerers are the correct filenames (`eager_tape/offset_tape/event_tape/collapsed_stage.rs`) each EXACTLY 17 LOC; `collapsed_tape.rs` does not exist (CH1-V2-F2 correction holds). | `passes/src/lib.rs:329,:392,:473`; `lower/mod.rs` 5-shape match; `grammar_provider.rs:40-42`; `backend_egraph.rs:9`; 4× `wc -l`=17; `ls collapsed_tape.rs`=absent. |
| CH1-V7-005 | ACCEPT | none | 1C runtime figures verify: 7× css_l4 `generated.rs` md5 identity `b654562c...` (git-modified yet byte-identical), 67 `@generated` markers vs ARCH §9:1932-asserted 0/9. | 7× md5 `b654562ccff46ed62dd48e9ace325830`; `rg -l '@generated' crates/core/src/runtime` = 67. |
| CH1-V7-006 | ACCEPT | none | All 1D RESULTS/REDRESS citations resolve to real ledger entries (no recalled rows): the JSON >sonic cold figures, the item-51/52/53 reject-span distinction (52 NOT conflated), and the live witnesses all verify. | `RESULTS.md` twitter 8349>4913 / citm 9079>8335 / canada 16709>12970 (per-iter PASS); `REDRESS.md:742`(51 REJECT)/`:769`(52 non-reject)/`:784`(53 REJECT)/`:126`/`:6326,:6356,:6416,:6446`; `find_component_delim:657`; `parse_w11_1_number`×7. |
| CH1-V7-007 | ACCEPT | none | 1E's sharpest falsifications are TRUE on disk: the 16-lock headings resolve to the exact cited lines, and the L14 self-gate is RED (13 sites vs LOCKS:349-asserted ZERO). | 16 headings at 75..453; `rg ... crates/ir/src/ crates/analysis/src/` = 13 (11+2); `LOCKS.md:349` command asserts ZERO. |
| CH1-V7-008 | ACCEPT | none | Every 1F coherence + auxiliary row resolves: the 9 idents rows at exact lines + `:216` consumer, the HANDOFF/SPEC scope drift, simd-scan exports, and the god-file LOC. The `gate.rs` LOC suspicion SELF-FALSIFIED for the inventory — `src/bin/gate.rs`=6175 (the cited `/bin/` path), the 545-LOC `src/gate.rs` is a different file. ACCEPT, not reject. | `strategy.rs:137..185,:216`; `HANDOFF.md:17-19` vs `SPEC.md:19-21`; `simd-scan/lib.rs:68`; report.rs 11863 / `src/bin/gate.rs` 6175 / lock14_baseline.rs 5095 / runtime_generator.rs 1611 / codegen/lib.rs 1473. |

## REJECT Gate

No GENUINE reject. Per the corrected V6/V7 convention, a REJECT requires an inventory
to STATE SOMETHING FALSE ON DISK with a live falsifying path:line. Nothing in any
inventory states a falsehood: every spec text, impl symbol, LOC figure, md5, and
ledger entry I tested matches disk, and the two V6 REVISEs are now repaired. The lone
suspicion I raised this pass (god-file `gate.rs` LOC) self-falsified in the
inventory's favour — the inventory's `src/bin/gate.rs` 6175 cite is exact; my probe
had hit the wrong sibling. A self-falsified suspicion is an ACCEPT, recorded as reject=0.

## Disposition

CH1 returns CLEAN at V7 — eight ACCEPTs, zero REVISE, zero REJECT — with both V6
residual precision REVISEs (CH2-V6-R01, CH4-V6-001) verified DISCHARGED on disk and
the two live 1F files reconciled. This is the second consecutive clean CH1 pass (V6
was also reject=0/revise=0); the citation surface my lens owns has reached the
2-consecutive-clean fixed point.

TALLY accept=8 revise=0 reject=0
