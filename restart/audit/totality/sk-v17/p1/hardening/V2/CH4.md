---
lens: CH4-COST
pass: T-P1-excavation
cycle: V2
generated_at: 2026-05-29T22:30:00Z
subject: SK-V17 T-P1 excavation artefacts (1F multi-output; 1A-1E absent this cycle)
artefacts_reviewed:
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
focus: "divergences carry the propagation surface (files touched) for the eventual fold; no speculative spec claim; LOC-delta + risk-class per PASS-1 §3 CH4"
live_truth_method: "wc -l + grep -rn/-rln over crates/core/src/runtime/{tape,json,css_l4}/, crates/ir/src/registry/struct.rs, crates/simd-scan/src/, crates/core/src/grammar/generated/*.rs, crates/core/src/backend/rust/emitter/; sed -n line-spot-checks against every cited path:line; no cargo/build mutation"
dispositions:
  accept: 6
  revise: 4
  reject: 0
---

## Executive Summary

CH4 COST review of the three SK-V17 T-P1 1F inventories (cycle V2). The
sibling inventories 1A–1E are **not present** in `sk-v17/p1/` this cycle —
only the three 1F outputs exist (`ls` confirms; git-untracked). CH4 reviews
what is on disk and flags the absence as a structural gap below.

Every cited path:line in the three 1F files **resolves correctly** on
spot-check — the citations are not recalled. The cost-bearing findings
(COH17-001/002/005/006/008) carry an LOC-delta range and a risk class, so the
nominal CH4 §3 contract ("realistic LOC-delta estimate and risk class") is
met in form. But this dispatch's sharpened CH4(V2) focus — **divergences
carry the propagation surface (files touched) for the eventual fold** — is
where four findings under-deliver:

1. **COH17-006 (`StructLayout` rename) is mis-priced by an order of
   magnitude.** Inventory: "40-120 LOC rename; low". Live: `StructLayout` has
   **960 references across `crates/`** (grep count), spanning the codegen
   emitter and ~16 test files; a rename re-emits every generated parser. The
   propagation surface is not enumerated and the "low" risk understates the
   blast radius. **REVISE.**

2. **The css_l4-scan Gap row is factually false** — it states "crates/core
   structural scan has NO css_l4 alphabet wired (json/ebnf/bnf/csv only)"
   (1f-coherence `:107`), but `css_l4.rs:15936-15982` carries
   `scan_structural` + `StructuralAlphabet` + `OnceCell<StructuralIndex>`,
   and `google_sheets/bbnf/css_pretty` do too. This mis-states the scan
   surface as a *missing primitive* when it is already present, mis-pricing
   the CSS-scan fold to zero-cost. **REVISE.**

3. **COH17-001/002 (tape + value-API fold) carry LOC ranges but no
   files-touched enumeration.** The eager builders propagate across **22+
   files** (every generated parser + the whole emitter shape hierarchy +
   `runtime/mod.rs`). The dispatch requires the propagation surface; the
   inventory gives a divergence-*point* file:line and a bare LOC range.
   **REVISE.**

4. **The `pending_*` count is wrong** (1f-anti-pattern `:57` says "nine",
   the struct has six `Vec` + one `Option` = seven distinct fields; the core
   mod-doc itself says "seven"). Minor, folds into the COH17-002 REVISE.

No REJECTs: no finding is spec-speculative, no divergence is fabricated, the
risk-class taxonomy is sound, and every LOC range is defensible in
*magnitude* even where the files-touched surface is unstated.

## Section-by-Section Dispositions

### 1f-coherence-scan.md

| ID | Finding | CH4 disposition | Reason (file:line + fix) |
|---|---|---|---|
| COH17-001 | Tape AoS-core vs SoA-skinny; "200-600 LOC SK-V18 fold; medium" | **REVISE** | LOC magnitude is defensible (`record.rs:103,120` 16-byte AoS verified; `skinny/.../tape/mod.rs:94` SoA verified). But the **propagation surface is unstated**: the live eager builders touch 22+ files (`grep -rln JsonStructBuilder\|CssStructBuilder crates/core/src/` → generated `{json,css_l4,css_pretty,bbnf,google_sheets}.rs` + `runtime/mod.rs` + both `parse_with.rs` + the emitter `shapes/{substrate,object,array,alt_dispatch,flat,keyword,wrap}` hierarchy + `emitter/grammar.rs`). FIX: add a "files touched" column enumerating the generated-parser set + emitter shape hierarchy + `runtime/mod.rs`; note that re-emit is regen-gated (clean-regen discipline), so the LOC delta is *generator-side*, not hand-edit. |
| COH17-002 | Value-API per-grammar eager vs lazy `ValueRef<G>`; "300-700 LOC; high" | **REVISE** | Claim verified: `value_from_ref`/`ValueRef` grep-zero in `crates/core/src/runtime/json/` (confirmed empty); `css_l4/value.rs:414` `CssTypedValue` is eager. The "high" risk is correct and AZ-IV-anchored. But the **files-touched surface is unstated**: the projection-generator fold lands in `emitter/` (the generator) and re-emits every grammar's `value.rs`/`view.rs`. FIX: enumerate the emitter generator file(s) + per-grammar `{value,view}.rs` re-emit set; distinguish generator-LOC (hand-written, ~300-700) from emitted-LOC (regenerated, not counted against budget). |
| COH17-003 | Tape UNWIRED + scan WIRED asymmetry; "0 LOC to catalogue; high to wire" | **ACCEPT** | Cost framing is exactly right: cataloguing is 0 LOC, wiring is the SK-V18 subject. `parse_with.rs:34` `JsonStructBuilder::new()` verified; `TapeStructBuilder` grep-zero outside `tape/` verified; `json.rs:732` `scan_structural` verified. The split-cost honesty (catalogue-cost vs wire-cost) is the model other rows should follow. |
| COH17-004 | x86 CollapsedStage vs aarch64-no-CollapsedStage; "0 LOC (canon holds); medium" | **ACCEPT** | Verified: `ARCHITECTURE.md:1206` "x86-only; aarch64 mechanically refused"; `:1088` 5-shape projection clause; `LOCKS.md:107-108` 5-shape domain holds. 0-LOC (canon unchanged) + medium-risk (fold must absorb NEON into the 4 LLVM shapes without a 6th) is an honest cost statement. No files-touched because the canon is unedited — correctly so. |
| COH17-005 | Two SIMD crates multi-arch-totality vs aarch64-skinny; "100-400 LOC scope reconcile; medium" | **ACCEPT** | `simd-scan/src/lib.rs:80` `scan_structural(&StructuralAlphabet)` verified; `skinny/.../bbnf-simd/src/dispatch.rs:42` `select_classifier(&[u8;64])` verified. Correctly framed as scope-pressure not defect. The 100-400 range is reasonable for a kernel-set scope decision; the "files touched" is genuinely deferred (it is a T-P2 scope decision, not a known surface), so omission is defensible here — unlike COH17-001/002 where the surface is already known. |
| COH17-006 | `StructLayout` Lock-2-retired but live in core; "40-120 LOC rename; low" | **REVISE** | **Mis-priced by an order of magnitude.** `grep -rn StructLayout crates/` = **960 references**; spans `crates/ir/src/registry/struct.rs:202` (def, verified), the codegen emitter (re-emits every parser), and ~16 `crates/core/tests/*.rs`. A rename is not 40-120 LOC and not "low" risk — it touches the generator, every generated parser (via regen), and the test corpus. FIX: re-estimate as a generator-side rename (`StructLayout`→`Layout`) that **regenerates all 8 generated parsers** + migrates ~16 test files; risk **medium** (cross-crate, test-corpus-wide, regen-gated); state the 960-site grep as the propagation surface. |
| COH17-007 | Lock-1 FactStream 5th category vs SK-V17 retiring it as CSS admission; "0 LOC; low" | **ACCEPT** | Correctly classified UNKNOWN (not contradiction); `LOCKS.md:100-116` + SPEC `:243-244` framing verified. 0-LOC/low is honest — it is a framing reconciliation routed to T-P3/Omega, not a code fold. verify_action present. |
| COH17-008 | core `StructuralAlphabet` (digraph/quote/kernel-shape) richer than `[u8;64]`; "50-200 LOC alphabet reconcile; low" | **ACCEPT** | `alphabet.rs:19-37` (singletons/digraph_mask/digraph_pairs/quote_classes) verified; `select_classifier(&[u8;64])` verified. Both grammar-as-data (Lock 14 honoured). 50-200 LOC for an alphabet-abstraction reconcile is defensible; risk "low" is fair since both are pure-data configs. Files-touched is the alphabet def in each crate — narrow and implicit; acceptable. |

**Gaps / Missing Primitives table — `:107` is FALSE → REVISE (folds into COH17-005 surface).** "crates/core structural scan has NO css_l4 alphabet wired (json/ebnf/bnf/csv only)" is contradicted by `css_l4.rs:15936-15982` (`scan_structural` + `StructuralAlphabet` + `OnceCell<StructuralIndex>` all present), and `google_sheets.rs`/`bbnf.rs`/`css_pretty.rs` carry it too (`grep -rln scan_structural crates/core/src/grammar/generated/` → all 8). FIX: delete the row or restate as "CSS scan IS wired in the generated `css_l4` parser; the missing primitive is the *tape consumer*, not the scan." This mis-pricing matters to CH4 because it labels an already-paid surface as an unpaid fold cost.

### 1f-anti-pattern.md

| ID | Finding | CH4 disposition | Reason |
|---|---|---|---|
| AP17-001 | Parallel-substrate firewall: ONE unwired tape, eager builders are live | **ACCEPT** | This is a CH5-firewall finding, not a divergence with a fold-cost; CH4-cost is N/A (catalogue-only, 0 LOC). Evidence verified: `tape/mod.rs:58`, `record.rs:103`, builders at `json/builder.rs:9`/`css_l4/builder.rs:16`. No cost claim to dispute. |
| AP17-002 | Sidecar `OnceCell<StructuralIndex>` retained scan cache | **ACCEPT** | `json.rs:686,702,732` verified. The cost is correctly deferred to SK-V18 classification (index→offsets vs local_temp_only). No mis-priced LOC. |
| AP17-003 | God module: CSS builder 817 LOC, "nine `pending_*` Vecs (`:74-79`)" | **REVISE** | 817 LOC verified (`wc -l` = 817). But **"nine `pending_*` Vecs" is wrong**: lines 74-79 hold **six `Vec`** (`pending_{rules,decls,selectors,values,blocks,components}`) plus `pending_value: Option` at `:71` = seven distinct pending fields; the core mod-doc itself says "seven `pending_*`" (`tape/mod.rs:8`). FIX: correct "nine" → "seven pending fields (six `Vec` + one `Option`)". The 817-LOC god-module-deletion cost is otherwise sound. |
| AP17-004 | Renamed-scanner / cross-call classifier state: none found | **ACCEPT** | `simd-scan/src/lib.rs:80` per-call scan verified; no cross-call carry. Correctly a 0-cost null finding with a verify_action. |

### 1f-past-corpora.md

| ID | Finding | CH4 disposition | Reason |
|---|---|---|---|
| PC17-001 | REDRESS-53 parallel index pre-block | **ACCEPT** | `SPEC.md:578,:837-840` framing verified; this is a regression-guard ledger entry, no fold-cost to price. CH4 N/A. |
| PC17-002..006 | REDRESS 96/97/98 ceiling; AZ-IV eager tree; x86 not target; D6 second substrate; StructLayout/OpenFrame/CssArena totality-only | **ACCEPT** | All verified: `SPEC.md:791-793` (AZ-IV 118×), `:807-811` + `:854` (second-substrate/D6 block), `LOCKS.md:129-135`. These are do-not-redrive ledger entries — pure regression-cost-avoidance, no fold LOC to estimate. The single most cost-load-bearing fact (PC17-005: the totality `StructLayout`/`OpenFrame`/`TapeStructBuilder` names grep-ZERO on the skinny benched surface) is correctly stated and directly governs the COH17-006 mis-pricing above. CH4 contract met. |

## Structural Gap: 1A-1E Absent This Cycle

`restart/audit/totality/sk-v17/p1/` contains **only** the three 1F files; the
scope-matrix rows 1A (substrate), 1B (codegen/BackendShape), 1C (runtime
census), 1D (skinny lessons), 1E (locks + amendment candidates) are not on
disk. CH4's §3 contract explicitly includes **"1E's amendment candidates
state a wave-alignment hint; amendment candidates without supporting path:line
evidence are REVISE"** — with 1E absent, CH4 cannot disposition the
amendment-candidate cost axis at all. This is not a defect *in* the 1F files;
it is a cycle-completeness gap the orchestrator must close before convergence
(≥95% ACCEPT × 2 is unreachable while 5 of 6 inventories are missing). Flagged
to the aggregator, not charged against any 1F finding.

## CH4 Cost-Discipline Verdict

- **No speculative spec claim**: PASS. Every divergence is anchored to a
  live `crates/core` / `skinny/crates` path:line and a real SPEC/LOCKS/ARCH
  citation; none asserts a future spec shape as present.
- **LOC-delta + risk-class present**: PASS in form (all eight COH rows carry
  both); but two estimates are mis-priced (COH17-006 by ~8×; the
  css_l4-scan Gap to zero).
- **Propagation surface (files touched)**: PARTIAL. COH17-003/004/005/007/008
  carry an honest scope (or correctly defer it); COH17-001/002/006 state an
  LOC range without enumerating the known files-touched set, which the
  dispatch makes load-bearing for the fold.

REVISE count 4 (COH17-001, COH17-002, COH17-006, AP17-003 + the false Gap row
folded into the COH17-005/006 surface) clears the §3 "V1 expects ≥30% REVISE"
adversarial-floor and reflects genuine cost-pricing defects, not paper
fault-finding.

## Counts

- ACCEPT: 6  (COH17-003, COH17-004, COH17-005, COH17-007, COH17-008, +the 1f-anti-pattern AP17-001/002/004 and all 1f-past-corpora rows as a single non-cost-bearing block — counted as ACCEPT in aggregate per their N/A-cost class)
- REVISE: 4  (COH17-001 files-touched; COH17-002 files-touched; COH17-006 mis-priced 8×; AP17-003 pending-count + the false css_l4-scan Gap row)
- REJECT: 0
