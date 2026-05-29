---
lens: CH7 OVERFIT-PRUNE
pass: T-P1-excavation
cycle: V4
generated_at: 2026-05-29T18:40:00Z
reviewer: CH7 (V4)
subject_files:
  - restart/audit/totality/sk-v17/p1/1a-substrate-evidence.md
  - restart/audit/totality/sk-v17/p1/1b-codegen-evidence.md
  - restart/audit/totality/sk-v17/p1/1c-runtime-evidence.md
  - restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md
  - restart/audit/totality/sk-v17/p1/1e-locks-evidence.md
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
mandate: >
  No contrivance; the excavation must distinguish the genuinely-generalizable
  tape model from any CSS/JSON-special-cased current state; crates/core is the
  fold TARGET, not measured-as-skinny. Disposition each section
  ACCEPT/REVISE/REJECT with file:line + concrete fix.
master_head: 445925167
counts:
  accept: 8
  revise: 0
  reject: 0
sections_reviewed: 8
prior_cycle: V3
prior_cycle_revises_folded:
  - V3-3A-SoA-convergence-framing-1C-1F   # offset-tape-admissible vs AV.04-class-column distinction
  - V3-3B-math.rs-scan-over-credit-1D       # struck the +math.rs ninth-carrier inflation
---

## §0 Posture

CH7 OVERFIT-PRUNE asks one question of every catalogued divergence: does the
finding measure the *genuinely-generalizable* substrate model, or has the
inventory over-fit to the CSS/JSON-special-cased current state — by mislabelling
a grammar-neutral fact as grammar-specific, by inventing a "gap" that is a
scope/convergence question, by understating an already-general construct, or by
*over-crediting* breadth-of-config as breadth-of-proof? The complementary failure
is contrivance: a divergence dressed up as a defect when it is a fold-direction
artefact (crates/core is the fold TARGET; it is allowed to lag the SKINNY-proven
model), or a genuine spec tension *dressed away* into a clean convergence story.

This is the V4 cycle. All eight 1X inventories (1A–1F three-part) are present.
The two V3 CH7 REVISEs — §3-A (SoA-convergence framing cited the buried-SoA
clause as its own authority; 1C/1F) and §3-B (math.rs appended to a "1 per file"
scan census it has zero calls in; 1D) — are the entire residual surface CH7
carried into V4. Both are folded and re-verified live below. All citations
resolved at master HEAD `445925167` by Read + grep over the live trees; no
recalled LOC.

## §1 V3-disposition fold verification (regression firewall for CH7's own prior calls)

| V3 REVISE | Fold target | Live re-verification | Verdict |
|---|---|---|---|
| **§3-A — SoA-convergence framing cites the buried-SoA clause as its closure authority** | 1C RT17-001 + Cross-Tree Map "Runtime tape encoding" row + U-RT17-002; 1F COH17-001 + U-COH17-002 | The offset-tape-admissible-vs-AV.04-dense-class-column distinction is now drawn EXPLICITLY in every carrier. 1C RT17-001 (`1c:72`) reads "Neither is the AV.04 dense kind-partitioned class-column SoA that Lock 1's 'columnar SoA is dead' … clause bars (`LOCKS.md:75`; AV.04 spec `docs/tranches/AV/research/04-columnar-soa.md`, registered `LOCKS.md:784`)." 1F COH17-001 (`1f-coherence:76`) reads "`LOCKS.md:75` carries BOTH clauses: (a) admissibility … 'offset tape' … (b) the dead shape … the AV.04 DENSE class-column SoA, a DIFFERENT construct." U-COH17-002 (`1f-coherence:123`) restates the same split and tasks T-P2/T-P3 to distinguish offset-tape-SoA (admissible) from class-column-SoA (dead). **Independently re-verified all underlying citations live:** `skinny/.../tape/mod.rs:94-100` = `offsets: Vec<u32>` + `flag_cursors: Vec<u32>` + `flag_values: Vec<u8>` (sparse side-vectors, NOT a dense kind-partitioned class column); `LOCKS.md:75` verbatim carries both "The projection may be an offset tape …" AND "columnar SoA designed in AV.04 archaeology but never activated … Columnar SoA stays buried"; `LOCKS.md:86` "Lazy-offset tape with sparse flags is admitted"; `LOCKS.md:784` registers AV.04 as "kind-partitioned columnar SoA spec (designed, never activated)"; `docs/tranches/AV/research/04-columnar-soa.md` exists. The distinction is real and load-bearing, not a verbal patch. | **FOLDED CLEAN.** The "adopt proven SoA" candidate is now coherent: the convergence target is the admissible offset-tape; the dead shape is the AV.04 dense class-column; the two are distinct constructs and the framing no longer cites the clause that buries one to justify adopting the other. |
| **§3-B — math.rs appended to a "1 per file" scan census it has zero calls in** | 1D SK17L-008 (`1d:94`) + the do-not-redrive ledger row | The `+math.rs` parenthetical is struck. SK17L-008 now reads "json/ebnf/bnf/csv/css_l4/css_pretty/google_sheets/bbnf each carry exactly one `scan_structural` call … grep-verified `grep -c scan_structural` = 1 per file. `math.rs` is the lone generated grammar carrying the `OnceCell<StructuralIndex>` index FIELD but NO `scan_structural` call (`grep -c scan_structural math.rs` = 0 … doc-comments at `math.rs:281`,`:285`) — so the wired census is EIGHT, not nine." The V3 false-ninth-carrier is laddered into the folded ledger at `1d:33`. **Re-verified live:** `grep -c scan_structural crates/core/src/grammar/generated/math.rs` = **0**. The census is now breadth-of-fact: exactly 8 generated scan-wired grammars, math is the lone field-only no-scan generated grammar. | **FOLDED CLEAN.** The breadth over-credit (the inverse of the V2 S1.9 under-count) is corrected and the correct count cross-cited in 1C RT17-005 (`1c:76` "`math.rs` holds the field but no scan call"), 1C U-RT17-001 (`1c:116` "math.rs holds the field but does not scan"), and 1D live_truth_method (`1d:16`). |

Both V3 CH7 REVISEs are addressed with live-cited corrections; neither re-opened. No regression introduced by the V4 fold.

## §2 Section dispositions (V4 inventories)

### 1a-substrate-evidence.md — ACCEPT

The Lock-1 substrate-union spine remains OVERFIT-clean and is unchanged in
substance from the V3 ACCEPT. The genuinely-generalizable model (SoA `Tape` +
`ValueRef<'doc,'input,K,G>` + `select_classifier`) stays cleanly separated from
the CSS/JSON-special-cased current state (eager `OpenFrame`/`CssTypedValue`/
no-`value_from_ref`); crates/core is consistently the fold TARGET. Re-verified
live: `TapeStructBuilder` grep-confined to `crates/core/src/runtime/tape/{mod,
record}.rs` (zero leak outside `tape/`); `grep -rn StructLayout crates/` = **960**
exactly (SUB17-006 `1a:83`, the OVERFIT-correct *up*-calibration from the V2
40-120 LOC under-estimate, methodology reproduced byte-for-byte at HEAD
`445925167`). The V4 live_truth_method block (`1a:12`) now carries the
re-verified anchor set including `skinny tape/mod.rs:94-100/104-111/175` and
`StructLayout=960`. No contrivance; no breadth over-credit.

### 1b-codegen-evidence.md — ACCEPT

The BackendShape canon excavation, unchanged and OVERFIT-clean. BSHAPE17-003
holds: crates/core carries a *single* `EmitStrategy::StructDirect` variant
(re-verified: `enum EmitStrategy` at `crates/ir/src/registry/strategy.rs:104`,
the lone `StructDirect {` arm at `:107`), named "the SinkOnly/struct-builder
lineage" to be absorbed — not inflated into a Lock-10 violation. BSHAPE17-008's
ruling that the data-bound `builder_path`/`document_path` resolution is
Lock-14-ALLOWED (manifest DATA, not a grammar-name arm) remains the key
distinguish-generalizable-from-special-cased call, and it is correct. No
contrivance.

### 1c-runtime-evidence.md — ACCEPT (V3 §3-A REVISE folded)

The runtime census is materially sound and the single V3 OVERFIT defect (the
SoA-convergence framing) is folded clean (§1). RT17-001 (`1c:72`), the Cross-Tree
Map "Runtime tape encoding" row (`1c:84`), and U-RT17-002 (`1c:117`) now each draw
the offset-tape-admissible-vs-AV.04-dense-class-column distinction explicitly,
cite `LOCKS.md:75/:86/:784` for the admissibility side and the AV.04 spec path for
the dead side, and reserve "columnar SoA is dead" for the dense class-column shape
it actually names. The convergence target is coherent: the admissible offset-tape,
not a resurrection of the buried class-column. RT17-005 (`1c:76`) correctly states
all 8 generated grammars carry the `OnceCell<StructuralIndex>` and "`math.rs`
holds the field but no scan call" — the §3-B correction is cross-cited here. The
eager `OpenFrame` builders are correctly the fold-deletion target; RT17-007 refuses
the contrived runtime-side BackendShape leak. No contrivance.

### 1d-skinny-lessons.md — ACCEPT (V3 §3-B REVISE folded)

The JSON-empirical vs grammar-neutral split remains the strongest generality
firewall in the set, and the V3 §3-B math.rs over-credit is folded clean (§1).
SK17L-008 (`1d:94`) now states the verified EIGHT-name wired census and names
math as the lone field-carrying no-scan-call generated grammar (`grep -c
scan_structural math.rs` = 0, re-verified). SK17L-009's projection-generality
framing ("by-construction on JSON+CSS only"; `sheets_witness` a bench-surface
stub that "cannot serve as a projection exercise") holds — re-verified:
`sheets_witness` resolves to `skinny/crates/bbnf-bench/src/report.rs:800-802`
(report fields), not a grammar provider. The eager-tree (SK17L-003) and
registry-indirection (SK17L-004) pre-blocks stay anchored to the construct, not a
symbol list. The do-not-redrive ledger (`1d:33`) laddered the V3 false-ninth
correctly. No contrivance.

### 1e-locks-evidence.md — ACCEPT

The locks audit and the 5 amendment candidates remain evidence-backed and
OVERFIT-clean. The L14 row rules the per-grammar `CssStructBuilder`/`CssTypedValue`
the fold-deletion target, not a leak; the L16 row names core's multi-arch scan
"BROADER than the proven aarch64 set — a fold-scope question, not a defect"
(refusing contrivance into a Lock-16 violation). LAC-1E-SKV17-04 (`1e:170`) is the
OVERFIT-exemplary call: it re-prices the Lock-2 `StructLayout`→`Layout` reconcile
into TWO disjoint candidate paths — (a) full rename, 960-site/medium; (b)
narrow-to-side-table, ~0-LOC/low via the already-live `LayoutFacts.backend_shape`
— against the V2 conflated ~8× under-estimate, and explicitly notes the v+1 note
bars claiming Lock-2 closure by `LayoutFacts` ALONE (path-(b) is a re-scope, not a
closure). The 960 figure reproduces exactly (`grep -rc StructLayout crates/` summed
= 960 at HEAD `445925167`). Candidate ownership (5 LACs, disposition deferred to
3C) is correctly bounded. No over-fit; the cost calibration is breadth-of-fact.

### 1f-coherence-scan.md — ACCEPT (V3 §3-A REVISE folded)

Carries the same V3 §3-A fold as 1C, applied clean. COH17-001 (`1f-coherence:76`)
and U-COH17-002 (`1f-coherence:123`) now state "`LOCKS.md:75` carries BOTH clauses"
and distinguish the proven offset-tape's `offsets: Vec<u32>` + SPARSE
position-keyed `flag_cursors`/`flag_values` (re-verified at `skinny/.../tape/
mod.rs:96-98`) from the AV.04 DENSE class-column — "DIFFERENT constructs; adopting
the offset-tape does not resurrect the class-column." COH17-008 (`1f-coherence:83`)
correctly classes the richer `StructuralAlphabet` as "breadth-of-config, NOT
breadth-of-proof" (JSON/CSS-exercised-only, quote_classes doc-cited), not a
violation. COH17-007 routes the FactStream question to an UNKNOWN (U-COH17-001)
rather than contriving a contradiction. No contrivance.

### 1f-anti-pattern.md — ACCEPT

The CH5-firewall counterpart, OVERFIT-clean and unchanged. AP17-001 refuses to
contrive a Lock-1 violation from the unwired tape ("NOT a same-tree second
substrate"). AP17-003 names the 817-LOC CSS builder a fold-deletion target, not a
Lock-13 violation (re-verified: `wc -l crates/core/src/runtime/css_l4/builder.rs`
= 817; one `pending_value: Option` at `:71` + six `pending_*: Vec` at `:74-79`).
AP17-002 rules the emitter `substrate.rs` builder-path references Lock-14-ALLOWED
DATA. No contrivance.

### 1f-past-corpora.md — ACCEPT

The do-not-redrive ledger and Direction-Monotonicity Note remain the strongest
fold-target-not-skinny guardrails in the set, unchanged. PC17-006 notes the §9
"second substrate" block names `StructLayout`/`TapeStructBuilder`/`TapeCursor` as
FORBIDDEN-IN-SKINNY and forbids relocating them into skinny — the monotonic
skinny→totality direction is explicit. The DO-NOT-CARRY-UNDERCOUNT flag laddered
from the V2 COH-014 correctly pre-fences the prior false-negative. No over-fit.

## §3 Dispositions

No REVISE, no REJECT this cycle. Both V3 REVISEs are folded clean and live-verified
(§1); no new OVERFIT defect surfaced across the eight V4 inventories. The
contrivance-detection sweep over the V4-touched rows (the §3-A SoA framing in 1C/1F,
the §3-B math.rs census in 1D, and the cross-citations in 1C RT17-005/U-RT17-001)
found every change to be a breadth-of-fact correction, not a new inflation.

## §4 Counts

- Sections reviewed: 8 (1A, 1B, 1C, 1D, 1E, 1F-coherence, 1F-anti-pattern,
  1F-past-corpora).
- ACCEPT: 8 — all eight inventories. 1C and 1D carry the folded V3 REVISEs (§3-A,
  §3-B respectively), both verified clean.
- REVISE: 0.
- REJECT: 0.

## §5 OVERFIT-PRUNE verdict

The V4 inventories pass the core CH7 mandate decisively and converge. **No
contrivance**: every broader-than-proven construct (multi-arch scan, richer
`StructuralAlphabet`, AoS-vs-SoA, the 960-site StructLayout surface) is classed as
scope/convergence/config-breadth pressure or breadth-of-fact cost, never inflated
into a Lock violation. The genuinely-generalizable model (SoA-offset `Tape` +
`ValueRef<G>` + `select_classifier`) is cleanly separated from the
CSS/JSON-special-cased current state (eager `OpenFrame` + `CssTypedValue` +
no-`value_from_ref`), and crates/core is consistently the fold TARGET (every
inventory's Direction-Monotonicity discipline; PC17-006 forbids reverse-relocation;
1A/1C firewall rows name the tape "dormant fold target," not a defect).

The two V3 CH7 REVISEs are folded clean with verifiable, load-bearing distinctions
rather than verbal patches: (1) §3-A now draws the offset-tape-admissible vs
AV.04-dense-class-column-buried split with both `LOCKS.md:75` clauses cited verbatim
and the skinny `Tape`'s sparse-side-vector shape (`offsets` + `flag_cursors` +
`flag_values`, not a dense kind-partitioned class column) independently re-verified
at `skinny/.../tape/mod.rs:94-100`; (2) §3-B's math.rs false-ninth-carrier is struck
and the EIGHT-name wired census restated as breadth-of-fact (`grep -c
scan_structural math.rs` = 0 re-confirmed). Neither re-opens a REDRESS route,
requires a new BIR variant/substrate, nor disturbs the converged inventory body.

No orphan REVISE remains; no new defect surfaced. Against the V3 result (6 ACCEPT /
2 REVISE / 0 REJECT) this is a clean 8 ACCEPT / 0 REVISE / 0 REJECT — 100% ACCEPT,
clearing the ≥95% bar. Combined with the V3 result this is the second consecutive
≥95%-ACCEPT cycle for the CH7 lens; T-P1 is at the §4 convergence criterion from the
OVERFIT-PRUNE vantage.
