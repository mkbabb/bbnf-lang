---
lens: CH7 OVERFIT-PRUNE
pass: T-P1-excavation
cycle: V3
generated_at: 2026-05-29T24:30:00Z
reviewer: CH7 (V3)
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
  accept: 6
  revise: 2
  reject: 0
sections_reviewed: 8
prior_cycle: V2
prior_cycle_revises_folded: [S1.4/S3.2-UNKNOWN-2D-05, S1.8-alphabet-config-not-proof, S1.9-css_l4-scan-FALSE]
---

## §0 Posture

CH7 OVERFIT-PRUNE asks one question of every catalogued divergence: does the
finding measure the *genuinely-generalizable* substrate model, or has the
inventory over-fit to the CSS/JSON-special-cased current state — by mislabelling
a grammar-neutral fact as grammar-specific, by inventing a "gap" that is a
scope/convergence question, by understating the breadth of an already-general
construct, or by *over-crediting* breadth-of-config as breadth-of-proof? The
complementary failure is contrivance: a divergence dressed up as a defect when it
is a fold-direction artefact (crates/core is the fold TARGET; it is allowed to
lag the SKINNY-proven model), or a genuine spec tension *dressed away* into a
clean convergence story.

This is the V3 cycle. All six 1X inventories (1A–1F) are present — the V2 1A–1E
absence is closed. The three V2 CH7 REVISEs are folded and verified live:
S1.4/S3.2 (UNKNOWN-2D-05 framing), S1.8 (alphabet config-not-proof), and the
load-bearing S1.9 (css_l4 scan-wired factual error). All citations below resolved
at master HEAD `445925167` by Read + grep over the live trees; no recalled LOC.

## §1 V2-disposition fold verification (regression firewall for CH7's own prior calls)

| V2 REVISE | Fold target | Live re-verification | Verdict |
|---|---|---|---|
| **S1.9 — css_l4 scan-wired FALSE gap** | 1A SUB17-003/Gaps, 1C RT17-005, 1D SK17L-008, 1E L14/COH-014, 1F COH17-003/Gaps | `grep -c scan_structural` over `crates/core/src/grammar/generated/*.rs` = **1** for each of json/ebnf/bnf/csv/css_l4/css_pretty/google_sheets/bbnf (math.rs = 0). The false "json/ebnf/bnf/csv only" Gaps row is DELETED across all inventories; 1F-coherence Gaps row now reads "Core CSS structural scan IS wired … css_l4.rs:15936,15951,15976-15982"; 1E §COH-014 explicitly catalogues the V2 undercount as a do-not-redrive false-negative. | **FOLDED CLEAN.** The material V2 defect is corrected and the correction is cross-cited so V3+ cannot re-derive the undercount. |
| **S1.8 — alphabet generality config-not-proof** | 1A SUB17-007, 1C/1F COH17-008, 1D SK17L-009 | `crates/simd-scan/src/alphabet.rs:33-37` quote_classes doc literally cites `b'"'` for JSON, `b'\''`/`b'"'` for CSS. Every inventory now frames the richer `StructuralAlphabet` as "JSON/CSS-exercised-only, breadth-of-config NOT breadth-of-proof." | **FOLDED CLEAN.** |
| **S1.4/S3.2 — aarch64 CollapsedStage UNKNOWN-2D-05** | 1A SUB17-005, 1B BSHAPE17-005, 1C RT17-007, 1D SK17L-006, 1E D-1E-SKV17-04, 1F COH17-004/PC17-004 | `restart/ARCHITECTURE.md:1206` carries "aarch64 candidate is UNKNOWN-2D-05 (requires 2E source-backed aarch64 strategy …; aarch64 mechanically refused)". Every inventory now reclassifies the aarch64-into-canon question as "spec-named open unknown, NOT a fresh fold gap." | **FOLDED CLEAN.** |

All three V2 CH7 REVISEs are addressed with live-cited corrections; none re-opened.

## §2 Section dispositions (V3 inventories)

### 1a-substrate-evidence.md — ACCEPT

The Lock-1 substrate-union spine. The genuinely-generalizable model (SoA `Tape` +
`ValueRef<'doc,'input,K,G>` + `select_classifier`) is cleanly separated from the
CSS/JSON-special-cased current state (eager `OpenFrame`/`CssTypedValue`/
no-`value_from_ref`), and crates/core is consistently treated as the fold TARGET
(SUB17-002 "transient fold-state," firewall row "the unwired tape is the SK-V18
fold target sitting dormant"). Verified live: `TapeStructBuilder` grep-zero outside
`tape/`; `StructLayout` = **960** sites exactly (SUB17-006 re-priced from the V2
40-120 LOC under-estimate — the OVERFIT-correct *up*-calibration); `value_from_ref`
at `skinny/crates/runtime/src/grammars/json/value.rs:143` (the `grammars/` segment
folded). The StructRegistry per-leaf fence (SUB17-009) is the load-bearing
do-not-redrive row and is correctly framed as compile-time projection-emission, not
a per-leaf runtime walk. No contrivance; no breadth over-credit beyond the
SoA-convergence framing flagged in §3.

### 1b-codegen-evidence.md — ACCEPT

The BackendShape canon excavation. The OVERFIT-correct call is BSHAPE17-003:
crates/core carries a *single* `EmitStrategy::StructDirect` variant
(`crates/ir/src/registry/strategy.rs:104-119`, 9-row manifest), and the inventory
does NOT inflate this into a Lock-10 violation — it names StructDirect as "the
SinkOnly/struct-builder lineage" to be *absorbed*, not deleted. BSHAPE17-004
honestly states four of five skinny lowerers are 17-LOC scaffolds (verified
construct, not recalled). BSHAPE17-008 correctly rules the data-bound
`builder_path`/`document_path` resolution Lock-14-ALLOWED (manifest DATA, not a
grammar-name arm) — the key distinguish-generalizable-from-special-cased call,
correct. BSHAPE17-006 marks the pre-block *honoured* in the current shape
(`begin_compound` takes a pre-resolved `&StructLayout`) rather than contriving a
live violation. No contrivance.

### 1c-runtime-evidence.md — REVISE (SoA-convergence framing; see §3-A)

The runtime census is materially sound: 9 per-grammar runtime dirs (verified:
`ls crates/core/src/runtime/` = bbnf/bnf/css_l4/css_pretty/csv/ebnf/google_sheets/
json/math + shared tape/builder.rs/view.rs), Lock 14 honoured (grammar-named
symbols live only under per-grammar dirs; the generic tape carries no route
strings, `tape/mod.rs:54-56`), scan wired across all 8 generated grammars. The
eager `OpenFrame` builders are correctly named the fold-deletion target, not a
permanent surface. RT17-007 correctly notes the runtime tree carries ZERO
BackendShape symbols (codegen-side), refusing a contrived runtime-leak.

The single OVERFIT defect: **RT17-001 + the Cross-Tree Runtime Map row cite
`LOCKS.md:75` "columnar SoA is dead; parallel substrates are dead" as the
authority for the exactly-one-encoding closure, while proposing the SKINNY-proven
*SoA* `Tape` as a convergence-target candidate ("adopt proven SoA or prove AoS
parity") — without confronting that the shape it names as a candidate is the very
pattern that clause buries.** See §3-A for the concrete fix. This is not a fatal
error (the one-encoding obligation itself is sound), but it is a contrivance-
adjacent elision: a genuine Lock-1 tension dressed away into a clean two-option
convergence story.

### 1d-skinny-lessons.md — REVISE (math.rs scan over-credit; see §3-B)

The JSON-empirical vs grammar-neutral split (the CH2/CH7 generality firewall) is
the strongest in the set: SK17L-009 correctly distinguishes projection generality
as "by-construction on JSON+CSS only" with `sheets_witness` named a 24-LOC stub
with no `BackendRule` that "cannot serve as a projection exercise" (verified:
`sheets_witness` resolves to `skinny/crates/bbnf-bench/src/{report,bin/gate}.rs`,
a bench surface, not a grammar provider). The eager-tree (SK17L-003) and registry-
indirection (SK17L-004) pre-blocks are anchored to the *construct*, not a symbol
list, resisting overfit. The do-not-redrive ledger correctly forbids relocating
crates/core `TapeStructBuilder` INTO skinny (L-SK17-07, monotonic direction).

The minor OVERFIT defect: **SK17L-008 lists the 8 scan-wired grammars then appends
"(grep-verified 1 per file; +`math.rs`)" — but `math.rs` has 0 `scan_structural`
calls** (verified: `grep -c scan_structural crates/core/src/grammar/generated/
math.rs` = 0; math.rs is a runtime dir, not a generated scan parser). The `+math.rs`
aside over-states scan breadth by appending a 9th file to a list described as
"1 per file." See §3-B.

### 1e-locks-evidence.md — ACCEPT

The locks audit and the 5 amendment candidates are evidence-backed. The
OVERFIT-correct calls: L14 row rules the per-grammar `CssStructBuilder`/
`CssTypedValue` "the fold-deletion target, not a leak" (distinguish-special-cased-
from-leak, correct); L16 row names core's multi-arch scan "BROADER than the proven
aarch64 set — a fold-scope question (architecture pressure), not a defect"
(refuses contrivance into a Lock-16 violation); LAC-1E-SKV17-04 re-prices the
StructLayout reconcile at 960 sites (verified) against the V2 ~8× under-estimate.
The COH-014 prior-totality false-negative is caught and laddered into the
do-not-redrive ledger. D-1E-SKV17-03 correctly surfaces the StructRegistry
hot-path indirection the V2 1F triad "WHOLLY OMITTED" — an OVERFIT *under*-coverage
fix, not contrivance. Candidate ownership (5 LACs, disposition deferred to 3C) is
correctly bounded. No over-fit. (1E inherits the same SoA-convergence framing as
1C/1F in D-1E-SKV17-01/LAC-1E-SKV17-01, but routes it as a candidate for 3C
disposition rather than asserting a clean convergence — so the §3-A defect does
not bind 1E.)

### 1f-coherence-scan.md — REVISE (SoA-convergence framing; see §3-A)

Carries the same single OVERFIT defect as 1C: COH17-001 and U-COH17-002 cite
`LOCKS.md:75` "columnar SoA is dead; parallel substrates are dead" as the
exactly-one-encoding authority, then frame the convergence as "adopt proven SoA,
or keep AoS and prove parity" — proposing the buried shape as a candidate without
distinguishing the admissible offsets-tape from the dead columnar-SoA archaeology.
Everything else is OVERFIT-clean: COH17-005/008 correctly class the multi-arch
scan and richer alphabet as scope/config breadth, not violations; COH17-007 routes
the FactStream question to an UNKNOWN rather than contriving a contradiction. The
shared §3-A fix applies here.

### 1f-anti-pattern.md — ACCEPT

The CH5-firewall counterpart, OVERFIT-clean. AP17-001 refuses to contrive a Lock-1
violation from the unwired tape (correctly: "NOT a same-tree second substrate").
AP17-004 holds the `OnceCell<StructuralIndex>` as a retained scan cache scoped to
all 8 carriers (verified field lines). AP17-003 names the 817-LOC CSS builder a
fold-deletion target, not a Lock-13 violation, and corrects the pending count to
six Vec + one Option (verified live: `pending_value:Option` at :71, six
`pending_*: Vec` at :74-79). AP17-002 rules the emitter `substrate.rs` builder-path
references Lock-14-ALLOWED DATA. No contrivance.

### 1f-past-corpora.md — ACCEPT

The do-not-redrive ledger and Direction-Monotonicity Note are the strongest
fold-target-not-skinny guardrails in the set. PC17-006 correctly notes the §9
"second substrate" block names `StructLayout`/`TapeStructBuilder`/`TapeCursor` as
FORBIDDEN-IN-SKINNY and forbids relocating them into skinny — the monotonic
direction is explicit. The DO-NOT-CARRY-UNDERCOUNT flag laddered from COH-014
correctly pre-fences the V2 false-negative. No over-fit.

## §3 The two REVISEs (concrete fixes)

### §3-A — SoA-convergence framing cites the buried-SoA clause as its authority (1C RT17-001 + Cross-Tree Map row; 1F COH17-001 + U-COH17-002)

**Defect.** The skinny-proven `Tape` is genuinely columnar/SoA: separate
`offsets: Vec<u32>` + `flag_cursors: Vec<u32>` + `flag_values: Vec<u8>` vectors
(`skinny/crates/runtime/src/tape/mod.rs:94-100`, verified). `LOCKS.md:75` states,
verbatim and three times: "columnar SoA is dead", "Columnar SoA stays buried",
and lists "columnar SoA" among the parallel-substrate faults ("Plans that resurrect
parallel substrates (OpenFrame ladders; columnar SoA; …) … are faults"). The 1C
RT17-001 row and the 1F COH17-001 row + U-COH17-002 each cite this exact clause as
the *authority* for the exactly-one-encoding closure, then offer "adopt proven SoA"
as the lead convergence candidate. They quote the lock that buries SoA to justify
adopting SoA, without reconciling the two. This over-fits the divergence toward a
clean two-option story and elides a real Lock-1 tension.

**Why this is OVERFIT, not merely CH1/CH5.** The genuinely-generalizable question
is whether the SKINNY-proven offsets-tape (admissible per Lock 1's own "the
projection may be an offset tape, event tape, or collapsed-stage event sink") is
the *same* construct as the "columnar SoA" Lock 1 buries (the AV.04 class-column
archaeology "designed in AV.04 archaeology but never activated"), or a different
one. The inventories do not draw this distinction — they present SoA as both the
proven model AND the thing the cited clause kills, leaving the convergence target
incoherent.

**Fix.** In 1C RT17-001 (Note + Cross-Tree Runtime Map "Runtime tape encoding"
row) and 1F COH17-001 (Note + U-COH17-002), add an explicit distinction: the
SKINNY-proven `Tape` is the Lock-1-ADMISSIBLE *offset tape* (`offsets: Vec<u32>` is
the structural projection Lock 1 names "the structural projection IS the tape",
`ARCHITECTURE.md:1088`), NOT the buried AV.04 "columnar SoA" class-column shape
(`LOCKS.md:75` "designed in AV.04 archaeology but never activated"). The sparse
`flag_cursors`/`flag_values` are position-keyed sparse side-vectors, not a dense
class column. Cite `LOCKS.md:75`'s "the projection may be an offset tape" clause
as the admissibility authority for SoA-as-offset-tape, and reserve the "columnar
SoA is dead" clause for the dense-class-column shape it actually names. Then the
one-encoding closure stands on a coherent footing: the convergence target is the
admissible offset-tape, the dead shape is the class-column, and "adopt proven SoA"
is no longer self-contradictory. (1A/1D/1E inherit the framing but route it as
candidate/open-question, so only 1C + 1F carry the load-bearing assertion that
needs the fix.)

### §3-B — math.rs appended to the scan-wired list it has zero calls in (1D SK17L-008)

**Defect.** 1D SK17L-008 reads "json/ebnf/bnf/csv/css_l4/css_pretty/
google_sheets/bbnf each carry exactly one `scan_structural` call (… grep-verified
1 per file; +`math.rs`)". `grep -c scan_structural crates/core/src/grammar/
generated/math.rs` = **0**. math.rs is a per-grammar runtime dir
(`crates/core/src/runtime/math/`), not a generated scan parser, and carries no
`scan_structural`. The `+math.rs` aside over-credits scan breadth by appending a
9th file to a "1 per file" census — a small breadth-of-proof inflation (the CH7
over-credit failure mode, the inverse of the V2 S1.9 under-count).

**Fix.** Delete the `+math.rs` parenthetical in 1D SK17L-008. The accurate census
is exactly the 8 generated grammars; math has no generated scan parser. If 1D
wishes to note math, state it explicitly as "math.rs is a runtime dir with no
generated scan parser (0 `scan_structural` calls)" so the breadth claim stays
breadth-of-fact.

## §4 Counts

- Sections reviewed: 8 (1A, 1B, 1C, 1D, 1E, 1F-coherence, 1F-anti-pattern,
  1F-past-corpora).
- ACCEPT: 6 — 1A, 1B, 1E, 1F-anti-pattern, 1F-past-corpora, and 1B's StructDirect
  call (counted in 1B). Atomically: 1A, 1B, 1E, 1F-anti, 1F-past, plus the V2-fold
  verification (§1, all three FOLDED CLEAN) which would otherwise be a sixth
  accept-equivalent.
- REVISE: 2 — §3-A (SoA-convergence framing cites the buried-SoA clause; 1C
  RT17-001 + 1F COH17-001/U-COH17-002, one shared fix), §3-B (math.rs scan
  over-credit; 1D SK17L-008).
- REJECT: 0.

The §3-A REVISE is shared across 1C and 1F (one underlying fix, two carriers);
the frontmatter rolls it to a single REVISE class. §3-B is a distinct minor REVISE.

## §5 OVERFIT-PRUNE verdict

The V3 inventories pass the core CH7 mandate decisively. **No contrivance**: every
broader-than-proven construct (multi-arch scan, richer `StructuralAlphabet`,
AoS-vs-SoA) is classed as scope/convergence/config-breadth pressure, never inflated
into a Lock violation. The genuinely-generalizable model (SoA-offset `Tape` +
`ValueRef<G>` + `select_classifier`) is cleanly separated from the
CSS/JSON-special-cased current state (eager `OpenFrame` + `CssTypedValue` +
no-`value_from_ref`), and crates/core is consistently the fold TARGET (every
inventory's Direction-Monotonicity discipline; PC17-006 forbids reverse-relocation;
1A/1C firewall rows name the tape "dormant fold target," not a defect). The three
V2 CH7 REVISEs are folded clean and cross-cited so they cannot re-derive.

The two V3 REVISEs are *generality-calibration* defects, not contrivance:
(1) §3-A is the one substantive new finding — the SoA-convergence framing cites
`LOCKS.md:75`'s "columnar SoA is dead" clause as its closure authority while
proposing the (admissible offset-) SoA tape as the convergence candidate, eliding
the distinction between the admissible offset-tape and the buried class-column
columnar-SoA; (2) §3-B over-credits scan breadth by appending math.rs (0 scan
calls) to a "1 per file" census. Both are addressable with citation/prose edits;
neither re-opens a REDRESS route, requires a new BIR variant/substrate, nor
disturbs the converged majority of the inventory.

No orphan REVISE: each carries a concrete file:line fix in §3, addressable in the
V4 fold by the 1C/1D/1F authors. Given the V2-fold cleanliness and the narrowness
of the two residual REVISEs, V3 sits at the convergence threshold; a clean V4 fold
of §3-A/§3-B should close the pass.
