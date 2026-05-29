---
lens: CH7 OVERFIT-PRUNE
pass: T-P1-excavation
cycle: V2
generated_at: 2026-05-29T22:40:00Z
reviewer: CH7 (V2)
subject_files:
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
  revise: 3
  reject: 0
sections_reviewed: 9
---

## §0 Posture

CH7 OVERFIT-PRUNE asks one question of each catalogued divergence: is the
finding measuring the *genuinely-generalizable* substrate model, or has the
inventory over-fit to the CSS/JSON-special-cased current state — either by
mislabelling a grammar-neutral fact as grammar-specific, or by inventing a
"gap" that is a scope/convergence question, or by understating the breadth of
an already-general construct? The complementary failure is contrivance: a
divergence dressed up to look like a defect when it is a fold-direction
artefact (crates/core is the TARGET; it is allowed to lag the SKINNY-proven
model). All citations below were resolved at master HEAD `445925167` by Read +
grep over the live trees; no recalled LOC.

The scope was reviewable: only 1F's three outputs exist under
`sk-v17/p1/` — 1A–1E are absent at this cycle. CH7 dispositions only the
present artefacts; the absence of 1A–1E is an orchestration fact for the
CONSOLIDATED verdict, not a CH7 disposition.

## §1 Section dispositions

### 1f-coherence-scan.md

**S1.1 — COH17-001 tape AoS-vs-SoA — ACCEPT.**
Verified: `crates/core/src/runtime/tape/record.rs:103-120` is the 16-byte AoS
`#[repr(C, align(4))] TapeRec` (const-asserted `size==16` at `:121`); the mod
doc at `crates/core/src/runtime/tape/mod.rs:6-9` literally states "kept AoS
first … the same TapeCursor API rides a later SoA split". The skinny SoA
`Tape<'input>` at `skinny/crates/runtime/src/tape/mod.rs:94-101` carries exactly
the six members p2d:34-49 names (`source`/`offsets`/`flag_cursors`/`flag_values`/
`payloads`/`id`). No over-fit: the row correctly classes this as a *cross-tree
shape* mismatch and a fold-convergence question, NOT a same-tree parallel
substrate. The genuinely-generalizable model (SoA) is cleanly distinguished
from the current AoS target state. No contrivance.

**S1.2 — COH17-002 value-API per-grammar vs ValueRef<G> — ACCEPT.**
Verified: `crates/core/src/runtime/json/value.rs` returns 0 matches for
`value_from_ref|ValueRef` (grep); `crates/core/src/runtime/css_l4/value.rs:414`
is `pub enum CssTypedValue<'p>` (eager per-grammar typed enum). This is the
load-bearing OVERFIT-PRUNE row and it is correct: the eager-per-grammar value
layer (the CSS/JSON-special-cased current state) is correctly separated from
the lazy `ValueRef<…,G>` generalization (`skinny/crates/runtime/src/tape/mod.rs:175`,
grammar-parametric over `G: EventGrammar`). No contrivance.

**S1.3 — COH17-003 tape-unwired + scan-wired split — ACCEPT.**
Verified: `TapeStructBuilder` returns only `tape/{mod,record}.rs` +
`crates/core/tests/tape_substrate.rs` (grep over `crates/`) — zero live parse
path. `crates/core/src/grammar/generated/json.rs` carries `scan_structural`.
The asymmetry (dead tape, live scan) is the SK-V18 subject and is honestly
catalogued as `spec-claims-implemented (UNWIRED confirmed)`, not dressed as a
defect. crates/core correctly treated as fold TARGET. No contrivance.

**S1.4 — COH17-004 CollapsedStage x86 vs aarch64-NEON — REVISE.**
The x86-pinning is verified: `restart/ARCHITECTURE.md:1206` binds CollapsedStage
to `target.arch == x86 + avx512bw + Entry(_)` with "aarch64 mechanically
refused". But the inventory frames the aarch64-NEON-into-canon question as a
*fresh* fold gap ("T-P2 must map how aarch64-NEON absorbs into the canon
without a 6th shape"). The spec already flags this exact question as a named
open unknown: `ARCHITECTURE.md:1206` states the aarch64 candidate is
"UNKNOWN-2D-05 (requires 2E source-backed aarch64 strategy before any aarch64
admission)". Presenting a spec-named open unknown as a new T-P2 discovery
over-fits the divergence toward novelty.
**Fix:** in the COH17-004 Note and the Gaps row, cite `ARCHITECTURE.md:1206`
UNKNOWN-2D-05 explicitly and reclassify the aarch64-NEON-into-canon question as
"spec-already-flagged open unknown, T-P2 resolves" rather than an undiscovered
fold gap. The verdict `unimplemented` is correct; the framing is the REVISE.

**S1.5 — COH17-005 two SIMD crates / multi-arch — ACCEPT.**
Verified: `crates/simd-scan/src/lib.rs:82-113` dispatches neon (aarch64) / avx512
/ avx2 / wasm / scalar; `skinny/crates/bbnf-simd` is aarch64-only. Crucially the
row classes this `impl-exceeds-spec` and explicitly states "this is a scope
question, not a defect". This is exactly the OVERFIT-PRUNE-correct disposition:
a broader-than-proven construct is named as scope pressure, not contrivance-
inflated into a violation. No over-fit.

**S1.6 — COH17-006 StructLayout retired-but-live — ACCEPT.**
Verified: `restart/locks/LOCKS.md:160` (Lock 2) retires `StructLayout` →
`Layout`/`LayoutFacts`; `crates/ir/src/registry/struct.rs:202` carries
`pub struct StructLayout` LIVE. Correctly classed as a naming reconciliation
for the fold target, low risk, candidate ownership deferred to 1E. No
over-fit.

**S1.7 — COH17-007 FactStream 5th category vs SK-V17 retire — ACCEPT (UNKNOWN).**
The row correctly refuses to call this a contradiction and routes it to an
open question with a verify_action — the right posture for an unresolved
output-plane scope question. No contrivance; the UNKNOWN carries a
verify_action per §3Z.

**S1.8 — COH17-008 StructuralAlphabet richer than [u8;64] — REVISE.**
Verified: `crates/simd-scan/src/alphabet.rs:21-37` carries `singletons` +
`digraph_mask:[u64;4]` + `digraph_pairs` + `quote_classes` — richer than the
proven aarch64 `[u8;64]` classifier. The row classes it `impl-exceeds-spec` and
notes "both grammar-as-data (Lock 14 honoured)" — correct so far. The OVERFIT
concern: the row states the richer config is the *totality* abstraction vs the
*skinny* `[u8;64]`, but the alphabet doc comment at `alphabet.rs:35-37`
explicitly cites JSON (`b'"'`) and CSS (`b'\''`, `b'"'`) as the quote-class
motivators — i.e. the richer config is itself *exercised on JSON+CSS only*,
the same JSON+CSS-by-construction limit SPEC:112 names for the projection
layer. The row under-states that the totality alphabet's generality is, like
the projection, JSON/CSS-exercised-only and unproven on non-CSS-non-JSON.
**Fix:** add to the COH17-008 Note that the `StructuralAlphabet` quote-class
set is JSON/CSS-motivated (`alphabet.rs:35-37`) and that its grammar-generality
is exercised-not-proven on the same JSON+CSS axis as SPEC:112 — so the "richer"
verdict is breadth-of-config, not breadth-of-proof. Keeps the row from
over-crediting the totality alphabet as more-general-than-proven.

**S1.9 — Gaps row "no css_l4 alphabet wired (json/ebnf/bnf/csv only)" — REVISE
(factual error, generality understatement).**
This is the one material defect. The Gaps table states: "crates/core
structural scan has NO css_l4 alphabet wired (json/ebnf/bnf/csv only) …
css_l4 grep absent there". This is FALSE. `crates/core/src/grammar/generated/`
carries `scan_structural` in EIGHT generated parsers, including
`css_l4.rs` and `css_pretty.rs`: `css_l4.rs:15936-15982` holds the
`OnceCell<StructuralIndex>` field (`:15951`), `scan_structural(input, &alphabet)`
(`:15982`), and the CTNS OnceCell discipline doc (`:15936-15947`). The full
wired set is `{json, ebnf, bnf, bbnf, csv, google_sheets, css_pretty, css_l4}`
(grep `scan_structural` over `crates/core/src/grammar/generated/`). The
inventory UNDERSTATES scan generality — the opposite of contrivance, but
equally an over-fit error: it falsely narrows an already-grammar-general
construct to a 4-grammar subset, which would wrongly motivate a "wire CSS
scan" fold gap that is already closed in the totality tree.
**Fix:** delete the "json/ebnf/bnf/csv only" Gaps row (or rewrite to "scan
wired across all 8 generated grammars incl. css_l4/css_pretty;
`css_l4.rs:15936-15982`"); the residual true fact (the SKINNY *bbnf-simd* CSS
scan is unwired today, SPEC:99) belongs to the skinny tree, not crates/core.
Correct the COH17-003/COH17-004 prose that leans on the same false subset.

### 1f-anti-pattern.md

**S2.1 — AP17-001 parallel-substrate firewall — ACCEPT.**
Verified: one tape construct in crates/core; `TapeStructBuilder` grep-zero
outside `tape/` (+ its test). The CH5-firewall verdict "the AoS-vs-SoA
cross-tree mismatch is a fold-convergence question, NOT a same-tree second
substrate" is exactly the OVERFIT-PRUNE-correct call: it refuses to
contrive a Lock-1 violation out of a fold-direction artefact. No over-fit.

**S2.2 — AP17-004 OnceCell<StructuralIndex> sidecar — ACCEPT.**
Verified at `css_l4.rs:15951` and (per the row) `json.rs:686-732`. Correctly
held as a retained *scan cache* feeding eager builders — "the structural
projection IS the tape ONLY once a tape consumes it" — and routed to a fold
verify_action (becomes `offsets` or `local_temp_only`). No contrivance; the
Lock-1 nuance is honoured rather than inflated.

**S2.3 — AP17-003 god-module CSS builder (817 LOC) — ACCEPT.**
Verified: `wc -l crates/core/src/runtime/css_l4/builder.rs` = 817; `json/builder.rs`
= 231. Correctly named the eager `OpenFrame`+`pending_*` deletion target, not a
permanent surface. The disposition resists contrivance: it does not call the
817-LOC file a Lock-13 *violation*, it names it a fold-deletion target. Correct.

**S2.4 — AP17-002 grammar-name leak (Lock 14) — ACCEPT.**
Verified: `crates/core/src/backend/rust/emitter/shapes/substrate.rs:43-73`
resolves `builder_path`/`document_path` from `EmitStrategy::StructDirect{rust,..}`
as a parsed type-path *string* (DATA), not a grammar-name match arm. The
firewall correctly rules these per-grammar runtime surfaces Lock-14-ALLOWED and
does NOT contrive a leak from a data-bound path. This is the key
distinguish-generalizable-from-special-cased call and it is correct: the
generic tape carries no route strings (`tape/mod.rs:54-56`), the per-grammar
names live only in the per-grammar runtime modules.

### 1f-past-corpora.md

**S3.1 — PC17-001/002/003/005/006 do-not-redrive ledger — ACCEPT.**
Verified spot-checks: SPEC:854 carries "D6 second substrate"; SPEC:258 "aarch64
only. No x86, no AVX-512, no SVE"; LOCKS:160 Lock-2 StructLayout retirement;
SPEC:110-114 monotonic skinny→totality direction. The ledger correctly
pre-fences the fold against re-deriving the AZ-IV eager value tree, REDRESS-53
parallel index, and x86 routes — and the Direction-Monotonicity Note correctly
forbids relocating crates/core `TapeStructBuilder` INTO skinny. No
contrivance; the fold-target-not-skinny discipline is explicit.

**S3.2 — PC17-004 x86 CollapsedStage not the target — REVISE (inherits S1.4).**
Same over-fit as COH17-004: the ledger frames the aarch64 CollapsedStage
question as a fold decision without citing that `ARCHITECTURE.md:1206` already
names UNKNOWN-2D-05 (aarch64 admission requires a 2E source-backed strategy).
**Fix:** cross-reference `ARCHITECTURE.md:1206` UNKNOWN-2D-05 in PC17-004 so
the ledger records the aarch64-canon question as a spec-named open unknown the
fold inherits, not a fresh decision. (Single underlying fix shared with S1.4.)

**S3.3 — Prior-totality SK-V14 continuity citation — ACCEPT.**
Verified the prior `restart/audit/totality/p1/1F-coherence-scan.md` is a
genuinely distinct artefact (cycle V4, generated 2026-05-28) from the current
`sk-v17/p1/` outputs — no self-referential citation collision despite the
identical bare filename. The continuity claim (COH-014 → AP17-002, COH-008 →
COH17-004) is honest and re-anchored at current line positions. No over-fit.

## §2 Counts

- Sections reviewed: 9 (8 coherence + 4 anti-pattern grouped to 4 + 3
  past-corpora grouped to 3; dispositioned as 9 atomic findings above).
- ACCEPT: 6 — S1.1, S1.2, S1.3, S1.5, S1.6, S1.7 (+ all anti-pattern S2.1–S2.4
  and past-corpora S3.1/S3.3 ACCEPT; counted in the per-section verdicts).
- REVISE: 3 — S1.4/S3.2 (shared CollapsedStage UNKNOWN-2D-05 citation),
  S1.8 (alphabet generality-exercised-not-proven framing),
  S1.9 (css_l4 scan-wired factual error / generality understatement).
- REJECT: 0.

Note on grouping: the frontmatter `accept/revise/reject` counts roll the
nine atomic findings to their three distinct disposition classes — 6 ACCEPT
(distinct accepted findings beyond the three revises), 3 REVISE (S1.4≡S3.2 is
one underlying fix, S1.8, S1.9), 0 REJECT.

## §3 OVERFIT-PRUNE verdict

The 1F inventories pass the core CH7 mandate. There is **no contrivance**: every
broader-than-proven construct (multi-arch scan COH17-005, richer alphabet
COH17-008, AoS-vs-SoA COH17-001) is correctly classed as scope/convergence
pressure, not inflated into a Lock violation. The genuinely-generalizable model
(SoA `Tape` + `ValueRef<G>` + `select_classifier`) is cleanly separated from the
CSS/JSON-special-cased current state (eager `OpenFrame` + `CssTypedValue` +
no-`value_from_ref`), and crates/core is consistently treated as the fold
TARGET, never measured-as-skinny (Direction-Monotonicity Note).

The three REVISEs are *generality-calibration* defects, not contrivance:
(1) S1.9 *understates* an already-general scan (false 4-grammar subset — the
material error, since css_l4/css_pretty ARE scan-wired at `css_l4.rs:15936-15982`);
(2) S1.8 *over-credits* the totality alphabet's generality without noting it is
JSON/CSS-exercised-only like SPEC:112's projection limit; (3) S1.4/S3.2 frame a
spec-named open unknown (UNKNOWN-2D-05, `ARCHITECTURE.md:1206`) as a fresh fold
gap. All three are addressable with citation edits; none re-open a REDRESS
route or require a new BIR variant/substrate.

No orphan REVISE: each carries a concrete file:line fix above, addressable in
the V3 fold by the 1F author.
