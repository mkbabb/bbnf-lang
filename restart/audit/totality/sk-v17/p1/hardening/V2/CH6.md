# CH6 — ANTI-PAPER-CLOSE (V2) — SK-V17 T-P1 EXCAVATION

```yaml
lens: CH6-ANTI-PAPER-CLOSE
pass: T-P1-SKV17-excavation
cycle: V2
generated_at: 2026-05-29T22:40:00Z
reviewer: CH6
master_head: 445925167154de73540e3ea3283d0170371de790
subjects_reviewed:
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
subjects_absent:
  - 1a-substrate-evidence.md   # NOT WRITTEN for sk-v17/p1
  - 1b-codegen-evidence.md     # NOT WRITTEN for sk-v17/p1
  - 1c-runtime-evidence.md     # NOT WRITTEN for sk-v17/p1
  - 1d-skinny-lessons.md       # NOT WRITTEN for sk-v17/p1
  - 1e-locks-evidence.md       # NOT WRITTEN for sk-v17/p1
live_truth_method: "wc -l + grep/sed path:line over crates/core/src/runtime/tape/{mod,record,cursor,arena}.rs, crates/core/src/runtime/{json,css_l4}/{builder,value,view,parse_with}.rs, crates/ir/src/registry/struct.rs, crates/simd-scan/src/{lib,alphabet}.rs, crates/core/src/grammar/generated/*.rs, crates/core/src/backend/rust/emitter/shapes/substrate.rs, skinny/crates/runtime/src/tape/mod.rs, skinny/crates/bbnf-simd/src/dispatch.rs; sed over restart/{ARCHITECTURE,MASTER-PLAN}.md, restart/locks/LOCKS.md, restart/skinny/tranches/sk-v17/SPEC.md + research/{alpha,p2}; git rev-parse HEAD"
counts:
  sections_dispositioned: 7
  accept: 4
  revise: 2
  reject: 1
accept_rate: 0.571
```

## Lens charge

CH6 ANTI-PAPER-CLOSE: no "excavated / resolved / wired" claim stands
without a live-evidence citation (cargo asm symbol, bench row, REDRESS
admit, file:line); no divergence is deferred to "a later inventory" or
"T-P2 will find" in place of present ground truth; every UNKNOWN carries
a verify_action. Ground truth is concrete, resolved to file:line / SHA.

## Verification posture

This wave is NOT a paper-close on the subject. Every load-bearing
citation in all three 1F artefacts was re-resolved against live code at
master `445925167`:

- `crates/core/src/runtime/tape/mod.rs:6-9,54-56,58,185` — AoS-first doc,
  `begin_compound(&StructLayout)`, `TapeStructBuilder` — **resolve.**
- `record.rs:103,120` (`TapeRec` + 16-byte const-assert),
  `skinny/crates/runtime/src/tape/mod.rs:94,175` (`Tape<'input>` SoA +
  `ValueRef<'doc,'input,K,G>`) — **resolve.**
- `crates/ir/src/registry/struct.rs:84,202` (`FieldSource`, `StructLayout`)
  — **resolve;** Lock 2 retirement at `LOCKS.md:160` — **resolves.**
- `crates/simd-scan/src/lib.rs:80` (`scan_structural(&StructuralAlphabet)`),
  `alphabet.rs:19-37` (digraph/quote/kernel-shape config) — **resolve.**
- `ARCHITECTURE.md:1088,1206` (CollapsedStage AVX-512 + "aarch64
  mechanically refused") — **resolve verbatim.**
- `TapeStructBuilder` grep-zero outside `crates/core/src/runtime/tape/` —
  **CONFIRMED** (0 hits); `value_from_ref`/`ValueRef` grep-empty in
  `crates/core/src/runtime/json/value.rs` — **CONFIRMED;** CSS builder
  817 LOC, JSON builder 231 LOC — **CONFIRMED exact.**
- SPEC `:110-114,:258,:806,:807-811,:825,:837-840,:854` and alphaC
  `:20-29,:307-316` — **resolve verbatim.**

The artefacts are, by excavation standards, unusually well-grounded:
near-zero recalled LOC. CH6 therefore disputes only the small set of
claims that fail the live check or that defer ground truth — and ONE of
those fails hard.

---

## Dispositions

### §1 — 1f-coherence-scan: "CSS structural scan NOT wired in crates/core" → **REJECT**

**Location.** `1f-coherence-scan.md:107` (Gaps / Missing Primitives table,
row 4):
> "crates/core structural scan has NO css_l4 alphabet wired (json/ebnf/bnf/csv
> only). … generated css_l4 in crates/core not in the scan-wired set
> (`…/{json,ebnf,bnf,csv}.rs:*` carry `scan_structural`); **css_l4 grep absent
> there**."

Reinforced in the Cross-Tree map `:84` ("WIRED into generated
json/ebnf/bnf/csv") and the receiver column defers CSS scan wiring to
"SK-V17 W3 wires CSS scan in skinny; core fold later."

**Live evidence contradicts the claim.** `scan_structural` IS wired in
`crates/core/src/grammar/generated/css_l4.rs`:
- `css_l4.rs:15951` — `pub(crate) structural_index: ::core::cell::OnceCell<…StructuralIndex>`
- `css_l4.rs:15982` — `::simd_scan::scan_structural(input, &alphabet)`
— byte-identical shape to `json.rs:732`.

The full scan-wired census at master `445925167` is **eight** generated
grammars, not four: `bbnf.rs:4843`, `bnf.rs`, `css_l4.rs:15982`,
`css_pretty.rs`, `csv.rs`, `ebnf.rs`, `google_sheets.rs:3559`, `json.rs:732`
each carry exactly one `::simd_scan::scan_structural(input, …)` call.

This is a textbook CH6 paper-close failure inverted: a **divergence
asserted on a false-negative grep**, with the closing of the gap
**deferred to a later wave** ("core fold later") when the ground truth is
that the wiring **already exists in core today**. The artefact's own
prior-totality anchor already knew this — COH-014 at
`restart/audit/totality/p1/1F-coherence-scan.md:87` states "generated JSON
**and Google Sheets** carry/consume the sidecar," so sheets-scan was on
the record; css_l4 was simply never grepped.

**Concrete fix.** Delete the "css_l4 grep absent there / json,ebnf,bnf,csv
only" claim. Replace the Gaps row with: the scan-wired set in crates/core
is **all eight** generated grammars (`bbnf, bnf, css_l4, css_pretty, csv,
ebnf, google_sheets, json`), each holding one
`OnceCell<StructuralIndex>` + `scan_structural` call
(`crates/core/src/grammar/generated/css_l4.rs:15951,15982`;
`json.rs:732`; `google_sheets.rs:3559`; `bbnf.rs:4843`). The CSS-scan gap
in crates/core is therefore **CLOSED, not pending** — the genuine residual
gap is narrower and must be re-stated precisely: the CSS scan index in
core **feeds the eager `OpenFrame`/`pending_*` builder, not a tape**
(which the artefact's own AP17-002 already says correctly). The W3 receiver
note must be corrected: SK-V17 W3 wires CSS scan **in skinny**; crates/core
CSS scan is already wired — what core lacks is the **tape consumer**, not
the scan. Also correct the Cross-Tree map `:84` to "WIRED into all 8
generated grammars including css_l4/css_pretty."

---

### §2 — 1f-coherence-scan: COH17-001..006, 008 spec↔impl rows → **ACCEPT**

Every row resolves at the cited path:line (verified above). The verdicts
are honest: COH17-001/002/004/006 are labelled `unimplemented` with the
divergence stated at present file:line (AoS vs SoA at
`record.rs:103` vs `skinny tape/mod.rs:94`; per-grammar value vs
`ValueRef<G>` at `css_l4/value.rs:414` + grep-empty `json/value.rs`;
x86-pinned CollapsedStage at `ARCH:1206`; live `StructLayout` at
`struct.rs:202`). No row claims a divergence "resolved" or "wired"
without the counter-citation. COH17-005/008 are labelled
`impl-exceeds-spec` and carry both the core multi-arch evidence and the
narrower skinny `[u8;64]` evidence. The "T-P2 must reconcile" language
lives only in the reconciliation/note columns — it disposes the divergence
forward without substituting for present ground truth, which is the
correct excavation posture per PASS-1 §0 (excavation proposes; synthesis
disposes). No CH6 violation.

---

### §3 — 1f-coherence-scan: COH17-003 "tape UNWIRED / scan WIRED split" → **ACCEPT**

The `unwired` claim is the strongest possible anti-paper-close evidence:
`TapeStructBuilder` grep-zero outside `crates/core/src/runtime/tape/`
(CH6 re-confirmed: 0 hits), and the live path provably uses
`JsonStructBuilder::new()` at `json/parse_with.rs:34`. The verdict
"spec-claims-implemented (UNWIRED confirmed as stated)" is honestly
self-aware: it does not claim the tape is wired; it confirms the SPEC
§0.1.11 prediction that it is dead-code-pending-SK-V18. This is the
opposite of paper-close. ACCEPT.

---

### §4 — 1f-coherence-scan: COH17-007 FactStream `unknown` + U-COH17-001 → **ACCEPT**

The only `unknown` verdict carries an explicit verify_action
(U-COH17-001) naming the comparison surfaces (`LOCKS.md:100-116` vs SPEC
`:243-244,:796-797`) and the receiver (T-P3/Omega) — both citations
re-resolved verbatim. Per CH6 every UNKNOWN must carry a verify_action;
this one does, and it does not defer ground truth (both sides are cited
at present file:line). U-COH17-002 and U-COH17-003 likewise carry
verify_actions with cited surfaces. ACCEPT.

---

### §5 — 1f-anti-pattern: AP17-002 sidecar producer scope → **REVISE**

**Location.** `1f-anti-pattern.md:56`: "Generated parsers retain
`OnceCell<StructuralIndex>` initialized via `scan_structural`
(`…json.rs:686,702,732`; **same shape in ebnf.rs:1335, bnf.rs:802,
csv.rs:520**)."

The cited lines resolve (`json.rs:686,702,732` verified;
`ebnf.rs:1335`, `bnf.rs:802`, `csv.rs:520` each carry the OnceCell doc
comment). The qualifier "same shape in" makes this a representative
sample rather than a closed set, so it is not a hard CH2/CH6 false claim
— **but** it is the same four-grammar undercount that produced the §1
REJECT in the sibling artefact, and an anti-pattern (Lock-1 firewall)
inventory must not undercount the live retained-projection surface. The
sidecar retained-index anti-pattern is present in **eight** grammars, and
the CH5 firewall verdict that turns on "how many retained structural
projections exist in crates/core" must enumerate all of them.

**Concrete fix.** Replace "same shape in ebnf.rs:1335, bnf.rs:802,
csv.rs:520" with the full enumerated set: the `OnceCell<StructuralIndex>`
+ `scan_structural` retained-projection sidecar appears in all eight
generated grammars — `bbnf.rs:4843`, `bnf.rs:802`, `css_l4.rs:15951`,
`css_pretty.rs`, `csv.rs:520`, `ebnf.rs:1335`, `google_sheets.rs:3559`,
`json.rs:686-732`. This makes the Lock-1 firewall scope honest:
**every** generated grammar, not a JSON-family sample, must collapse its
index into the tape's `offsets` or `local_temp_only` at SK-V18, else
REDRESS-53 re-entry. The verify_action in U-AP17-001 must scope to all
eight, not just json.rs.

---

### §6 — 1f-anti-pattern: AP17-001/003/004 + CH5 verdict → **ACCEPT**

AP17-001 (parallel-substrate firewall): the "ONE retained tape construct,
UNWIRED" claim is backed by the grep-zero CH6 re-confirmed; the verify_action
("grep `TapeStructBuilder|TapeRec|PayloadArena|TapeCursor` … confirm
exactly one tape survives") is concrete and present-tense. AP17-003 (god
module): CSS builder = 817 LOC re-confirmed exact; the nine `pending_*`
slab claim resolves to `css_l4/builder.rs:74-79`. AP17-004
(renamed-scanner): "no cross-call classifier state proven in this pass" is
honestly hedged with a verify_action to grep `prev_state|carry|prefix_xor`
before SK-V18 — it does NOT claim the absence as resolved, it carries the
verify_action forward (correct CH6 posture: a not-yet-proven absence is
stated as such, not paper-closed). The CH5 firewall verdict
(`:61-72`) does not over-claim "no second substrate exists" — it scopes
to "within crates/core in this scan" and names the exact fold conditions
that would trip Lock-1. ACCEPT.

---

### §7 — 1f-past-corpora: do-not-redrive ledger + direction-monotonicity → **REVISE**

The ledger rows resolve: REDRESS-53 pre-block (SPEC `:578,:825,:837-840`),
second-substrate §9 block (SPEC `:807-811,:483-485`), AZ-IV eager value
tree (SPEC `:791-793`), x86 not-target (alphaC `:307-316`), StructLayout
totality-only (alphaC `:20-29`) — all verbatim. The direction-monotonicity
note is correct and load-bearing (skinny→totality fold, never reverse).
**However**, the artefact inherits §1's false premise by reference: PC17-003
(`:55`) states the crates/core CSS builder's eager `CssTypedValue` "IS the
eager-value-tree shape" and must be replaced by "lazy `ValueRef<G>`
projection (COH17-002)" — this is sound — but the Prior-Totality
Continuity block (`:71-79`) re-anchors COH-014's `OnceCell` coupling
without noting that COH-014 itself enumerated **JSON + Google Sheets** as
carriers, which already contradicts the sibling coherence-scan's
"json/ebnf/bnf/csv only." A past-corpora scan whose explicit charge is "do
not re-derive prior findings" must surface that the prior finding's
grammar set (JSON+Sheets+scan-wired) is **wider** than the current
coherence inventory recorded — i.e., it should have caught the §1 REJECT.

**Concrete fix.** Add one row to the Prior-Totality Continuity block: prior
COH-014 (`restart/audit/totality/p1/1F-coherence-scan.md:87`) already
recorded JSON **and Google Sheets** as `OnceCell<StructuralIndex>`
carriers; the current SK-V17 scan-wired census is **all eight** generated
grammars including css_l4 (`css_l4.rs:15951,15982`). Flag the sibling
coherence-scan Gaps row 4 as contradicting this prior finding (cross-ref
the §1 REJECT), so the V3 fold corrects the coherence-scan rather than
carrying the undercount forward.

---

## Cross-cutting CH6 observations

1. **No tape claim is paper-closed as "wired."** Every artefact correctly
   states the crates/core tape is UNWIRED dead code (grep-zero confirmed).
   This is the central CH6-positive finding: the subject does NOT claim a
   fold that has not happened. The SK-V18 fold is consistently named as
   FUTURE, with the proven skinny shape as the source. No REVISE there.

2. **The one hard defect is a false-negative grep, not an over-claim.**
   The CSS-scan-not-wired claim (§1 REJECT, propagating to §5/§7 REVISE)
   is the inverse of typical paper-close: it manufactures a gap that does
   not exist and defers its closure to a later wave. Under CH6 this is
   still a ground-truth failure — "no divergence on false evidence" — and
   it materially mis-scopes the Lock-1 retained-projection firewall.

3. **Absent inventories (1a–1e).** Only the three 1F artefacts exist under
   `restart/audit/totality/sk-v17/p1/`. CH6 cannot disposition 1A–1E
   anti-paper-close because they are not written. This is flagged to the
   orchestrator: the convergence criterion (§4) cannot be met on a partial
   inventory set. NOT a CH6 disposition against 1F — a coverage gap the
   aggregator must record.

## Disposition summary

| § | Subject | Disposition |
|---|---|---|
| 1 | coherence: CSS scan not wired | **REJECT** — false-negative grep; gap does not exist |
| 2 | coherence: COH17-001..006,008 rows | ACCEPT |
| 3 | coherence: COH17-003 unwired/wired split | ACCEPT |
| 4 | coherence: COH17-007 + UNKNOWNs | ACCEPT |
| 5 | anti-pattern: AP17-002 sidecar scope | **REVISE** — undercount to 4, enumerate 8 |
| 6 | anti-pattern: AP17-001/003/004 + CH5 verdict | ACCEPT |
| 7 | past-corpora: ledger + continuity | **REVISE** — must catch §1 contradiction |

**Counts:** 7 dispositioned · 4 ACCEPT · 2 REVISE · 1 REJECT · ACCEPT-rate 57.1%.

**Verdict:** the SK-V17 1F triplet is well-grounded on its substrate /
value-API / BackendShape / NEON spine, but carries one load-bearing
false-negative (CSS structural scan IS wired in crates/core across all 8
generated grammars) that fabricates a gap and mis-scopes the Lock-1
retained-projection firewall from 4 grammars to its true 8. The REJECT +
two REVISEs must fold into V3 before this lens returns ≥95% ACCEPT.
