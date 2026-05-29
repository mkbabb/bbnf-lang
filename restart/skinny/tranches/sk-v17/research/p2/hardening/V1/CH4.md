# SK-V17 S-P2 CHALLENGE — CH4 COST (V1)

Lens: CH4 COST. Cycle: V1. Date: 2026-05-29.
Pass: S-P2 Research. Contract: `restart/prompts/skinny/PASS-2-RESEARCH.md` §3 CH4 + §8.2;
`ORCHESTRATOR.md` §3W/§3Z; §8 non-negotiables (no SIMD/ASM primitive ships without a
scalar reference + checkasm parity *before* wiring; no kernel ships without a same-wave
consumer).
Subject: `restart/skinny/tranches/sk-v17/research/p2/{p2a..p2f}.md`.
Master HEAD `0ae1caa52`; bbnf-simd verified this cycle.

## §0 — The CH4 test, stated precisely

CH4 disposes each §2 candidate primitive against THREE fields, all required:

1. **Scalar-reference status** — does the candidate name whether its scalar oracle
   EXISTS (file:line) or is ABSENT-with-sketch, or is justified N/A (substrate/codegen
   op, not a vector kernel)? An N/A claim is only valid when the candidate is provably
   not a SIMD/ASM kernel; an N/A on a vector kernel FAILS.
2. **Checkasm-parity expectation** — does the candidate name the differential gate
   (`tests/checkasm_<prim>.rs`, PRESENT/REQUIRED-NEW), or, for a non-kernel, name the
   correctness-parity analogue (corpus/8-field equality) that stands in for checkasm?
3. **Same-wave-consumer note** — does the candidate name the named JSON/CSS row that
   wires (moves or rejects) it in the SAME wave it lands (Lock 16 v+1 / `LOCKS.md:511-516`,
   the escape_mask clause)? A kernel with no same-wave consumer is dead asm — REJECT.

A candidate missing ANY of the three fails CH4 (REVISE if the missing field is
recoverable from sibling artefacts; REJECT if the candidate is intrinsically a
no-consumer / no-oracle kernel).

CH4 also verifies COST HONESTY of any "already wired / parity-passing / cheap" claim:
a candidate that asserts a SIMD win its named instruction route cannot deliver on the
target alphabet is a cost defect even if all three fields are present.

## §1 — Verification performed this cycle (orchestrator-citable)

- `bbnf-simd/src/scalar/` holds 7 scalar twins (`byte_class_from_eq_set_64`,
  `byte_class_from_table_64`, `bitmap_next_set_bit`, `bitmap_prefix_xor_64`,
  `bulk_emit_positions_64`, `eob_pad_clamp`, `swar_8byte`). Confirms every PRESENT
  scalar-ref claim in P2-B/C/E/F. **No `comment_body_mask_64.rs` / `bracket_depth_mask_64.rs`
  scalar twin** — confirms P2-E G1/G2 ABSENT.
- `bbnf-simd/tests/` holds 11 `checkasm_*.rs` + parity files; **no `checkasm_digit_mac.rs`**
  — confirms the udot orphan's MISSING checkasm gate (P2-B C-B3, P2-C C5, P2-E G4, P2-F
  CF-4a all assert this correctly).
- lo6 mod-0x3f collision recomputed: JSON `{}[],:"` admissible; CSS `;{}` and `:{};`
  COLLIDE at slot 59 (`;`0x3b ≡ `{`0x7b). Confirms the P2-C C1 / P2-F §1.2 finding and
  exposes the P2-A CP-A1 cost defect (§3, A-1).

## §2 — Per-candidate CH4 disposition matrix

Legend: SR = scalar-reference status; CK = checkasm-parity expectation; SWC =
same-wave-consumer note. ✔ present+correct; ~ present-but-thin; ✘ missing.

| Artefact / candidate | SR | CK | SWC | Disposition |
|---|---|---|---|---|
| **P2-A** CP-A1 byte_class_index_64 | ✔ | ✔ | ✘ | **REVISE** (no SWC line + lo6 cost defect) |
| P2-A CP-A2 push_plain_offset | ✔ | ~ | ✘ | **REVISE** (no SWC line) |
| P2-A CP-A3 lazy ValueRef rider | ✔ | ~ | ✘ | **REVISE** (no SWC line) |
| P2-A CP-A4 tokenize-once | ✔ | ~ | ✘ | **REVISE** (no SWC line) |
| P2-A CP-NONE / CP-BLOCKED ×3 | n/a | n/a | n/a | **ACCEPT** (non-candidates, correctly retired) |
| **P2-B** C-B1 byte_class_from_eq_set_64 | ✔ | ✔ | ✔ | **ACCEPT** |
| P2-B C-B2 push_plain_offset | ✔ | ✔ | ✔ | **ACCEPT** |
| P2-B C-B3 udot (orphan) | ✔ | ✔ | ✔ | **ACCEPT** (correct REJECT, all 3 fields) |
| P2-B C-B0 admission process (G1-G6) | n/a | n/a | n/a | **ACCEPT** (the gate; SWC = G4) |
| **P2-C** C1 lo6 TBL | ✔ | ✔ | ✔ | **ACCEPT** (correctly ruled INADMISSIBLE) |
| P2-C C2 eq-set fan | ✔ | ✔ | ✔ | **ACCEPT** |
| P2-C C3 shrn movemask | ✔ N/A | ✔ transitive | ✔ fold | **ACCEPT** |
| P2-C C4 host CTZ | ✔ | ✔ | ✔ fold | **ACCEPT** |
| P2-C C5 udot (orphan) | ✔ | ✔ REQ-NEW | ✔ NONE | **ACCEPT** (orphan-flagged) |
| P2-C C6 i8mm (net-new) | ✔ would-be | ✔ would-be | ✔ NONE | **ACCEPT** (doubly-gated) |
| **P2-D** D1 push_plain_offset | ✔ N/A | ✘ | ✘ | **REVISE** |
| P2-D D2 lazy ValueRef view | ✔ N/A | ✘ | ✘ | **REVISE** |
| P2-D D3 O(1) checkpoint/truncate | ✔ N/A | ✘ | ✘ | **REVISE** |
| P2-D D4 one-shot SIMD reserve | ✔ | ~ | ✘ | **REVISE** |
| P2-D D5 sparse-flag side-table | ✔ N/A | ✘ | ✘ | **REVISE** |
| P2-D D6 second substrate | n/a | n/a | n/a | **ACCEPT** (REJECT-on-sight, correct) |
| **P2-E** G1 comment_body_mask_64 | ✔ ABSENT+sketch | ✘ | ✘ | **REVISE** |
| P2-E G2 bracket_depth_mask_64 | ✔ ABSENT+sketch | ✘ | ✘ | **REVISE** |
| P2-E G3 scan_components_to_index | ✔ | ~ | ✔ | **ACCEPT** |
| P2-E G4 udot checkasm gate | ✔ | ✔ | ✘ | **REVISE** (orphan, no SWC) |
| P2-E G5 FNV (non-candidate) | n/a | n/a | n/a | **ACCEPT** |
| **P2-F** CF-1 tape-append + ValueRef | ✔ N/A | ✘ | ✘ | **REVISE** |
| P2-F CF-2 membership classifier | ✔ + GAP | ✔ | ✘ | **REVISE** (no SWC line) |
| P2-F CF-3 commit-by-construction Alt | ✔ N/A | ✘ | ✘ | **REVISE** |
| P2-F CF-4a udot 4-digit | ✔ | ✔ | ✔ NONE | **ACCEPT** (orphan-flagged, all 3) |
| P2-F CF-4b i8mm net-new | ✔ REQ-NEW | ✔ REQ-NEW | ✔ NONE | **ACCEPT** (orphan-flagged, all 3) |
| P2-F CF-0 negative space | n/a | n/a | n/a | **ACCEPT** |

**Counts (candidate rows only, excluding the non-candidate / negative-space / process
rows which CH4 ACCEPTs as correctly-dispositioned):**

- Candidate rows: 24.
- ACCEPT: 11 (P2-B ×3, P2-C ×6, P2-E G3, P2-F CF-4a/CF-4b).
- REVISE: 13 (P2-A ×4, P2-D ×5, P2-E G1/G2/G4, P2-F CF-1/CF-2/CF-3).
- REJECT: 0.
- Non-candidate / process / negative-space rows ACCEPTed as correct: 6
  (CP-NONE+CP-BLOCKED bundle, C-B0, D6, G5, CF-0, plus orphan-as-non-active reading is
  already counted in the candidate ACCEPTs above).

**ACCEPT rate over candidate rows: 11/24 = 45.8%.** Below the §3Z ≥95% bar. CH4 is the
gating lens this cycle: P2-B and P2-C clear it outright; P2-D and P2-A clear it on no
candidate; P2-E and P2-F clear it partially. The single load-bearing fix is mechanical
(adopt P2-B's three-field shape) and re-frames cleanly into V2.

## §3 — Concrete fixes (path:line + the exact field to add)

### A — P2-A `p2a-sota-teardown.md`

**A-1 (REVISE, COST DEFECT). CP-A1, `p2a-sota-teardown.md:217-242`.** The shape (`:219-227`)
routes `byte_class_index_64` through `vqtbl4q_u8` 4-table lookup and asserts at `:232`
"**Already wired and parity-passing for JSON** (`json/scan.rs:219` calls `classify_tbl4`)."
This is cost-misleading for CSS: the lo6 `classify_tbl4` backend is INADMISSIBLE for every
CSS structural alphabet — `;`(0x3b) and `{`(0x7b) collide at lo6 slot 59 (verified this
cycle; P2-C C1 `:152-157`, P2-F §1.2 `:53-83`). On CSS the lo6 route silently falls back to
scalar and yields NO SIMD win. Fix: replace the `vqtbl4q_u8` shape line (`:219-221`) with
the admissible eq-set fan (`byte_class_from_eq_set_64_neon`, the P2-C C2 / P2-F CF-2 route),
and re-word `:232` to "JSON-witnessed via the lo6 backend; the CSS route is the collision-
free eq-set / 256-table primitive (the lo6 backend is JSON-alphabet-overfit, `dispatch.rs:101`
guards the fallback)." Without this, CP-A1 claims a cost it cannot pay on the target grammar.

**A-2 (REVISE). CP-A1..A4 all lack an explicit same-wave-consumer line, `:217-308`.** Each
candidate names SHAPE / SCALAR-REF / ARCH / P1-ANTECEDENT / SOTA-ANTECEDENT but no
`Same-wave consumer:` field. CH4 requires it. The consumers are derivable and must be
stated: CP-A1's SWC is the CP-A2 tape build that consumes the `Vec<u32>` index (the
recognizer scan replacement); CP-A2's SWC is CP-A3 (the lazy `ValueRef` projection rides the
tape it appends to — they land together or neither); CP-A3's SWC is the CSS typed-equality
re-proof against cssparser 8-field + the eager-tree parity baseline; CP-A4's SWC is the
declaration/selector parse that consumes the shared index. Add one `- **Same-wave consumer.**`
bullet per candidate naming the row, mirroring P2-B's `Same-wave consumer:` field
(`p2b-dav1d-process.md:131,160`).

**A-3 (REVISE, thin). CP-A2/A3/A4 checkasm-parity expectation, `:254-255,:276-278,:299-301`.**
The N/A-because-not-a-kernel justification is correct, but CH4 wants the parity ANALOGUE
named as the standing-in gate. CP-A2 names "cssparser 8-field structural-equality re-proof"
(`:254`) — good; promote it to an explicit `Checkasm analogue:` field. CP-A3 names
"preserve-rich-ast parity + cssparser 8-field" (`:276-278`) — promote likewise. CP-A4 names
"the same cssparser 8-field equality" (`:299`) — promote. This is a labelling fix, not new
content; it makes the three-field shape uniform with P2-B/C.

### B — P2-D `p2d-substrate-tape.md`

**B-1 (REVISE). D1-D5 lack both the checkasm-parity-expectation field AND the
same-wave-consumer field, `:186-313`.** P2-D's §2 preamble (`:180-184`) declares the
scalar-ref column "N/A — substrate op" which is correct for D1/D2/D3/D5 and "PRESENT (the
consumed kernel's)" for D4 — but it omits the other two CH4 fields entirely. These are all
recoverable (the candidates are good; the artefact under-specifies the CH4 surface):

- **D1 `push_plain_offset` (`:186-207`):** add `Checkasm analogue:` = the `tape ↔ fact_stream`
  corpus-parity differential (the existing `corpus_parity.rs` shape extended to CSS, exactly
  as P2-B C-B2 states at `:156-158`); add `Same-wave consumer:` = D2 (the lazy `ValueRef`
  projection — the append and the view land together, Lock 1 substrate union).
- **D2 lazy `ValueRef` view (`:209-235`):** `Checkasm analogue:` = preserve-rich-ast parity
  (dimensions/colors/functions counts match the eager-tree baseline) + cssparser 8-field;
  `Same-wave consumer:` = D1 (it reads the tape D1 appends; the `PayloadArena.write_count==0`
  invariant is the parity proof).
- **D3 O(1) checkpoint/truncate (`:237-260`):** `Checkasm analogue:` = the JSON
  checkpoint/rollback soundness already banked (`8153236e8`, 20× sound); `Same-wave consumer:`
  = the CSS recognizer's speculative Alts (D2's spine), in the SAME wave the tape lands.
- **D4 one-shot SIMD reserve (`:262-287`):** SR already names `scan_structurals_scalar`
  (`:272-273`) — good; add `Checkasm:` = the consumed kernel's existing
  `checkasm_byte_class_from_table_64` (the count reuses the scan kernel); add
  `Same-wave consumer:` = D1's `TapeBuilder` (the reserve sizes the offset vector D1 fills).
  The "gated behind D1/D2 + NEON scan" caveat (`:286-287`) already implies this; promote it
  to the explicit field.
- **D5 sparse-flag side-table (`:289-313`):** `Checkasm analogue:` = the flag-read binary
  search parity (the `flags_at` round-trip); `Same-wave consumer:` = D2's projection (the
  flag exists only to let D2 disambiguate a kind a source-byte re-read cannot recover). The
  Lock-14 GUARD (`:305-313`) is orthogonal to CH4 and stays.

D6 (`:315-324`) is a REJECT-on-sight non-candidate; no fields needed — ACCEPT.

### C — P2-E `p2e-parse-that-gaps.md`

**C-1 (REVISE). G1 `comment_body_mask_64` (`:102-128`).** SR is exemplary (ABSENT + a full
scalar-ref sketch `:112-122` — the executable spec). Missing: explicit checkasm-parity
expectation and same-wave-consumer FIELDS. Both are recoverable: add
`Checkasm-parity: REQUIRED-NEW — checkasm_comment_body_mask_64.rs, NEON==scalar byte-exact
over the digraph-straddle/carry edge cases, under the canary+signal+alignment harness`
(mirroring `checkasm_byte_class_from_eq_set_64.rs`); add `Same-wave consumer: G3
scan_components_to_index` (G3 `:166-184` already names G1/G2 as its inputs — make the back-
reference explicit on G1). Without the checkasm field, a net-new NEON kernel is proposed
without naming its parity gate, the exact §8.2 non-negotiable CH4 enforces.

**C-2 (REVISE). G2 `bracket_depth_mask_64` (`:130-164`).** Same as C-1: SR exemplary
(ABSENT + sketch `:140-150`); add `Checkasm-parity: REQUIRED-NEW —
checkasm_bracket_depth_mask_64.rs, NEON==scalar byte-exact, with adversarial
underflow/overflow depth-carry seeds`; add `Same-wave consumer: G3`. Note: G2's optional
CTZ "ranges" path (`:159-161`) touches REDRESS-89 — CH3's concern, not CH4 — but the
checkasm field is what makes the depth-carry differential auditable, so it is load-bearing
here too.

**C-3 (REVISE). G4 udot checkasm gate (`:185-199`).** This candidate IS a checkasm gate
(its whole shape is the missing `checkasm_digit_mac`), and SR is PRESENT (`:190`) — so CK is
inherently satisfied. The missing field is the same-wave consumer: G4 has NONE on the
current planes (correctly, it is orphan-gated `:193-199`). CH4 requires the SWC field be
PRESENT-AND-NAMED-NONE (as P2-B C-B3 `:176` and P2-C C5 `:236-243` and P2-F CF-4a do), not
merely implied by prose. Add `Same-wave consumer: NONE on either CSS plane — orphan-gated;
admits only after a post-W1/W2 typed-ValueRef re-profile names a digit/dimension leaf`. As
written G4 leaves the SWC field unstated, which is exactly the ambiguity CH4 closes.

G3 (`:166-184`) ACCEPTs: SR = `scan_structurals_scalar` mirror (`:174-176`); CK = the
byte-exact-vs-`find_component_delim` parity anchor (`:175-176`); SWC = explicitly named "the
consumer that makes G1/G2 same-wave-consumed" + "the tape the index feeds" (`:183`). All
three present. G5 (`:201-210`) is a non-candidate — ACCEPT.

### D — P2-F `p2f-grammar-neutral.md`

**D-1 (REVISE). CF-1 (`:134-160`), CF-3 (`:199-223`).** SR correctly N/A (substrate /
codegen). Missing CK + SWC fields. P2-F's charter is grammar-neutrality, and its §2 preamble
(`:128-132`) carries the CH2 verdict richly — but CH4's two extra fields are absent. Add:
- CF-1: `Checkasm analogue:` = the `tape↔fact_stream` corpus-parity + cssparser 8-field (same
  as D1/CP-A2); `Same-wave consumer:` = the CSS `value_from_ref` rider it feeds (CF-1's
  append and projection are the same wave).
- CF-3: `Checkasm analogue:` = N/A codegen, parity = the recognizer output equality with/
  without the commit-by-construction pass; `Same-wave consumer:` = the CSS recognizer spine
  post-CF-1. CF-3 already self-flags a hard S-P1-re-confirm obligation (`:213-217`) — the SWC
  field should name that re-confirm as the gate.

**D-2 (REVISE). CF-2 (`:162-198`).** SR is the strongest cost-honest entry in the artefact:
it names the PRESENT scalar refs AND the live GAP — "the NEON `byte_class_from_table_64_neon`
is a scalar passthrough today (`aarch64/byte_class_from_table_64.rs:1-4`)" (`:175-178`),
verified this cycle. CK present (`checkasm_byte_class_from_eq_set_64`, `:181`). Missing only
the explicit SWC field: add `Same-wave consumer: CF-1's tape build (the Vec<u32> index this
classifier emits is the tape's offsets)` — the same consumer P2-C C2 names. This is a one-line
addition; CF-2 is otherwise a model CH4 entry (it is the only candidate that names the SIMD
backend's "does not yet earn its keep" cost honestly, `:178`).

CF-4a (`:225-252`) and CF-4b (`:254-274`) ACCEPT: both carry SR (PRESENT / REQUIRED-NET-NEW),
CK (`checkasm_digit_mac` / `checkasm_i8mm_*`), and an explicit SWC=NONE with the orphan-gate
(`:234-241,:263-266`) — all three fields, correctly dispositioning the orphans as
non-active-shortlist contingencies. CF-0 (`:276-288`) is negative space — ACCEPT.

## §4 — Cross-artefact observation (the recoverable systemic gap)

The 13 REVISEs are ONE systemic shape gap, not 13 independent defects. P2-B (the dav1d
process artefact) and P2-C (the arch artefact) carry the canonical three-field CH4 shape
per candidate (`p2b-dav1d-process.md:122-143` C-B1 carries SR + CK + SWC as named bullets;
the G1-G6 gate table `:189-196` makes G4=same-wave-consumer a first-class admission gate).
P2-A, P2-D, P2-E (G1/G2/G4), and P2-F simply did not adopt that bullet shape — they carry
SR + grammar-neutral (the §2.1 frontmatter schema asks for "shape + scalar-ref status +
arch + P1 antecedent") but the §2.1 schema PRE-DATES the CH4 three-field requirement and
omits CK + SWC. The fix is mechanical: every candidate adopts P2-B's `Same-wave consumer:`
and `Checkasm[-analogue]:` fields. No candidate's substance changes; no candidate is
REJECTed for a missing oracle or missing consumer (CF-4a/4b correctly carry SWC=NONE +
orphan-gate, which is the right disposition, not a CH4 failure). The single substantive
COST defect is P2-A CP-A1's lo6 cost-misclaim (A-1), which P2-C/P2-F already caught and
which the V2 fold should propagate into P2-A.

The aggregator should fold all 13 REVISEs into the V2 dispatch with the instruction:
"adopt the P2-B per-candidate three-field shape (Scalar-ref / Checkasm[-analogue] /
Same-wave-consumer) for every §2 candidate; the consumers and parity analogues are named in
§3 of CH4.md and are not new research." No orphan REVISE: every REVISE here carries its
concrete fix + the file:line + the exact field text to add.

## §5 — CH4 verdict

- Candidate rows: 24. ACCEPT 11 / REVISE 13 / REJECT 0.
- ACCEPT rate 45.8% — CH4 does NOT clear the §3Z ≥95% bar this cycle; V2 required.
- Zero REJECT: every candidate has a real oracle path and a derivable consumer; nothing is
  intrinsically dead asm. The orphans (udot/i8mm) are correctly carried as SWC=NONE gated
  contingencies, not as live kernels — that is the right CH4 disposition, not a failure.
- One substantive COST defect (P2-A CP-A1 lo6 cost-misclaim, A-1); the other 12 REVISEs are
  the mechanical adoption of the P2-B three-field shape.
- No orphan REVISE: all 13 carry path:line + the exact field to add.
