# SK-V17 S-P2 CHALLENGE — CH4 COST (V3)

Lens: CH4 COST. Cycle: V3. Date: 2026-05-29.
Pass: S-P2 Research. Contract: `restart/prompts/skinny/PASS-2-RESEARCH.md` §3 CH4 + §8.2;
`ORCHESTRATOR.md` §3W/§3Z; §8 non-negotiables (no SIMD/ASM primitive ships without a
scalar reference + checkasm parity *before* wiring; no kernel ships without a same-wave
consumer).
Subject: `restart/skinny/tranches/sk-v17/research/p2/{p2a..p2f}.md` (V3 revisions).
Master HEAD `0ae1caa52`; bbnf-simd ground truth re-verified this cycle.

## §0 — The CH4 test, stated precisely (carried verbatim from V1/V2)

CH4 disposes each §2 candidate primitive against THREE fields, all required:

1. **Scalar-reference status (SR)** — does the candidate name whether its scalar oracle
   EXISTS (file:line) or is ABSENT-with-sketch, or is justified N/A (substrate/codegen op,
   not a vector kernel)? An N/A is valid only when the candidate is provably not a SIMD/ASM
   kernel; an N/A on a vector kernel FAILS.
2. **Checkasm-parity expectation (CK)** — does the candidate name the differential gate
   (`tests/checkasm_<prim>.rs`, PRESENT/REQUIRED-NEW), or, for a non-kernel, name the
   correctness-parity analogue (corpus/8-field equality) that stands in for checkasm?
3. **Same-wave-consumer note (SWC)** — does the candidate name the JSON/CSS row that wires
   (moves or rejects) it in the SAME wave it lands? A kernel with no same-wave consumer is
   dead asm — REJECT.

A candidate missing ANY of the three fails CH4 (REVISE if recoverable; REJECT if
intrinsically a no-consumer / no-oracle kernel). CH4 also verifies COST HONESTY of any
"already wired / parity-passing / cheap" claim against what the named instruction route can
deliver on the target alphabet.

**The V3 gate is the V2 fold-landing check.** V2 found exactly ONE residual defect: P2-F
alone had not adopted the labeled three-field bullet shape (`Checkasm[-parity/-analogue]
(CH4)` + `Same-wave-consumer (CH4)`) that its five sibling artefacts already carried, leaving
CF-1 through CF-4b with five bullets each (Shape / Scalar-ref / Arch / P1 antecedent /
Grammar-neutral verdict) and the CK + SWC information only prose-implied. V2 ruled
prose-implied SWC insufficient (the ambiguity V1 §3 C-3 closed on G4). The V2→V3 fold
instruction was the exact mechanical bullet-adoption, with the field text named verbatim in
V2 CH4.md §3 D. V3 verifies the fold landed — per CF candidate, as a LABELED field — and
re-verifies the five siblings still hold the shape and the cost-honesty anchors.

## §1 — Verification performed this cycle (orchestrator-citable)

- **P2-F three-field fold LANDED (the V3 load-bearing check).** The five V2-REVISE CF
  candidates now carry the labeled `Checkasm[-analogue/-parity] (CH4)` AND
  `Same-wave-consumer (CH4)` bullets, matching the verbatim field text V2 CH4.md §3 D named:
  - **CF-1** (`p2f:138-174`): `Checkasm-analogue (CH4)` at `:149-154` (corpus-parity +
    cssparser 8-field equality, `PayloadArena.write_count==0`); `Same-wave-consumer (CH4)`
    at `:155-158` (the lazy `ValueRef` projection, Lock-1 same-substrate, land-together).
  - **CF-2** (`p2f:176-218`): `Checkasm-parity (CH4)` at `:193-196` (REQUIRED-NEW
    `checkasm_byte_class_from_eq_set_64`, the NEON eq-set fan differential against the scalar
    twin); `Same-wave-consumer (CH4)` at `:197-199` (CF-1's tape build consumes the `Vec<u32>`
    index; neither ships without the other).
  - **CF-3** (`p2f:220-261`): `Checkasm-analogue (CH4)` at `:227-230` (N/A as SIMD; parity =
    recognizer output equality with/without the Alt-mode pass); `Same-wave-consumer (CH4)` at
    `:231-234` (the post-CF-1 CSS recognizer spine, GATED on the typed-tape re-profile, named
    as the admission gate not a live consumer).
  - **CF-4a** (`p2f:263-297`): `Checkasm-parity (CH4)` at `:270-272` (REQUIRED-NEW
    `checkasm_digit_mac`, verified ABSENT); `Same-wave-consumer (CH4)` at `:273-277` (NONE,
    orphan-gated, PRESENT-AND-NAMED-NONE form).
  - **CF-4b** (`p2f:299-326`): `Checkasm-parity (CH4)` at `:306-308` (REQUIRED-NET-NEW
    `checkasm_i8mm_*`, neither kernel nor gate exists); `Same-wave-consumer (CH4)` at
    `:309-312` (NONE, net-new, HARD-GATED, "S-P3 must NOT shortlist as active").
  `grep -n "Same-wave-consumer (CH4)\|Checkasm-analogue (CH4)\|Checkasm-parity (CH4)"
  p2f-grammar-neutral.md` returns all ten labeled fields. The V2 defect is FOLDED. No
  candidate's substance changed — exactly the mechanical adoption V2 §3 D prescribed.

- **`byte_class_from_table_64_neon` scalar passthrough re-verified (cost-honesty anchor).**
  `bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4` tail-calls
  `byte_class_from_table_64_scalar` — confirms the "scalar passthrough today / does not yet
  earn its keep" cost-honest claims in CF-2 (`p2f:189-196`), C2 (`p2c`), CP-A1 (`p2a:248-251`).
  Every artefact correctly routes CSS through the eq-set fan, NOT this passthrough, and CF-2
  states the gap verbatim (`p2f:189-192`). A candidate routing CSS through the lo6/table
  path would claim a SIMD win it runs scalar.

- **lo6 mask `& 0x3f` re-verified (the `;`/`{` collision cost anchor).**
  `bbnf-simd/src/dispatch.rs:106` is `let slot = (byte & 0x3f) as usize` — a bitmask, NOT a
  modulo. `;`(0x3b)&0x3f=0x3b=59 ≡ `{`(0x7b)&0x3f=0x3b=59 COLLIDE; under true modulo
  (0x7b%0x3f=0x3c=60) they would NOT. Confirms the eq-set-route cost claims in CF-2
  (`p2f:60-87`), C1/C2 (`p2c:155-163,169-197`), CP-A1 (`p2a:240-253`), and the
  lo6-reuse-on-CSS overfit flags. The CSS scan correctly routes through the collision-free
  eq-set primitive, not the JSON lo6 TBL.

- **`checkasm_digit_*` absent re-verified (the udot/i8mm orphan gate).**
  `ls bbnf-simd/tests/ | grep -i digit` = empty. Confirms the REQUIRED-NEW
  `checkasm_digit_mac` gate stated in CF-4a (`p2f:270-272`), C5 (`p2c:243-244`), G4
  (`p2e:207-209`); and the REQUIRED-NET-NEW `checkasm_i8mm_*` in CF-4b (`p2f:306-308`), C6
  (`p2c:268-269`). The present checkasm differentials re-verified:
  `checkasm_byte_class_from_eq_set_64.rs`, `checkasm_byte_class_from_table_64.rs`,
  `checkasm_ascii_set_member_find_64.rs`, `checkasm_bitmap_next_set_bit.rs`,
  `checkasm_structural_terminator_64.rs` all present in `bbnf-simd/tests/` — the
  PRESENT-claims in C2/C4/G3 and CP-A1 are accurate.

- **Five-sibling three-field shape re-verified.** `grep -n "Same-wave\|[Cc]heckasm\|Scalar-ref"`
  per artefact confirms the labeled per-candidate fields persist from V2:
  - **P2-A** CP-A1/A2/A3/A4: SR (`:254,294,325,359`), CK (`:259,297,328,361`), SWC
    (`:268,303,334,365`) — all PRESENT-AND-NAMED.
  - **P2-B** C-B1/C-B2/C-B3: Scalar-ref status (`:122,171,192`), Checkasm status
    (`:124,177,196`), Same-wave consumer (`:131,181,197`) — the canonical three-field shape
    the fold instruction pointed at.
  - **P2-C** C1–C6: Scalar-ref (`:143,185,211,226,241,264`), checkasm parity
    (`:147,188,213,228,243,268`), Same-wave consumer present per candidate (C1 `:150`
    inline, C2 via summary table + §3, C3/C4 fold-into-C1/C2 named in arch/P1, C5/C6
    orphan-NONE in P1 antecedent `:246-255,272-277` + summary table `:283-288`).
  - **P2-D** D1–D5: Scalar-ref status (`:215,252,289,339,384`), Checkasm-analogue (CH4)
    (`:218,254,290,343,385`), Same-wave-consumer (CH4) (`:226,262,297,351,392`).
  - **P2-E** G1–G4: Scalar-ref status (`:117,155,193,209`), checkasm gate (G1/G2 REQ-NEW
    `:104-138/:166-177` summary table `:233-238`, G3 composition `:193`, G4 REQUIRED-NEW
    `:204-208`), same-wave consumer (G1/G2=G3 `:201-202`, G4=NONE orphan `:213-218`).

## §2 — Per-candidate CH4 disposition matrix

Legend: SR = scalar-reference status; CK = checkasm-parity expectation; SWC =
same-wave-consumer note. ✔ present+correct (LABELED field or load-bearing per-candidate
statement); ✘ missing.

| Artefact / candidate | SR | CK | SWC | Disposition |
|---|---|---|---|---|
| **P2-A** CP-A1 byte-class classifier (eq-set/lo6) | ✔ | ✔ | ✔ | **ACCEPT** (carried from V2) |
| P2-A CP-A2 push_plain_offset | ✔ N/A | ✔ | ✔ | **ACCEPT** |
| P2-A CP-A3 lazy ValueRef rider | ✔ N/A | ✔ | ✔ | **ACCEPT** |
| P2-A CP-A4 tokenize-once | ✔ N/A | ✔ | ✔ | **ACCEPT** |
| P2-A CP-NONE / CP-BLOCKED ×3 | n/a | n/a | n/a | **ACCEPT** (non-candidates, correctly retired) |
| **P2-B** C-B1 byte_class_from_eq_set_64 | ✔ | ✔ | ✔ | **ACCEPT** |
| P2-B C-B2 push_plain_offset | ✔ N/A | ✔ | ✔ | **ACCEPT** |
| P2-B C-B3 udot (orphan) | ✔ | ✔ ABSENT | ✔ NONE | **ACCEPT** (correct PROCESS-REJECT) |
| P2-B C-B0 admission process | n/a | n/a | n/a | **ACCEPT** (the gate) |
| **P2-C** C1 lo6 TBL | ✔ | ✔ | ✔ | **ACCEPT** (correctly ruled inadmissible for CSS) |
| P2-C C2 eq-set fan | ✔ | ✔ | ✔ | **ACCEPT** |
| P2-C C3 shrn movemask | ✔ N/A | ✔ transitive | ✔ fold | **ACCEPT** |
| P2-C C4 host CTZ | ✔ | ✔ | ✔ fold | **ACCEPT** |
| P2-C C5 udot (orphan) | ✔ | ✔ REQ-NEW | ✔ NONE | **ACCEPT** (orphan-flagged) |
| P2-C C6 i8mm (net-new) | ✔ would-be | ✔ would-be | ✔ NONE | **ACCEPT** (doubly-gated) |
| **P2-D** D1 push_plain_offset | ✔ N/A | ✔ | ✔ | **ACCEPT** |
| P2-D D2 lazy ValueRef view | ✔ N/A | ✔ | ✔ | **ACCEPT** |
| P2-D D3 O(1) checkpoint/truncate | ✔ N/A | ✔ | ✔ | **ACCEPT** (S-P1-re-confirm carried) |
| P2-D D4 one-shot SIMD reserve | ✔ | ✔ | ✔ | **ACCEPT** |
| P2-D D5 sparse-flag side-table | ✔ N/A | ✔ | ✔ | **ACCEPT** |
| P2-D D6 second substrate | n/a | n/a | n/a | **ACCEPT** (REJECT-on-sight, correct) |
| **P2-E** G1 comment_body_mask_64 | ✔ ABSENT+sketch | ✔ REQ-NEW | ✔ G3 | **ACCEPT** |
| P2-E G2 bracket_depth_mask_64 | ✔ ABSENT+sketch | ✔ REQ-NEW | ✔ G3 | **ACCEPT** (REDRESS-89 bound inline) |
| P2-E G3 scan_components_to_index | ✔ | ✔ | ✔ | **ACCEPT** |
| P2-E G4 udot checkasm gate | ✔ | ✔ | ✔ NONE | **ACCEPT** (orphan SWC stated) |
| P2-E G5 FNV (non-candidate) | n/a | n/a | n/a | **ACCEPT** |
| **P2-F** CF-1 tape-append + ValueRef | ✔ N/A | ✔ (`:149`) | ✔ (`:155`) | **ACCEPT** (V2 REVISE folded) |
| **P2-F** CF-2 membership classifier | ✔ + GAP | ✔ (`:193`) | ✔ (`:197`) | **ACCEPT** (V2 REVISE folded) |
| **P2-F** CF-3 commit-by-construction Alt | ✔ N/A | ✔ (`:227`) | ✔ GATED (`:231`) | **ACCEPT** (V2 REVISE folded) |
| P2-F CF-4a udot 4-digit | ✔ | ✔ REQ-NEW (`:270`) | ✔ NONE (`:273`) | **ACCEPT** (V2 REVISE folded; orphan SWC) |
| P2-F CF-4b i8mm net-new | ✔ REQ-NET-NEW | ✔ REQ-NET-NEW (`:306`) | ✔ NONE (`:309`) | **ACCEPT** (V2 REVISE folded; orphan SWC) |
| P2-F CF-0 negative space | n/a | n/a | n/a | **ACCEPT** |

**Counts (candidate rows only; non-candidate / process / negative-space rows excluded
from the rate, ACCEPTed as correctly-dispositioned). V1 census of 24 candidate rows held
through V2; held again V3:**

- Candidate rows: **24** (V1/V2 census held).
- **ACCEPT: 24** (every candidate carries SR + CK + SWC as a labeled field or load-bearing
  per-candidate statement; the five V2-REVISE CF rows folded cleanly this cycle).
- **REVISE: 0.**
- **REJECT: 0.**

**ACCEPT rate over candidate rows: 24/24 = 100%.** Up from V2's 79.2% (and V1's 45.8%). The
single V2 defect (P2-F's un-folded labeled-field shape) is resolved by the exact mechanical
bullet-adoption V2 §3 D prescribed — no substance change. **Clears the §3Z ≥95% bar.** CH4
is no longer the gating lens.

## §3 — Concrete fixes (path:line)

**No CH4 REVISE or REJECT this cycle.** Every candidate's three fields are present and
cost-honest. The per-artefact audit:

### A — P2-F `p2f-grammar-neutral.md` — V2 REVISE ×5 RESOLVED (ACCEPT, recorded for audit)

The V2 §3 D-1/D-2/D-3/D-4/D-5 fixes named the verbatim labeled-field text for CF-1, CF-2,
CF-3, CF-4a, CF-4b. All five now carry the labeled `Checkasm[-analogue/-parity] (CH4)` +
`Same-wave-consumer (CH4)` bullets at the lines cited in §1, matching the prescribed text:

- **CF-1 (D-1 RESOLVED).** SR N/A (substrate migration, `:145-148`); CK at `:149-154`
  (corpus-parity + cssparser 8-field, `PayloadArena.write_count==0`); SWC at `:155-158`
  (lazy `ValueRef` projection, Lock-1 same-substrate). The CF-1↔projection pairing mirrors
  CP-A2↔CP-A3 / D1↔D2.
- **CF-2 (D-2 RESOLVED).** SR PRESENT-with-GAP (`:185-192`, the strongest cost-honest entry:
  names the live NEON scalar-passthrough gap, re-verified `:189-192`); CK at `:193-196`
  (REQUIRED-NEW `checkasm_byte_class_from_eq_set_64`); SWC at `:197-199` (CF-1 tape build).
- **CF-3 (D-3 RESOLVED).** SR N/A (codegen, `:226`); CK at `:227-230` (N/A SIMD; recognizer
  output equality with/without Alt-mode); SWC at `:231-234` (post-CF-1 spine, GATED on the
  typed-tape re-profile, correctly named as gate not live consumer). The P1-antecedent bullet
  (`:236-255`) remains exemplary on refusing the 28.87%+2.45% recognition-control figure as a
  measured rollback antecedent — CH1's obligation, untouched by CH4.
- **CF-4a (D-4 RESOLVED).** SR PRESENT (`:267-269`); CK at `:270-272` (REQUIRED-NEW
  `checkasm_digit_mac`, verified ABSENT); SWC at `:273-277` (NONE, orphan-gated,
  PRESENT-AND-NAMED-NONE — the canonical orphan disposition, matching C-B3/C5/G4).
- **CF-4b (D-5 RESOLVED).** SR REQUIRED-NET-NEW (`:304-305`); CK at `:306-308`
  (REQUIRED-NET-NEW `checkasm_i8mm_*`); SWC at `:309-312` (NONE, HARD-GATED, "S-P3 must NOT
  shortlist as active"). Matches C6 and the §3 table row `:350`.

P2-F ACCEPTs. No further fix.

### B — P2-A / P2-B / P2-C / P2-D / P2-E — three-field shape held (ACCEPT, recorded)

All five carry the labeled or load-bearing per-candidate three-field shape verified in §1,
and the cost-honesty anchors (lo6 `& 0x3f` collision, NEON table passthrough, absent digit
checkasm) re-verified byte-accurate this cycle. No CH4 fix. (One out-of-lens hygiene note
for the aggregator, NOT a CH4 disposition: **P2-A's frontmatter still reads `Cycle: V2`**
[`p2a:3` vs the other five at `Cycle: V3`]. P2-A's candidate substance is correct and needs
no CH4 change — but a stale cycle marker is a CH1/CH6 provenance matter the aggregator may
want a one-line bump on. CH4 does not gate on it.)

## §4 — Cross-artefact observation (the V3 systemic truth)

V1 found ONE systemic shape gap (the §2.1 frontmatter schema pre-dated the three-field
requirement; four artefacts omitted labeled CK + SWC) and issued "adopt the P2-B three-field
bullet shape." V2 confirmed five of six artefacts folded; P2-F alone had not. V3 confirms
**P2-F executed the identical mechanical adoption V2 §3 D named verbatim** — all five CF
candidates now carry the labeled `Checkasm[-parity/-analogue] (CH4)` + `Same-wave-consumer
(CH4)` fields, with no substance change. The homogeneous shape gap is fully closed across all
six artefacts.

The cost-honesty substance was already sound from V2: the eq-set route is load-bearing (lo6
is correctly demoted as inadmissible for the CSS alphabet), the NEON 256-table is honestly
flagged as a scalar passthrough that does not yet earn its keep, and the udot/i8mm digit
kernels are correctly carried as orphan-gated contingencies (SWC=NONE) with their checkasm
gates flagged REQUIRED-NEW / REQUIRED-NET-NEW rather than claimed-present. No candidate makes
a cost claim the named instruction route cannot deliver on the target alphabet.

The lift trajectory is 45.8% (V1) → 79.2% (V2) → **100% (V3)** on a clean fold of the single
homogeneous shape defect. CH4 clears the §3Z ≥95% bar this cycle with margin and is no longer
the gating lens. No orphan REVISE (zero REVISE issued); no REJECT (no candidate is
intrinsically dead asm — CF-4a/CF-4b/C5/C6/C-B3/G4 are correctly carried as orphan-gated
contingencies, which is the right CH4 disposition, not a failure).

## §5 — CH4 verdict

- Candidate rows: **24** (V1/V2 census held). **ACCEPT 24 / REVISE 0 / REJECT 0.**
- ACCEPT rate **100%** — up from V2's 79.2%; **clears the §3Z ≥95% bar this cycle.** CH4 is
  no longer the gating lens.
- The five V2 REVISEs (all in P2-F: CF-1, CF-2, CF-3, CF-4a, CF-4b) are RESOLVED by the exact
  mechanical labeled-three-field-bullet adoption V2 §3 D prescribed — verified landed
  per-candidate at the cited lines, no substance change.
- Zero REJECT: every candidate carries a real oracle path (or a justified N/A) and a
  derivable/named consumer; CF-4a/CF-4b/C5/C6/C-B3/G4 are correctly orphan-gated (SWC=NONE),
  not dead kernels.
- Cost-honesty anchors re-verified byte-accurate: lo6 `& 0x3f` `;`/`{` slot-59 collision
  (`dispatch.rs:106`), `byte_class_from_table_64_neon` scalar passthrough
  (`aarch64/byte_class_from_table_64.rs:1-4`), `checkasm_digit_*` absent (`ls tests/ | grep
  digit` empty). No candidate claims a SIMD win it would run scalar.
- One out-of-lens hygiene note (NOT a CH4 disposition): P2-A frontmatter `Cycle: V2` is stale
  vs the five-sibling `Cycle: V3` (`p2a:3`); flagged for the aggregator's CH1/CH6 provenance
  pass.
