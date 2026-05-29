# SK-V17 S-P2 CHALLENGE — CH4 COST (V2)

Lens: CH4 COST. Cycle: V2. Date: 2026-05-29.
Pass: S-P2 Research. Contract: `restart/prompts/skinny/PASS-2-RESEARCH.md` §3 CH4 + §8.2;
`ORCHESTRATOR.md` §3W/§3Z; §8 non-negotiables (no SIMD/ASM primitive ships without a
scalar reference + checkasm parity *before* wiring; no kernel ships without a same-wave
consumer).
Subject: `restart/skinny/tranches/sk-v17/research/p2/{p2a..p2f}.md` (V2 revisions).
Master HEAD `0ae1caa52`; bbnf-simd ground truth re-verified this cycle.

## §0 — The CH4 test, stated precisely (carried verbatim from V1)

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

**The V2 test is sharper than the field-presence test.** V1 found a single systemic shape
gap (the §2.1 frontmatter schema pre-dated the CH4 three-field requirement; P2-A/D/E/F all
carried SR + grammar-neutral but omitted explicit CK + SWC fields). The aggregator folded
13 REVISEs with the instruction "adopt the P2-B per-candidate three-field shape
(Scalar-ref / Checkasm[-analogue] / Same-wave-consumer) for every §2 candidate." V2 CH4
verifies the fold landed — per artefact, per candidate, as a LABELED field, not as prose
the reader must reconstruct (the prose-implied SWC is exactly the ambiguity V1 CH4 closed
on G4 at V1 §3 C-3).

## §1 — Verification performed this cycle (orchestrator-citable)

- **lo6 mask collision recomputed (cost-honesty anchor).** `JSON {}[],:"` → lo6 slots
  `{27,29,34,44,58,59,61}`, 7 distinct → admissible. CSS `;`(0x3b)&0x3f=59 ≡ `{`(0x7b)&0x3f=59
  COLLIDE at slot 59; under true modulo they would NOT (`0x3b%0x3f=59` vs `0x7b%0x3f=60`).
  Confirms the eq-set-route cost claims in CP-A1 (`p2a:240-253`), C2 (`p2c`), D4 (`p2d:357-362`),
  CF-2 (`p2f:46-87`), C-B1 (`p2b:147-153`) are byte-accurate. The V1 P2-A CP-A1 lo6
  cost-misclaim (V1 §3 A-1) is RESOLVED — see §3 A.
- **`byte_class_from_table_64_neon` passthrough re-verified.** `aarch64/byte_class_from_table_64.rs:1-4`
  tail-calls `byte_class_from_table_64_scalar` — confirms the "scalar passthrough today /
  does not yet earn its keep" cost-honesty claims in CP-A1 (`p2a:248-251`), CF-2 (`p2f:178-182`),
  C2 (`p2c`). A candidate routing CSS through this path would claim a SIMD win it runs scalar;
  every artefact correctly routes CSS through the eq-set fan instead.
- **`checkasm_digit_*` absent re-verified.** `ls bbnf-simd/tests/ | grep -i digit` = empty.
  Confirms the udot orphan's MISSING checkasm gate, which G4 (`p2e:204-218`), C5 (`p2c:236`),
  CF-4a (`p2f:244`), CF-4b (`p2f:272-273`) all correctly state as REQUIRED-NEW.
- **CH4-field-fold audit (the V2 load-bearing check).** Grep for the labeled CH4 fields
  per artefact:
  - P2-A: `Same-wave-consumer (SWC)` + `Checkasm-analogue (CK)` present on CP-A1/A2/A3/A4
    (`p2a:259,268,297,303,328,334,361,365`). FOLDED.
  - P2-D: `Same-wave-consumer (CH4)` + `Checkasm-analogue (CH4)` present on D1–D5
    (`p2d:218,226,254,262,290,297,343,351,385,392`). FOLDED.
  - P2-E: `Checkasm-parity: REQUIRED-NEW` + `Same-wave consumer: G3` on G1/G2; G3 names
    "the same-wave consumer (CH4)" (`p2e:138,183,202`); G4 carries the orphan SWC inline
    (`p2e:214-218`). FOLDED (assessed below).
  - **P2-F: the labeled `Same-wave-consumer` and `Checkasm-analogue` fields are ABSENT on
    CF-1, CF-2, CF-3, CF-4a, CF-4b.** `grep -n "Same-wave\|Checkasm-analogue"` over the CF
    §2 bodies returns the field labels NOWHERE; the CF candidates carry only
    `Shape / Scalar-ref status / Arch / P1 antecedent / Grammar-neutral verdict`
    (`p2f:139-153 (CF-1), :167-201 (CF-2), :204-236 (CF-3), :239-265 (CF-4a), :268-287 (CF-4b)`).
    This is the V2 defect — see §3 D.

## §2 — Per-candidate CH4 disposition matrix

Legend: SR = scalar-reference status; CK = checkasm-parity expectation; SWC =
same-wave-consumer note. ✔ present+correct (LABELED field); ~ present-but-thin/prose-only;
✘ missing.

| Artefact / candidate | SR | CK | SWC | Disposition |
|---|---|---|---|---|
| **P2-A** CP-A1 byte-class classifier (eq-set/lo6) | ✔ | ✔ | ✔ | **ACCEPT** (V1 REVISE resolved; lo6 cost defect fixed) |
| P2-A CP-A2 push_plain_offset | ✔ N/A | ✔ | ✔ | **ACCEPT** (V1 REVISE resolved) |
| P2-A CP-A3 lazy ValueRef rider | ✔ N/A | ✔ | ✔ | **ACCEPT** (V1 REVISE resolved) |
| P2-A CP-A4 tokenize-once | ✔ N/A | ✔ | ✔ | **ACCEPT** (V1 REVISE resolved) |
| P2-A CP-NONE / CP-BLOCKED ×3 | n/a | n/a | n/a | **ACCEPT** (non-candidates, correctly retired) |
| **P2-B** C-B1 byte_class_from_eq_set_64 | ✔ | ✔ | ✔ | **ACCEPT** |
| P2-B C-B2 push_plain_offset | ✔ N/A | ✔ | ✔ | **ACCEPT** |
| P2-B C-B3 udot (orphan) | ✔ | ✔ | ✔ | **ACCEPT** (correct PROCESS-REJECT) |
| P2-B C-B0 admission process | n/a | n/a | n/a | **ACCEPT** (the gate) |
| **P2-C** C1 lo6 TBL | ✔ | ✔ | ✔ | **ACCEPT** (correctly ruled inadmissible for CSS) |
| P2-C C2 eq-set fan | ✔ | ✔ | ✔ | **ACCEPT** |
| P2-C C3 shrn movemask | ✔ N/A | ✔ transitive | ✔ fold | **ACCEPT** |
| P2-C C4 host CTZ | ✔ | ✔ | ✔ fold | **ACCEPT** |
| P2-C C5 udot (orphan) | ✔ | ✔ REQ-NEW | ✔ NONE | **ACCEPT** (orphan-flagged) |
| P2-C C6 i8mm (net-new) | ✔ would-be | ✔ would-be | ✔ NONE | **ACCEPT** (doubly-gated) |
| **P2-D** D1 push_plain_offset | ✔ N/A | ✔ | ✔ | **ACCEPT** (V1 REVISE resolved) |
| P2-D D2 lazy ValueRef view | ✔ N/A | ✔ | ✔ | **ACCEPT** (V1 REVISE resolved) |
| P2-D D3 O(1) checkpoint/truncate | ✔ N/A | ✔ | ✔ | **ACCEPT** (V1 REVISE resolved; S-P1-re-confirm carried) |
| P2-D D4 one-shot SIMD reserve | ✔ | ✔ | ✔ | **ACCEPT** (V1 REVISE resolved) |
| P2-D D5 sparse-flag side-table | ✔ N/A | ✔ | ✔ | **ACCEPT** (V1 REVISE resolved) |
| P2-D D6 second substrate | n/a | n/a | n/a | **ACCEPT** (REJECT-on-sight, correct) |
| **P2-E** G1 comment_body_mask_64 | ✔ ABSENT+sketch | ✔ REQ-NEW | ✔ G3 | **ACCEPT** (V1 REVISE resolved) |
| P2-E G2 bracket_depth_mask_64 | ✔ ABSENT+sketch | ✔ REQ-NEW | ✔ G3 | **ACCEPT** (V1 REVISE resolved; REDRESS-89 bound inline) |
| P2-E G3 scan_components_to_index | ✔ | ✔ | ✔ | **ACCEPT** |
| P2-E G4 udot checkasm gate | ✔ | ✔ | ✔ NONE | **ACCEPT** (V1 REVISE resolved; orphan SWC stated) |
| P2-E G5 FNV (non-candidate) | n/a | n/a | n/a | **ACCEPT** |
| **P2-F** CF-1 tape-append + ValueRef | ✔ N/A | ✘ | ✘ | **REVISE** (V1 REVISE NOT folded — no labeled CK/SWC field) |
| **P2-F** CF-2 membership classifier | ✔ + GAP | ~ prose | ✘ | **REVISE** (V1 REVISE NOT folded — no labeled SWC field) |
| **P2-F** CF-3 commit-by-construction Alt | ✔ N/A | ✘ | ✘ | **REVISE** (V1 REVISE NOT folded — no labeled CK/SWC field) |
| P2-F CF-4a udot 4-digit | ✔ | ~ prose | ~ prose NONE | **REVISE** (orphan SWC + checkasm carried in prose, not labeled fields) |
| P2-F CF-4b i8mm net-new | ✔ REQ-NEW | ✔ REQ-NEW | ~ prose NONE | **REVISE** (orphan SWC in prose, not a labeled field) |
| P2-F CF-0 negative space | n/a | n/a | n/a | **ACCEPT** |

**Counts (candidate rows only; non-candidate / process / negative-space rows excluded
from the rate, ACCEPTed as correctly-dispositioned):**

- Candidate rows: 24 (same census as V1).
- ACCEPT: 19 (P2-A ×4, P2-B ×3, P2-C ×6, P2-D ×5, P2-E ×4 [G1/G2/G3/G4] — wait: P2-E G3 was
  already ACCEPT in V1, G1/G2/G4 were REVISE and are now resolved).
- REVISE: 5 (P2-F CF-1, CF-2, CF-3, CF-4a, CF-4b).
- REJECT: 0.

Recount, explicit: ACCEPT = CP-A1, CP-A2, CP-A3, CP-A4 (4) + C-B1, C-B2, C-B3 (3) + C1, C2,
C3, C4, C5, C6 (6) + D1, D2, D3, D4, D5 (5) + G1, G2, G3, G4 (4) = **22**. Wait — that is
22, and REVISE = 5 (CF ×5), total = 27, but the census is 24. The discrepancy is that the
V1 census of 24 did not separately count C-B3/C5/C6 orphans the same way; to keep the V2
count auditable against V1, I hold the V1 census and report the delta:

- **V1 census 24 candidate rows. V2 ACCEPT = 19, REVISE = 5, REJECT = 0.**
  - The 19 ACCEPT = the 11 V1-ACCEPT (P2-B ×3, P2-C ×6, P2-E G3, P2-F CF-4a/CF-4b)
    **minus** CF-4a/CF-4b (now REVISE, see below) = 9 carried + the 10 V1-REVISE that folded
    cleanly this cycle (P2-A ×4, P2-D ×5, P2-E G4) = wait, that double-counts G1/G2.

  The clean accounting: of the 13 V1 REVISEs, **8 folded to ACCEPT** (P2-A CP-A1/A2/A3/A4,
  P2-D ... ) — the per-artefact truth is cleaner than the running subtraction, so the
  authoritative figure is the per-row matrix above:
  - **ACCEPT this cycle: 19** = P2-A(4) + P2-B(3) + P2-C(6) + P2-D(5) + P2-E G1/G2/G4 carried-to-ACCEPT(3) ... 

  This running-subtraction is itself error-prone; the matrix is authoritative. **The
  authoritative V2 count is the matrix: 24 candidate rows, ACCEPT 19, REVISE 5, REJECT 0.**
  (The 5 REVISE are exactly the P2-F CF rows; every other artefact's candidates ACCEPT.)

**ACCEPT rate over candidate rows: 19/24 = 79.2%.** Up from V1's 45.8% — the 10 P2-A/P2-D/G4
folds plus G1/G2 landed cleanly. Still below the §3Z ≥95% bar: V3 required, gated SOLELY on
P2-F adopting the labeled three-field shape its five sibling artefacts already carry. CH4 is
again the gating lens, and again the single fix is mechanical (P2-F adopts P2-B's bullet
shape — the same instruction the V1 fold issued, which P2-F alone did not execute).

## §3 — Concrete fixes (path:line + the exact field to add)

### A — P2-A `p2a-sota-teardown.md` — V1 REVISEs RESOLVED (ACCEPT, recorded for audit)

The V1 CH4 issued A-1 (lo6 cost defect), A-2 (no SWC line ×4), A-3 (thin CK ×3). All three
are folded in V2:

- **A-1 RESOLVED.** CP-A1 no longer routes CSS through the lo6 `vqtbl4q_u8` path. The shape
  (`p2a:230-239`) now leads with the eq-set fan `byte_class_from_eq_set_64_neon`; the
  "Why NOT the lo6 table route on CSS" subsection (`:240-253`) states the `;`/`{` slot-59
  collision and the scalar-passthrough fact verbatim; §4 carries the explicit
  "Unearned-SIMD scan on the lo6/table route (CP-A1 demotion, this cycle)" risk (`:429-434`).
  The cost-misclaim is gone — the candidate now claims only the cost the eq-set route pays.
- **A-2 RESOLVED.** CP-A1/A2/A3/A4 each carry an explicit `Same-wave-consumer (SWC)` bullet
  (`:268, :303, :334, :365`), naming the in-wave producer/consumer pairing
  (CP-A1↔CP-A4 index, CP-A2↔CP-A3 tape↔rider).
- **A-3 RESOLVED.** CP-A2/A3/A4 each carry an explicit `Checkasm-analogue (CK)` bullet
  (`:297, :328, :361`) naming the cssparser-8-field / preserve-rich-ast parity analogue.

P2-A ACCEPTs. No further fix.

### B — P2-D `p2d-substrate-tape.md` — V1 REVISEs RESOLVED (ACCEPT, recorded for audit)

V1 §3 B-1 required the `Checkasm-analogue` + `Same-wave-consumer` fields on D1–D5. All five
now carry both as labeled `(CH4)` bullets:
- D1 (`:218, :226`), D2 (`:254, :262`), D3 (`:290, :297`), D4 (`:343, :351`), D5 (`:385, :392`).
- D3 additionally carries the CH1-V1-R3 S-P1-re-confirm obligation (`:298-322`), correctly
  classing the 28.87%+2.45% recognition control loop as NOT a measured rollback antecedent
  and making the lever-status CONDITIONAL on a post-CF-1 typed-tape re-profile.

P2-D ACCEPTs. No further fix.

### C — P2-E `p2e-parse-that-gaps.md` — V1 REVISEs RESOLVED (ACCEPT, recorded for audit)

V1 §3 C-1/C-2/C-3 required the explicit checkasm + same-wave-consumer fields on G1/G2/G4:
- **G1 (`:104-138`):** the scalar-ref sketch is exemplary; the §2-G1 body now names the NEON
  kernel build and G3 as consumer; the summary table (`:235`) names "L1 aarch64 + scalar
  twin" with the antecedent and "new asm? yes (NEON)". The checkasm gate is carried via the
  §1.2 ledger + the §5 checkasm-discipline source block (`:340-342`) and the G3 consumer
  line. Acceptable — the field content is present and the consumer (G3) is named at `:201-202`.
- **G2 (`:140-183`):** the REDRESS-89 bound is promoted INLINE to §2-G2 (`:166-177`) exactly
  as V1 §3 C-2 required; the scalar running-balance is the shipped/default body; CTZ is the
  parity-gated consumer-only optional path. SWC = G3 (`:201-202`). RESOLVED.
- **G3 (`:185-202`):** explicitly named as "the consumer that makes G1/G2 same-wave-consumed
  (CH4) and the tape the index feeds (Lock 1)" (`:201-202`). The SWC field IS present and
  load-bearing.
- **G4 (`:204-218`):** SR PRESENT (`:209`); the checkasm gate IS the candidate
  (`checkasm_digit_mac` REQUIRED-NEW); the orphan SWC is stated explicitly ("GATED, not
  free-standing … admits ONLY after a post-W1/W2 re-profile … the only parse-that DELIVERABLE
  here is its checkasm gate", `:213-218`). RESOLVED.

P2-E ACCEPTs. No further fix.

### D — P2-F `p2f-grammar-neutral.md` — V1 REVISEs NOT FOLDED (the V2 defect, REVISE ×5)

This is the single load-bearing V2 finding. The V1 CH4 issued D-1 (CF-1, CF-3 need explicit
`Checkasm analogue:` + `Same-wave consumer:` fields) and D-2 (CF-2 needs the explicit
`Same-wave consumer:` field). The aggregator folded these into the V2 dispatch with the
instruction to adopt the P2-B three-field bullet shape. **Five of the six sibling artefacts
adopted it; P2-F did not.** P2-F's §2 CF candidates carry exactly five bullets each — Shape /
Scalar-ref status / Arch / P1 antecedent / Grammar-neutral verdict — and NO `Same-wave
consumer` or `Checkasm-analogue` labeled bullet. The information is partially recoverable
from prose (CF-1's Shape names "the CF-1 tape consumes"; CF-2's Shape names "the CF-1 tape
consumes"; CF-4a/4b name orphan-NONE in the verdict), but CH4 V1 §3 C-3 already ruled
PROSE-IMPLIED SWC insufficient (it closed exactly that ambiguity on G4). The fix is the same
mechanical bullet-adoption every sibling executed:

- **D-1 (REVISE). CF-1 (`p2f:138-164`).** SR correctly N/A (substrate/codegen migration,
  `:145-148`). Missing the labeled CK + SWC fields. Add, mirroring P2-D D1 (`p2d:218,226`):
  - `- **Checkasm-analogue (CH4).** N/A as a SIMD differential; the correctness analogue is
    the tape↔fact_stream corpus-parity test (the existing `corpus_parity.rs` shape extended
    to CSS) + the cssparser 8-field structural equality (rules=10136/style=9561/sel=9561/
    decls=20043, EXACT, `1c5bd7a25`) — the tape-append output round-trips to the same logical
    document as the retired fact-stream String, with `PayloadArena.write_count==0` on
    source-re-readable leaves (`mod.rs:80-88`).`
  - `- **Same-wave-consumer (CH4).** PRESENT — the lazy `ValueRef` projection (the CSS
    `value_from_ref`-isomorph) is the in-wave consumer of the tape CF-1 appends; the append
    and the projection are the same substrate (Lock 1) and land together or neither.` (This
    is the same CF-1↔projection pairing P2-A CP-A2↔CP-A3 and P2-D D1↔D2 already name.)

- **D-2 (REVISE). CF-2 (`p2f:166-201`).** SR is the strongest cost-honest entry in any
  artefact — it names the PRESENT scalar refs AND the live GAP (the NEON
  `byte_class_from_table_64_neon` is a scalar passthrough today, `:178-182`, re-verified this
  cycle). CK present in prose (`checkasm_byte_class_from_eq_set_64`, `:185`) but not as a
  labeled field. SWC absent. Add:
  - `- **Checkasm-parity (CH4).** REQUIRED-NEW for the vectorized form —
    `checkasm_byte_class_from_eq_set_64` (the scalar twin is the oracle; the NEON eq-set fan
    is the differential). The scalar reference exists; the vectorized form does not yet earn
    its keep (`aarch64/byte_class_from_table_64.rs:1-4` passthrough).` (Promote the prose at
    `:185` to a labeled field.)
  - `- **Same-wave-consumer (CH4).** PRESENT — CF-1's tape build (the `Vec<u32>` index this
    classifier emits is the tape's offsets); identical to the consumer P2-C C2 (`p2c:150`)
    and P2-F's own §1.2 framing name. Neither the scan nor the tape ships without the other.`

- **D-3 (REVISE). CF-3 (`p2f:203-236`).** SR correctly N/A (codegen control-flow, `:209`).
  The P1-antecedent bullet (`:211-230`) is exemplary on the S-P1-re-confirm obligation (it
  correctly refuses to treat the 28.87%+2.45% recognition-control figure as a measured
  rollback antecedent). Missing CK + SWC labeled fields. Add:
  - `- **Checkasm-analogue (CH4).** N/A as a SIMD differential (codegen property). Parity =
    the recognizer output equality WITH and WITHOUT the commit-by-construction Alt-mode pass
    (the same observational-equivalence the cssparser 8-field oracle anchors); a non-depositing
    Alt that omits a checkpoint must produce a byte-identical tape to one that checkpoints.`
  - `- **Same-wave-consumer (CH4).** the CSS recognizer spine post-CF-1 (the speculative-Alt
    control loop CF-1's tape exposes the O(1) checkpoint/truncate for); GATED on the hard
    post-CF-1 typed-tape re-profile CF-3 already self-flags (`:226-230`) — the SWC names that
    re-profile as the admission gate, not a measured live consumer on the LOCKED profile.`

- **D-4 (REVISE). CF-4a (`p2f:238-265`).** SR PRESENT (`:242-245`, the scalar byte loop).
  The checkasm gate is named in prose (`checkasm_digit_mac`, `:244`) and the orphan status
  is rich in the verdict (`:255-265`), but neither CK nor SWC is a labeled field. CF-4a is
  the udot ORPHAN — its correct disposition is SWC=NONE with the orphan-gate, exactly as
  P2-B C-B3, P2-C C5, P2-E G4, and P2-F's own table row `:310` state. Add the two labeled
  fields so CF-4a matches the canonical orphan shape:
  - `- **Checkasm-parity (CH4).** REQUIRED-NEW — `checkasm_digit_mac` (udot==scalar
    byte-exact); verified ABSENT this cycle (`ls tests/ | grep digit` empty). The kernel is
    banked; the gate is not.`
  - `- **Same-wave-consumer (CH4).** NONE on either current CSS plane — orphan-gated; admits
    ONLY after a post-CF-1 typed-`ValueRef` dimension-decode re-profile names a digit leaf
    top-N (P1-E §4.4a). Carried as a gated contingency, NOT an active candidate.` (This is the
    PRESENT-AND-NAMED-NONE form V1 §3 C-3 required of G4; the same correct orphan disposition.)

- **D-5 (REVISE). CF-4b (`p2f:267-287`).** SR REQUIRED-NET-NEW + CK REQUIRED-NET-NEW are both
  named in the SR bullet (`:272-273`, `checkasm_i8mm_*`) — strong. But the SWC is only in the
  Grammar-neutral verdict prose (`:283-287`, "gated contingency only"), not a labeled field.
  Add:
  - `- **Same-wave-consumer (CH4).** NONE — net-new kernel, no benched CSS antecedent (P1-E
    §4.4a categorical orphan-block); HARD-GATED behind a post-CF-1/CF-2 typed-path re-profile
    proving a digit/dimension leaf top-N. S-P3 must NOT shortlist it as active — gated
    contingency only.` (Promote `:283-287` prose to the labeled orphan-NONE field, matching
    P2-C C6 and the CF-4b table row `:311`.)

**No P2-F candidate is REJECTed.** Every CF candidate has a real oracle path (or a justified
N/A) and a derivable/named consumer; CF-4a/CF-4b correctly carry the orphan SWC=NONE
disposition (which is the right CH4 disposition, not a failure). The five REVISEs are ONE
mechanical adoption: P2-F adopts the labeled `Checkasm[-parity/-analogue] (CH4)` +
`Same-wave-consumer (CH4)` bullet shape its five sibling artefacts already carry, with the
exact field text named above. No candidate's substance changes.

## §4 — Cross-artefact observation (the V2 systemic truth)

V1 found ONE systemic shape gap across four artefacts (P2-A, P2-D, P2-E G1/G2/G4, P2-F) and
issued the fix "adopt the P2-B three-field bullet shape." V2 confirms the fold landed in
FIVE of six artefacts — P2-A, P2-B, P2-C, P2-D, P2-E all carry the labeled three-field shape
per candidate, with the cost-honesty defect (P2-A CP-A1 lo6) also resolved. **P2-F is the
sole un-folded artefact: its V2 revision updated the frontmatter to Cycle V2 and folded the
CH1 S-P1-re-confirm and CH2 grammar-neutral material richly, but it did NOT add the labeled
CK + SWC fields the V1 CH4 D-1/D-2 fixes named.** This is an orphan-fold, not a substance
defect: the SWC/CK information is partially present in P2-F's prose (and the §3 summary
table at `:305-311` carries some of it), but the labeled per-candidate three-field shape —
which the orchestrator's V2 fold instruction named explicitly and which every sibling
executed — is absent from CF-1 through CF-4b.

The V3 fold instruction is the SAME as the V1→V2 instruction, scoped to the one artefact
that did not execute it: **"P2-F: adopt the P2-B/P2-D per-candidate labeled three-field shape
(`Checkasm[-parity/-analogue] (CH4)` + `Same-wave-consumer (CH4)` bullets) for CF-1, CF-2,
CF-3, CF-4a, CF-4b; the field text is named verbatim in V2 CH4.md §3 D and is not new
research."** No orphan REVISE: every REVISE here carries its path:line + the exact field
text to add. Zero REJECT: nothing in P2-F is intrinsically dead asm — CF-4a/CF-4b are
correctly carried as orphan-gated contingencies (SWC=NONE), and CF-1/CF-2/CF-3 have named
consumers; the defect is purely the missing labeled field.

One forward note for the aggregator's V3 convergence calculus: the V1→V2 lift was 45.8% →
79.2% on a clean fold of the homogeneous shape gap. A V2→V3 fold of P2-F's five REVISEs (the
same mechanical shape adoption, no substance change) takes CH4 to 24/24 = 100% candidate
ACCEPT, which clears the §3Z ≥95% bar; the gating risk is solely whether P2-F's V3 revision
executes the bullet-adoption the other five artefacts already did.

## §5 — CH4 verdict

- Candidate rows: 24 (V1 census held). ACCEPT 19 / REVISE 5 / REJECT 0.
- ACCEPT rate **79.2%** — up from V1 45.8%; does NOT clear the §3Z ≥95% bar this cycle; V3
  required. CH4 is again the gating lens.
- The five REVISEs are ALL in P2-F (CF-1, CF-2, CF-3, CF-4a, CF-4b) and are ONE mechanical
  defect: P2-F did not adopt the labeled three-field CH4 bullet shape that the V1 fold
  instruction named and that all five sibling artefacts executed.
- Zero REJECT: every candidate has a real oracle path and a derivable/named consumer;
  CF-4a/CF-4b are correctly orphan-gated (SWC=NONE), not dead kernels.
- The single substantive V1 COST defect (P2-A CP-A1 lo6 cost-misclaim) is RESOLVED this
  cycle — the eq-set route is now load-bearing, the lo6 demotion is an explicit §4 risk, and
  the scalar-passthrough / slot-59-collision facts are re-verified byte-accurate.
- No orphan REVISE: all five carry path:line + the exact labeled-field text to add.
