# SK-V17 S-P2 V1 — CH3 REGRESSION

Lens: CH3 REGRESSION. Cycle: V1. Date: 2026-05-29.
Pass: S-P2 Research, PASS-2-RESEARCH §3 CH3 + ORCHESTRATOR §3W.
Master HEAD: `0ae1caa52`. S-P1 LOCKED at `0ae1caa52`.
Subject: `restart/skinny/tranches/sk-v17/research/p2/{p2a..p2f}.md`.

## §0 — Mandate + method

CH3 disposition is narrow: **does any candidate/section re-open a `skinny/REDRESS.md`
pre-block route — AZ-IV eager, StructRegistry indirection, fact-stream-as-output,
the 24-row broadcast, FNV/fixture, or the historical blocked instruction routes
(28+33 Class-A NEON tiny-string, 50-55 UTF-8 fusion, 60-72 retained-parse + sidecar
producers + digest cap-16, 80 mantissa-widen, 82-84 single-quartet/StringBlock16/
object-pair, 88 PMULL prefix-XOR hot body, 89 CSSC CTZ next-bit bulk consumer) —
WITHOUT fresh P1 evidence + a new framing?** A re-open without both is REJECT;
antecedent-only / cost-only gaps belong to CH1 / CH4 and are not double-counted here.

The authoritative pre-block surface used for scoring: `SYNTHESIS.md` §0.4
(:189-225, inc. the inherited-family id list `28+33, 50-55, 60-72, 80, 82-84, 88,
89, 96-98, 183/184/209-213, 215, 242-247, FNV closed-enum`), cross-checked against
`skinny/REDRESS.md` Item 88 (:2510 PMULL prefix-XOR hot body, measured -10/-12/-15%),
Item 89 (:2544 CSSC CTZ bulk consumer + B6 fold), Items 82-84 (:2285-2396), Items
50-55 (:763-784 parser-local cursor / `Vec<JsonEvent>` / whitespace sidecar /
precomputed `StructuralIndex` retained), and the W6/W11 AZ-IV typed-CSS reject +
W10 FNV quarantine + W1/W6 broadcast-diagnostic (:6294-6444).

## §1 — Census

| Artefact | Active candidates | Non-candidate / gated / anchor entries |
|---|---|---|
| p2a | CP-A1, CP-A2, CP-A3, CP-A4 | CP-NONE (FNV), CP-BLOCKED (udot, asmjson) |
| p2b | C-B1, C-B2 | C-B3 (process-rejected udot), C-B0 (admission process) |
| p2c | C1→C2, C2, C3, C4 | C5 (udot orphan), C6 (i8mm orphan), 7 REDRESS-block flags |
| p2d | D1, D2, D3, D4, D5 | D6 (second-substrate reject-on-sight anchor) |
| p2e | G1, G2, G3 | G4 (udot gated), G5 (FNV non-candidate) |
| p2f | CF-1, CF-2, CF-3 | CF-4a (udot orphan), CF-4b (i8mm reject), CF-0 (negative space) |

Active candidate count scored by CH3: **21** (CP-A1..A4, C-B1/B2, C1/C2/C3/C4,
D1..D5, G1/G2/G3, CF-1/CF-2/CF-3). Non-candidate / gated / anchor entries scored
for completeness: **15**. Total dispositioned sections: **36**.

## §2 — Dispositions

### ACCEPT (no pre-block re-open; fresh P1 antecedent + new framing both present)

- **CP-A1 / C-B1 / C2 / CF-2 (the eq-set byte-class scan).** All four are the same
  `byte_class_from_eq_set_64` route. Each carries an explicit §4 disambiguation from
  the blocked routes it is NOT: p2a §4 (`p2a:360-370`) separates the 64-byte Class-B
  structural classifier from REDRESS-28/33 `match_tiny_plain_string`, from 82-84, and
  from 88/89 (transient producer, not a retained hot body / cross-call carry). p2c
  §3.3 (`p2c:322-326`) names the same. Fresh framing: the candidate is the
  `select_classifier(alphabet)` 64-byte route, never the tiny-string/quartet kernel.
  P1 antecedent: `find_component_delim` 56-59% + `consume_balanced_at` 10-11%. ACCEPT.

- **C1 (lo6 TBL) — verdict INADMISSIBLE, route falls to C2.** p2c measured the
  `;`/`{` mod-0x3f collision (`p2c:152-157`), so C1 is honestly retired in favour of
  C2 by the `lo6_table_admissible` guard — not a re-open of anything. The same
  finding is independently re-derived in p2f §1.2 (`p2f:53-83`) and used to hold CF-2
  to the admissible backend. This is the strongest cross-artefact convergence in the
  pool. ACCEPT (as a route-elimination, not a candidate).

- **CP-A2 / C-B2 / D1 / CF-1 (tape-append `push_plain_offset`).** Directly counter-
  designs three pre-blocks at once and names all three: AZ-IV eager (lazy-by-default,
  `write_count==0` invariant — `p2d:347-352`, `p2b:232-236`), StructRegistry
  indirection (single non-generic `TapeBuilder`, codegen-monomorphised not runtime
  `Arena<G>` — `p2d:353-357`, `p2a:349-353`), and fact-stream-as-output (RETIRES
  `emit_fact_stream` as the live plane — `p2a:354-356`, `p2d:358-360`, `p2f:341-343`).
  Fresh P1 antecedent: `emit_fact_stream` 24.59% self + 91.44% of the 57.63% alloc
  floor. ACCEPT.

- **CP-A3 / D2 / CF-1 lazy `ValueRef` rider.** The direct AZ-IV counter-design.
  Re-open test is named and guarded in every artefact: no eager per-leaf `Box::new`,
  no f64-alloc-per-number, `PayloadArena` `write_count`/`allocation_count` ~0 on the
  structural path (`p2d:347-352`, `p2a:344-348`, `p2f:294`). Fresh framing: kind
  recovered from the source byte at the offset (no stored tag), isomorphic to JSON's
  proven `value_from_ref` (`json/value.rs:143`). ACCEPT.

- **CP-A4 / CF-3 (tokenize-once / commit-by-construction Alt-mode).** Bounded to the
  single-substrate REDRESS-53 shape: the index is the tape, consumption is a
  per-grammar template, no parser-local second cursor (`p2a:336`, `p2f:332-334`).
  CF-3 additionally self-flags its P1 antecedent as weak/post-CF-1 with a hard
  re-confirm obligation (`p2f:207-217`) — a CH1 concern, not a CH3 re-open. ACCEPT
  for CH3 (REDRESS-53 not re-opened).

- **C3 (shrn movemask) / C4 (host CTZ first-match, fold-only).** C3 is a sub-task,
  not a standalone primitive. C4 is the single `trailing_zeros` extract already in
  `find_ascii_set_member64` — p2c explicitly bounds it AWAY from the REDRESS-89 bulk
  consumer ("the admissible form is C4 ... NOT a CTZ-driven bulk emit loop",
  `p2c:219-222`, restated `p2c:313-320`). Fresh framing present; no re-open. ACCEPT.

- **D3 (O(1) checkpoint/truncate) / D4 (one-shot SIMD reserve) / D5 (sparse-flag
  side-table).** D3 is the already-banked O(1) marker (`8153236e8`). D4 reuses the
  shared scan count, gated behind D1/D2. D5 stores opaque flag bits in the EXISTING
  sparse side-vectors and explicitly does NOT widen the per-position record nor
  become a retained class column. D5 carries a self-imposed Lock-14 guard
  (`p2d:305-313`) that the flag semantics must be a `BackendRule` branch-tag, not a
  relocated `W5C_REQUEST_FACT_PROFILES` — a CH2 REVISE trigger, not a CH3 re-open.
  None re-opens 60-72 sidecar/class-column (the side-table is the existing sparse
  flags, not a new substrate). ACCEPT.

- **D6 (second-substrate reject-on-sight anchor).** Records `StructLayout`/
  `TapeStructBuilder`/`TapeCursor`/sidecar/retained-cursor/aux-density-table as
  REJECT under Lock 1 (`p2d:315-324`). This is the explicit CH5/CH3 anchor; it
  proposes nothing. ACCEPT.

- **G1 `comment_body_mask_64` (NET-NEW).** No prior REDRESS entry exists for a
  comment-region mask (grep of REDRESS.md: zero `comment_body` / `region.fill`
  hits). It is genuinely net-new, not a re-open. Region fill reuses the
  `escape_mask_64` `overflowing_add` carry idiom (`lib.rs:188`), NOT PMULL — p2e §4
  names this explicitly to stay clear of REDRESS-88 (`p2e:263-266`). Fresh P1
  antecedent: the comment-skip arm of `find_component_delim`/`consume_balanced_at`.
  ACCEPT.

- **G3 `scan_components_to_index` (composition).** The single sharpest CH3 edge:
  REDRESS-53 (:766) blocks a "precomputed `StructuralIndex`" — but that block is for
  a PARSER-LOCAL second scanner producing a sidecar parallel to a retained parse.
  G3's fresh framing is Lock-1 v+1: the produced `Vec<u32>` IS the tape's `offsets`
  (the structural projection IS the tape, `LOCKS.md:75`), carry/depth threads WITHIN
  a single call and resets per parse (`p2e:276-280`). This framing is independently
  backed by p2d §1.4's substrate-union conclusion (`p2d:114-149`). With the
  index==tape-offsets identity enforced, REDRESS-53 is not re-opened. ACCEPT — with
  the §3 coupling note that S-P3 MUST carry the identity verbatim (a G3 that retains
  the index as a vector parallel to a parse, rather than AS the tape offsets,
  collapses back into REDRESS-53 and would CH3-REJECT at implementation).

- **C-B0 admission process / the 7 REDRESS-block flags in p2c §3 / p2b §4 / p2e §4 /
  p2f §4.** These are the deliverable: each artefact's §4 enumerates the pre-block
  ledger with per-route refutation (p2c §3 numbers all 7 instruction routes with
  measured refutations and re-open tests; p2b/p2e/p2f §4 ledgers cover AZ-IV/
  StructRegistry/fact-stream/broadcast/FNV/x86/SVE/Lock-1). This is exactly the
  CH3-defensive posture the pass asks for. ACCEPT.

- **All non-candidate / orphan-blocked entries (CP-NONE, CP-BLOCKED, C-B3, C5, C6,
  G4, G5, CF-0, CF-4a, CF-4b for the CH3 axis).** The FNV leaf (CP-NONE/G5/CF-0) is
  recorded as a non-candidate that RETIRES with the fact-stream — it re-opens no
  pre-block; any FNV/hex kernel proposal would, and all four artefacts pre-emptively
  REJECT that. The udot/i8mm orphans (C5/C6/G4/CF-4a/CF-4b/C-B3) are orphan-blocked
  on the no-antecedent axis (CH1/CH4), gated behind a future typed-path re-profile;
  they do not re-open a named REDRESS *route*. asmjson collapsed-stage is x86-host-
  blocked, correctly excluded. ACCEPT for CH3 (these are CH1/CH4 surfaces, not CH3
  re-opens; flagged below as cross-lens hand-offs).

### REVISE (touches a pre-block; framing imprecise or under-bounded — fix, do not drop)

- **R1 — p2c C2 / §3.3 `vdupq_n_u8` broadcast disambiguation.**
  `p2c:184` and `p2c:294`(implicit) parenthetically state the per-member
  `vdupq_n_u8` broadcast is "NOT the REDRESS-blocked runtime-broadcast pattern."
  There is no REDRESS entry for a *SIMD runtime-broadcast* route — the "broadcast"
  pre-block is the **24-row evidence-measurement broadcast** (one timing tuple
  projected across N rows; `SYNTHESIS.md:209-211`, REDRESS W1/W6 broadcast-diagnostic
  :6300/:6322). The disambiguation is harmless in intent (it correctly excludes a SIMD
  op from being a concern) but cites a pre-block that does not exist under that
  description, which muddies the CH3 ledger.
  **Fix (path:line):** `p2c-arch-esoterica.md:184` — change "the REDRESS-blocked
  runtime-broadcast pattern" to "(this is a compile-time per-member fixed-loop
  `vdupq`, unrelated to the §0.4 24-row evidence-broadcast pre-block, which concerns
  measurement rows not SIMD ops)". p2a §4 (`p2a:371-372`) already frames the
  broadcast pre-block correctly (per-corpus N≥50 median, not one tuple across N rows);
  align p2c to that.

- **R2 — p2e G2 CTZ "ranges" path vs REDRESS-89 (under-bounded in §2, bounded only
  in §4).** G2's §2 shape (`p2e:153-158, 159-161`) admits a CTZ-iteration "ranges"
  path via `bitmap_next_set_bit` as the cheaper alternative body, with the REDRESS-89
  bound stated only later in §4 (`p2e:257-262`). The §2 prose reads as if the CTZ
  ranges path is co-equal with the scalar running balance, when the binding
  disposition is: scalar running balance is the spec/default, CTZ is admitted ONLY as
  a consumer of the precomputed depth mask, gated by checkasm parity, never the
  default per-byte body. The framing IS fresh (depth-mask ranges, distinct from
  W10b's bulk prefix-XOR consumer + B6 fold) and the antecedent IS present
  (`consume_balanced_at` 11%), so this is REVISE not REJECT.
  **Fix (path:line):** `p2e-parse-that-gaps.md:153-161` — move the §4 REDRESS-89
  bound inline into the G2 §2 shape: state up front that the scalar running balance
  is the spec and default-shipped body, and the CTZ-ranges path is a checkasm-gated
  consumer-only refinement that S-P3 must carry with the "REVISE-back to consumer
  framing if shortlisted as unconditional body" condition verbatim (the condition is
  already correctly stated at `p2e:262` — promote it into §2 so the candidate cannot
  be read as proposing CTZ-as-default).

- **R3 — p2e G2 / p2c C4 cross-artefact CTZ consistency note.** p2c §3.2 (`p2c:313-
  320`) declares the admissible CTZ form is C4 (single `trailing_zeros` extract per
  mask) and the "CTZ-driven bulk-iterate consumer body" is REDRESS-89-blocked. p2e
  G2 admits a CTZ-iteration ranges path. These are reconcilable (G2's is gated/
  consumer-only, not a bulk body), but the two artefacts do not cross-reference, so a
  reader could see p2c forbidding what p2e admits. REVISE to add the cross-link.
  **Fix (path:line):** `p2e-parse-that-gaps.md:159-161` — add an explicit reference
  that the G2 CTZ-ranges consumer is the same family p2c §3.2 (`p2c:313-320`) bounds,
  and the two artefacts agree that the bulk-default form is REDRESS-89-blocked while a
  parity-gated consumer of a precomputed mask is admissible; OR, if S-P3 prefers the
  conservative read, drop the CTZ-ranges path entirely and ship only the scalar
  running balance (the artefact already states this alone is "a measurable win",
  `p2e:156-158`).

### REJECT (re-opens a pre-block without fresh P1 evidence + new framing)

- **None.** No candidate re-opens AZ-IV eager, StructRegistry indirection,
  fact-stream-as-output, the 24-row broadcast, FNV/fixture, or any historical blocked
  instruction route (28+33, 50-55, 60-72, 80, 82-84, 88, 89) without both fresh P1
  evidence and a new framing. The eager/registry/fact-stream/broadcast/FNV pre-blocks
  are each affirmatively counter-designed and named; the instruction-route pre-blocks
  (88/89/82-84/28+33) are flagged with refutations and bounded away from in every §4.

## §3 — Coupling notes carried to S-P3 (CH3 conditions on ACCEPTed candidates)

These are not REVISE dispositions (no artefact edit required) but binding conditions
the S-P3 shortlist MUST carry so an ACCEPTed candidate does not silently re-open a
pre-block at implementation:

1. **G3 index==tape-offsets identity.** `scan_components_to_index`'s `Vec<u32>` must
   BE the tape's `offsets` (Lock-1 v+1). A G3 that retains the index as a vector
   parallel to a retained parse re-opens REDRESS-53 and CH3-REJECTs at the wave. The
   tape-activation gate's `PayloadArena`/`offset_count` counters are the proof
   surface (`p2d:103-112`).

2. **D5 flag semantics = `BackendRule` branch-tag, never a per-rule catalogue.** A
   D5 flag table keyed by rule-id is a relocated `W5C_REQUEST_FACT_PROFILES`
   (Lock-14 phrase-#1 overfit re-entry, `SYNTHESIS.md:203-208`). CH2's REVISE
   trigger; CH3 flags it because the relocation also re-opens the fact-stream routing
   pre-block in flag form.

3. **CF-1 routing derived-from-grammar.** Identical condition: the tape lowering must
   not relocate the per-rule branching into projection DATA. Already a CH2 condition
   (`p2f:155-160`); CH3 records it because the `W5C_REQUEST_FACT_PROFILES` retirement
   is itself a §0.4 pre-block (`SYNTHESIS.md:203`).

4. **G2 scalar-balance default.** Per R2: the default-shipped G2 body is the scalar
   running balance; the CTZ-ranges path is consumer-only + parity-gated. S-P3 must
   not shortlist G2 with CTZ as the unconditional body (REDRESS-89).

## §4 — Cross-lens hand-offs (not CH3 surfaces)

- **CH1 (antecedent):** C5/C6/G4/CF-4a/CF-4b/C-B3 (udot + i8mm) have NO benched CSS
  P1 antecedent — orphan-blocked, gated behind a post-W1/W2 typed-path re-profile.
  CH3 does not REJECT these (they re-open no named REDRESS route); CH1 owns the
  speculative-kernel disposition. CF-4a's §2 prose is internally tense ("unconditional
  orphan retirement" vs "ONLY if a re-profile shows a digit leaf", `p2f:247-252`) —
  flag to CH1 for the antecedent verdict.
- **CH4 (cost):** G1/G2 are NET-NEW kernels requiring net-new scalar twins + new
  checkasm differentials (absent today); G4 needs a `checkasm_digit_mac` (absent). CH4
  owns the scalar-ref/checkasm/same-wave-consumer triple. CH3 notes the kernels are
  new (not re-opens) and defers the cost gate to CH4.
- **CH5 (hidden coupling):** D6 is the explicit substrate-union anchor; G3's
  index==tape identity (§3.1) is the Lock-1 surface CH5 owns. CH3's REJECT-test for a
  retained sidecar overlaps CH5's; both should agree the substrate union holds.

## §5 — Counts

- Sections dispositioned: **36** (21 active candidates + 15 non-candidate/gated/anchor).
- REVISE attaches to **2 distinct sections**: p2c C2/§3.3 (R1) and p2e G2 (R2+R3 both
  target G2). So at the section grain: **34 ACCEPT, 2 REVISE, 0 REJECT**.
- **REVISE: 3 disposition items** across those 2 sections (R1 p2c broadcast citation;
  R2 p2e G2 §2 CTZ bound placement; R3 p2e/p2c CTZ cross-artefact consistency).
- **REJECT: 0.**
- **ACCEPT rate (all 36 sections): 34/36 = 94.4%.**
- **ACCEPT rate (21 active candidates, the 2 REVISE-touched candidates counted
  against): 19/21 = 90.5%.**

The three REVISE dispositions are framing/citation corrections on candidates whose
underlying route is CH3-clean (fresh antecedent + new framing both present); none is
a pre-block re-open. No orphan REVISE: each REVISE carries a concrete path:line fix.
CH3 verdict: the S-P2 V1 pool re-opens no REDRESS pre-block; fold R1-R3 into V2.
