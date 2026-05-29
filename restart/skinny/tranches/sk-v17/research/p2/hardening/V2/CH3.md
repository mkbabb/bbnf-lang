# SK-V17 S-P2 V2 — CH3 REGRESSION

Lens: CH3 REGRESSION. Cycle: V2. Date: 2026-05-29.
Pass: S-P2 Research, PASS-2-RESEARCH §3 CH3 + ORCHESTRATOR §3W.
Master HEAD: `0ae1caa52`. S-P1 LOCKED at `0ae1caa52`.
Subject: `restart/skinny/tranches/sk-v17/research/p2/{p2a..p2f}.md` (all Cycle V2).
Prior: `p2/hardening/V1/CH3.md` (34 ACCEPT / 2 REVISE-sections / 0 REJECT; R1-R3 + 4 coupling notes).

## §0 — Mandate + method

CH3 disposition is narrow: **does any candidate/section re-open a `skinny/REDRESS.md`
pre-block route — AZ-IV eager, StructRegistry indirection, fact-stream-as-output, the
24-row evidence broadcast, FNV/fixture, or the historical blocked instruction routes
(28+33 Class-A NEON tiny-string, 50-55 UTF-8 fusion, 60-72 retained-parse + sidecar
producers + digest cap-16, 80 mantissa-widen, 82-84 single-quartet/StringBlock16/
object-pair, 88 PMULL prefix-XOR hot body, 89 CSSC CTZ next-bit bulk consumer, plus the
historical NEON/UTF8 blocked routes) — WITHOUT fresh P1 evidence + a new framing?** A
re-open without both is REJECT; antecedent-only / cost-only gaps belong to CH1 / CH4 and
are not double-counted here.

V2 method has two arms:
1. **Fold-verification.** Confirm every V1 CH3 disposition (R1, R2, R3) and every V1 §3
   coupling note actually landed in the V2 artefact text — a fold that did not land is an
   orphan REVISE (ORCHESTRATOR §3Z zero-orphan-REVISE bar) and re-opens the V1 finding.
2. **Fresh scan.** Re-disposition every active candidate + non-candidate/anchor at the new
   V2 line numbers, and grep the pre-block re-open tests (PMULL/`vmull_p64` on a hot body;
   retained cursor / sidecar / `StructLayout`/`TapeStructBuilder`/`TapeCursor`/`UnionTape`;
   FNV/hex kernel; x86/SVE; eager `Box::new` per leaf; CTZ bulk-default body) for any NEW
   unflagged re-open introduced in V2.

Authoritative pre-block surface: `SYNTHESIS.md` §0.4 inherited-family id list
(`28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, FNV
closed-enum`), cross-checked against `skinny/REDRESS.md` Item 88 (:2510 PMULL prefix-XOR
hot body, measured -10/-12/-15%), Item 89 (:2544 CSSC CTZ bulk consumer + B6 fold), Items
82-84 (:2285-2396), Items 50-55 (:763-784), Item 90 (:2589 PMULL/CSSC remain rejected
:2595), the W6/W11 AZ-IV typed-CSS reject + W10 FNV quarantine + W1/W6
broadcast-diagnostic.

## §1 — Census (V2)

| Artefact | Active candidates | Non-candidate / gated / anchor entries |
|---|---|---|
| p2a | CP-A1, CP-A2, CP-A3, CP-A4 | CP-NONE (FNV), CP-BLOCKED (udot, asmjson) |
| p2b | C-B1, C-B2 | C-B3 (process-rejected udot), C-B0 (admission process) |
| p2c | C1→C2, C2, C3, C4 | C5 (udot orphan), C6 (i8mm gated contingency), 7 REDRESS-block flags |
| p2d | D1, D2, D3, D4, D5 | D6 (second-substrate reject-on-sight anchor) |
| p2e | G1, G2, G3 | G4 (udot gated), G5 (FNV non-candidate) |
| p2f | CF-1, CF-2, CF-3 | CF-4a (udot orphan), CF-4b (i8mm reject), CF-0 (negative space) |

Active candidate count scored by CH3: **21** (CP-A1..A4, C-B1/B2, C1/C2/C3/C4, D1..D5,
G1/G2/G3, CF-1/CF-2/CF-3). Non-candidate / gated / anchor entries scored for completeness:
**15**. Total dispositioned sections: **36**. Census is unchanged from V1 (same 21+15); no
candidate was added or removed in V2. (p2c C6 carries a sharpened V2 header — "GATED
CONTINGENCY, NOT an active candidate (twins P2-F CF-4b)" — but it is the same i8mm
non-candidate V1 scored; it is a CH1 antecedent surface, not a CH3 re-open: `usmmla` is
grep-clean-absent and net-new, not a NAMED blocked REDRESS *route*.)

## §2 — Fold-verification of V1 CH3 dispositions (the load-bearing V2 check)

The three V1 REVISE items + the four V1 §3 coupling conditions are confirmed landed in the
V2 text. None is an orphan REVISE.

- **R1 — p2c broadcast citation.** V1 fix: replace "the REDRESS-blocked runtime-broadcast
  pattern" with an explicit per-member-`vdupq` vs 24-row-evidence-broadcast disambiguation.
  **FOLDED.** `p2c-arch-esoterica.md:190-194` now reads: `vdupq_n_u8` "is an ordinary NEON
  splat instruction and is wholly unrelated to the §0.4 'broadcast' pre-block, which
  forbids the *evidence-measurement* practice of projecting one CSS timing tuple across 24
  conceptual rows, SYNTHESIS §0.2/§0.4, p2a:371-372; that pre-block governs benchmark rows,
  not SIMD ops". Aligned to p2a §4's framing exactly as V1 directed.

- **R2 — p2e G2 §2 CTZ bound placement.** V1 fix: move the §4 REDRESS-89 bound inline into
  the G2 §2 shape; state the scalar running balance is the spec/default-shipped body and the
  CTZ-ranges path is a checkasm-gated consumer-only refinement, with the "REVISE-back to
  consumer framing if shortlisted as unconditional body" condition promoted into §2.
  **FOLDED.** `p2e-parse-that-gaps.md:166-177`: "**The shipped/default body is the scalar
  running balance over the two precomputed masks**", and the CTZ-ranges refinement "is NOT
  co-equal: it is a checkasm-gated, parity-proven, CONSUMER-ONLY optional path admitted only
  after it beats the scalar balance on the bench — and if S-P3 ever shortlists it as the
  UNCONDITIONAL body, CH3 must REVISE it back to the consumer framing (the §4 REDRESS-89
  bound, promoted inline here per the V1 CHALLENGE)." The §2-G2 depth-carry text (`:150-154`)
  also now carries the within-chunk / no-cross-call-retention invariant up front.

- **R3 — p2e/p2c CTZ cross-artefact consistency.** V1 fix: add a cross-link so a reader does
  not see p2c §3.2 forbidding what p2e G2 admits. **FOLDED.** `p2e:174-175` and the §4 bound
  at `p2e:282-285` both explicitly name `p2c-arch-esoterica.md §3.2`: "the two artefacts are
  reconciled — bulk-default CTZ is REDRESS-89-blocked, and a parity-gated
  consumer-of-a-precomputed-mask is the only admissible CTZ use." p2c §3.2 (`p2c:343-350`)
  states the reciprocal: admissible form is C4 single-`trailing_zeros`, bulk CTZ consumer is
  Item-89-blocked. The two artefacts now agree in text.

V1 §3 coupling conditions (binding on the S-P3 shortlist) — confirmed self-bound in V2:

- **G3 index==tape-offsets identity (V1 §3.1).** Present `p2e:299-303`: "G3 emits ONLY the
  structural index; if the offsets are retained, the structural projection IS the tape
  (`LOCKS.md:75`). No `UnionTape`, no second substrate … carry/depth threads WITHIN a single
  `scan_components_to_index` call, reset per parse." REDRESS-53 not re-opened.
- **D5 flag semantics = `BackendRule` branch-tag (V1 §3.2).** Present `p2d:396-397` ("ONLY as
  a `BackendRule` branch-tag projection … never a hand-curated per-rule catalogue") and the
  §3/§4 guard `p2d:404-412, 460-463` (relocated `W5C_REQUEST_FACT_PROFILES` in flag form ⇒
  CH2 REVISE). The fact-stream-routing-in-flag-form re-entry is fenced.
- **CF-1 routing derived-from-grammar (V1 §3.3).** Present `p2f:159-164` and §4 item 1
  (`p2f:340-344`): "relocating `W5C_REQUEST_FACT_PROFILES`' per-rule branching … into a
  projection-data table is the overfit re-entry seam and is FORBIDDEN. Every residual CSS
  routing entry must name its `.bbnf` rule."
- **G2 scalar-balance default (V1 §3.4 / R2).** Folded into §2-G2 as above; S-P3 cannot read
  G2 as proposing CTZ-as-default.

## §3 — Fresh dispositions (V2, at the new line numbers)

### ACCEPT (no pre-block re-open; fresh P1 antecedent + new framing both present)

- **CP-A1 / C-B1 / C2 / CF-2 (the eq-set byte-class scan).** Same `byte_class_from_eq_set_64`
  route, now with the lo6-`& 0x3f`-collision finding independently re-derived in all four
  artefacts (p2a:240-253, p2b:148-157, p2c:155-163, p2f:171-174,191-201). Each carries the
  §4 disambiguation from the blocked routes it is NOT: p2a §4 (`p2a:435-441`) separates the
  64-byte Class-B classifier from REDRESS-28/33 `match_tiny_plain_string` and 82-84; p2c §3
  flags 3 (`p2c:352-356`) and 4 (`p2c:358-362`) name the same. Fresh framing: the
  `select_classifier(alphabet)` 64-byte route, never the tiny-string/quartet kernel. P1
  antecedent: `find_component_delim` 56.52-59.24% + `consume_balanced_at` 10.31-11.05%. ACCEPT.

- **C1 (lo6 TBL) — verdict INADMISSIBLE, route falls to C2.** p2c measured the `;`/`{`
  `& 0x3f` slot-59 collision (`p2c:155-163`), honestly retiring C1 in favour of C2 via the
  `lo6_table_admissible` guard — a route-elimination, not a re-open. Re-derived in p2f §1.2
  (`p2f:171-174,308`) and p2b (`p2b:148-157`). ACCEPT (as route-elimination).

- **CP-A2 / C-B2 / D1 / CF-1 (tape-append `push_plain_offset`).** Counter-designs three
  pre-blocks at once and names all three: AZ-IV eager (lazy-by-default,
  `PayloadArena write_count==0` — `p2d:258-261,449-451`, `p2a:413-417`), StructRegistry
  indirection (single non-generic `TapeBuilder`, codegen-monomorphised — `p2d:452-456`,
  `p2a:418-422`, `p2f:340`), fact-stream-as-output (RETIRES `emit_fact_stream` as the live
  plane — `p2a:423-425`, `p2d:457-459`, `p2b:254-258`). Fresh P1 antecedent: `emit_fact_stream`
  24.59-25.01% self + 91.44% of the ~58-64% alloc floor. ACCEPT.

- **CP-A3 / D2 / CF-1 lazy `ValueRef` rider.** Direct AZ-IV counter-design; the re-open test
  is named and guarded in every artefact: no eager per-leaf `Box::new`, no f64-alloc-per-
  number, `PayloadArena write_count==0` on the source-re-readable path (`p2d:258-261,446-451`,
  `p2a:413-417`, `p2f:153-158`). Fresh framing: kind recovered from the source byte at the
  offset (no stored tag), isomorphic to JSON's proven `value_from_ref` (`json/value.rs:143`).
  ACCEPT.

- **CP-A4 / CF-3 (tokenize-once / commit-by-construction Alt-mode).** Bounded to the
  single-substrate REDRESS-53 shape: the index IS the tape, consumption is a per-grammar
  template, no parser-local second cursor (`p2a:350-358`, `p2f:203-208`). CF-3 self-flags its
  P1 antecedent as a hard blocking post-CF-1 re-confirm obligation (`p2f:211-230`) — a CH1
  concern, not a CH3 re-open. ACCEPT for CH3 (REDRESS-53 not re-opened).

- **C3 (shrn movemask) / C4 (host CTZ first-match, fold-only).** C3 is a sub-task, not a
  standalone primitive (`p2c:201-218`). C4 is the single `trailing_zeros` extract already in
  `find_ascii_set_member64`, explicitly bounded AWAY from the REDRESS-89 bulk consumer
  (`p2c:229-232` "promoting CTZ into a *dedicated bulk next-bit consumer* … is the
  REDRESS-89-rejected route … fold-only, no new bulk consumer"; restated `p2c:343-350`).
  Fresh framing present; no re-open. ACCEPT.

- **D3 (O(1) checkpoint/truncate) / D4 (one-shot SIMD reserve) / D5 (sparse-flag side-table).**
  D3 is the already-banked O(1) marker (`8153236e8`), with its lever-status correctly held
  CONDITIONAL on a post-CF-1 typed-tape re-profile (`p2d:305-322`) — a CH1 antecedent honesty,
  not a re-open. D4 reuses the shared scan count, gated behind D1/D2 + the NEON scan
  (`p2d:351-355,371-373`). D5 stores opaque flag bits in the EXISTING sparse side-vectors,
  does NOT widen the per-position record, and carries the self-imposed Lock-14
  `BackendRule`-branch-tag guard (`p2d:404-412`) — the V1 §3.2 condition now self-bound. None
  re-opens 60-72 sidecar/class-column (the side-table is the existing sparse flags, not a new
  substrate). ACCEPT.

- **D6 (second-substrate reject-on-sight anchor).** Records `StructLayout`/`TapeStructBuilder`/
  `TapeCursor`/sidecar/retained-cursor/aux-density-table/`UnionTape`/cross-call-carry as
  REJECT under Lock 1 (`p2d:414-423`). Explicit CH5/CH3 anchor; proposes nothing. ACCEPT.

- **G1 `comment_body_mask_64` (NET-NEW).** No prior REDRESS entry for a comment-region mask.
  Region fill reuses the `escape_mask_64` `overflowing_add` carry idiom (`lib.rs:188`), NOT
  PMULL — p2e §4 (`p2e:286-289`) names this to stay clear of REDRESS-88. Now also
  digraph-parameterised at the §2 signature (`p2e:120-134`), the Lock-14 fix V1 required.
  Fresh P1 antecedent: the comment-skip arm of `find_component_delim`/`consume_balanced_at`
  (`generated.rs:300,329,342`). ACCEPT.

- **G2 `bracket_depth_mask_64`.** R2/R3 folded (§2 above): scalar running balance is the
  default body, CTZ-ranges is consumer-only + parity-gated + REVISE-back-conditioned, cross-
  linked to p2c §3.2. The within-chunk depth-carry / no-cross-call-retention invariant is at
  the §2 shape (`p2e:150-154`). REDRESS-89 not re-opened. ACCEPT.

- **G3 `scan_components_to_index` (composition).** REDRESS-53 (:766) blocks a "precomputed
  `StructuralIndex`" for a PARSER-LOCAL second scanner producing a sidecar parallel to a
  retained parse. G3's fresh framing (Lock-1 v+1): the produced `Vec<u32>` IS the tape's
  `offsets`, carry/depth threads WITHIN a single call and resets per parse (`p2e:191-192,
  299-303`). Backed by p2d's substrate-union conclusion. ACCEPT — with the §4 coupling note
  (carried from V1 §3.1) that S-P3 MUST carry the index==tape-offsets identity verbatim; a G3
  that retains the index as a vector parallel to a parse collapses into REDRESS-53 and
  CH3-REJECTs at the wave.

- **C-B0 admission process / the 7 REDRESS-block flags (p2c §3) / the §4 ledgers (p2a/p2b/p2e/
  p2f).** The deliverable: each artefact's §4 enumerates the pre-block ledger with per-route
  refutation. p2c §3 numbers all 7 instruction routes with measured refutations + re-open
  tests (`p2c:330-386`): PMULL-88 (re-open test = `pmull`/`vmull_p64` text in the CSS prefix-
  XOR/string-region hot path, `p2c:340-341`), CTZ-89, tiny-string-28/33, 82-84, x86/GFNI/VBMI2/
  VPCLMUL, SVE/SME, runtime-feature-detect-in-hot-loop. This is the CH3-defensive posture the
  pass asks for. ACCEPT.

- **All non-candidate / orphan-blocked entries (CP-NONE, CP-BLOCKED, C-B3, C5, C6, G4, G5,
  CF-0, CF-4a, CF-4b for the CH3 axis).** The FNV leaf (CP-NONE/G5/CF-0) is a non-candidate
  that RETIRES with the fact-stream; all artefacts pre-emptively REJECT any FNV/hex kernel
  proposal (`p2a:380-385`, `p2e:220-229`, `p2f:289-301,359-361`, `p2d:469-473`, `p2b:262-264`).
  The udot/i8mm orphans (C5/C6/G4/CF-4a/CF-4b/C-B3) are orphan-blocked on the no-antecedent
  axis (CH1/CH4), gated behind a future typed-path re-profile; they do not re-open a named
  REDRESS *route* — `udot` and `usmmla` are host instructions bound to a re-admission gate,
  not blocked REDRESS routes. asmjson collapsed-stage is x86-host-blocked, correctly excluded
  (`p2a:150-166`). ACCEPT for CH3 (these are CH1/CH4 surfaces; flagged §5 as hand-offs).

### REVISE (touches a pre-block; framing imprecise or under-bounded — fix, do not drop)

- **None.** Every V1 REVISE (R1/R2/R3) is confirmed folded into the V2 text (§2); no NEW
  framing imprecision was introduced. The fresh scan (§4) found no V2 candidate that touches a
  pre-block with an imprecise or under-bounded framing.

### REJECT (re-opens a pre-block without fresh P1 evidence + new framing)

- **None.** No V2 candidate re-opens AZ-IV eager, StructRegistry indirection,
  fact-stream-as-output, the 24-row evidence broadcast, FNV/fixture, or any historical blocked
  instruction route (28+33, 50-55, 60-72, 80, 82-84, 88, 89, the historical NEON/UTF8 blocked
  routes) without both fresh P1 evidence and a new framing. The eager/registry/fact-stream/
  broadcast/FNV pre-blocks are each affirmatively counter-designed and named; the
  instruction-route pre-blocks (88/89/82-84/28+33) are flagged with refutations and bounded
  away from in every §4.

## §4 — Fresh-scan re-open tests (no NEW V2 re-open)

- **PMULL/`vmull_p64` on a hot body (REDRESS-88).** All occurrences are negative/reject
  framing: `p2e:287` ("NOT PMULL. No PMULL text is introduced"), `p2c:48` (inventory),
  `p2c:339` ("must NOT promote PMULL into the hot body"), `p2c:454,467` (REDRESS-88 source +
  ISA inventory). No candidate proposes PMULL as a prefix-XOR/string-region hot body.
- **Retained cursor / sidecar / `StructLayout`/`TapeStructBuilder`/`TapeCursor`/`UnionTape`
  (REDRESS 60-72 / Lock 1).** All occurrences are reject-test framing: `p2a:418-422` ("CP-A2/A3
  re-open this if they introduce a `StructLayout` …"), `p2d:165-167` ("NO SK-V17 substrate
  candidate may introduce …"), `p2d:414-423` (D6 reject-on-sight). No candidate proposes one.
- **Eager `Box::new` per leaf (AZ-IV).** Guarded by the `write_count==0`/`allocation_count`
  invariant in p2a/p2d/p2f; no candidate materialises eagerly.
- **CTZ bulk-default body (REDRESS-89).** Bounded in C4 (fold-only), G2 (scalar-balance
  default, CTZ consumer-only + parity-gated), cross-linked p2c §3.2 ↔ p2e §4. No candidate
  ships CTZ as the unconditional body.
- **FNV/hex kernel (FNV quarantine).** CP-NONE/G5/CF-0 record it as a retired non-candidate;
  all artefacts pre-REJECT any FNV/hex kernel proposal. No candidate.
- **x86/AVX/GFNI/VBMI2/VPCLMUL/SVE/SME.** Inventoried as host-blocked in p2c §3 flags 5+6 and
  p2f §4.7; no candidate proposes an x86/SVE path. asmjson is the explicit host-blocked
  non-candidate (p2a §1.4).
- **C6 i8mm `usmmla` (NEW V2 header).** Sharpened to "GATED CONTINGENCY, NOT an active
  candidate (twins CF-4b)" (`p2c:257-277`). `usmmla` is grep-clean-absent + net-new; it is not
  a NAMED blocked REDRESS *route*, so it does not re-open one. It is a CH1 antecedent surface
  (no P1 leaf) — hand-off below.

## §5 — Cross-lens hand-offs (not CH3 surfaces)

- **CH1 (antecedent):** C5/C6/G4/CF-4a/CF-4b/C-B3 (udot + i8mm) have NO benched CSS P1
  antecedent — orphan-blocked, gated behind a post-W1/W2 typed-path re-profile. CH3 does not
  REJECT these (they re-open no named REDRESS route); CH1 owns the speculative-kernel
  disposition. D3 (`p2d:305-322`) and CF-3 (`p2f:211-230`) both carry post-CF-1 re-profile
  obligations as their antecedent — CH1 must hold them to it (the LOCKED 28.87%+2.45%
  recognition-control figures are not a measured speculative-rollback antecedent).
- **CH4 (cost):** G1/G2 are NET-NEW kernels requiring net-new scalar twins + new checkasm
  differentials (ABSENT today, `p2e:155` "ABSENT — must be authored"); G4 needs a
  `checkasm_digit_mac` (ABSENT, verified `ls tests/ | grep digit` empty, `p2e:207`). CH4 owns
  the scalar-ref/checkasm/same-wave-consumer triple. CH3 notes the kernels are new (not
  re-opens) and defers the cost gate to CH4.
- **CH5 (hidden coupling):** D6 is the explicit substrate-union anchor; G3's index==tape
  identity (§3) is the Lock-1 surface CH5 owns. CH3's REJECT-test for a retained sidecar
  overlaps CH5's; both should agree the substrate union holds.

## §6 — Counts

- Sections dispositioned: **36** (21 active candidates + 15 non-candidate/gated/anchor).
- **ACCEPT: 36.** **REVISE: 0.** **REJECT: 0.**
- **ACCEPT rate (all 36 sections): 36/36 = 100%.**
- **ACCEPT rate (21 active candidates): 21/21 = 100%.**

All three V1 REVISE dispositions (R1 p2c broadcast citation; R2 p2e G2 §2 CTZ bound
placement; R3 p2e/p2c CTZ cross-artefact consistency) are confirmed folded into the V2 text;
all four V1 §3 coupling conditions (G3 index==tape identity, D5 `BackendRule` branch-tag,
CF-1 routing derived-from-grammar, G2 scalar-balance default) are self-bound in the V2
artefacts. No orphan REVISE. No NEW pre-block re-open introduced in V2.

CH3 verdict: the S-P2 V2 pool re-opens no `skinny/REDRESS.md` pre-block route. CH3 is
clean — nothing to fold into V3 from this lens. The §3/§5 coupling notes (G3 index==tape
identity verbatim, D5/CF-1 routing-derived-from-grammar, G2 scalar-default) carry forward as
binding S-P3 shortlist conditions, not as artefact edits.
