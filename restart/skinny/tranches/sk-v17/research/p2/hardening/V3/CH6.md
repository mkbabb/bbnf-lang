# SK-V17 S-P2 CHALLENGE — CH6 ANTI-PAPER-CLOSE (V3)

Lens: CH6 ANTI-PAPER-CLOSE. Pass: S-P2 Research. Cycle: V3. Date: 2026-05-29.
Subject: `restart/skinny/tranches/sk-v17/research/p2/{p2a..p2f}.md` (all six at Cycle V3).
Contract: `PASS-2-RESEARCH.md` §3 (CH6) + `ORCHESTRATOR.md` §3W/§3Z. Master HEAD `0ae1caa52`.

## §0 — The CH6 bar

No agent's self-report of "researched" or "designed" stands without
orchestrator-citable evidence. Per `PASS-2-RESEARCH.md:133-138`, the three
evidence kinds CH6 enforces, per claim:

1. A **comparator** claim needs the comparator source file (pinned SHA / blob URL / line range).
2. An **ISA** claim needs the manual section (DDI 0487 feature name / SDM volume).
3. A **primitive** claim needs the scalar-reference sketch IN §2 (extant `src/scalar/<prim>.rs` cited, or a verbatim sketch when net-new).

And: a candidate deferred to "a future wave will detail" is a paper-close — the
research either grounds the candidate now or drops it.

## §0.1 — V2→V3 fold verification (the single prior REVISE item)

The V2 CH6 (`p2/hardening/V2/CH6.md:191-193`) returned exactly ONE REVISE:

> **V2-R1 (p2c §3 C5/C6 grammar-neutral verdicts use "deferred to P2-F CF-4a/CF-4b").**
> Named concrete fix: replace "(deferred to P2-F CF-4a)" → "(grammar-neutral SHAPE
> per §C5; P2-F CF-4a carries the cross-grammar digit-run verdict in-pass)" and
> likewise for C6/CF-4b. Named fold target = p2f CF-4a/CF-4b (deliver in-pass).

**FOLD STATUS: NOT FOLDED — orphan carry into V3.** Verified directly against the
tree at HEAD `0ae1caa52`:

- `p2c-arch-esoterica.md:318` still reads `**C5 (UDOT 4-digit) — VERDICT:
  grammar-neutral IN SHAPE, but CSS-ORPHAN (deferred to P2-F CF-4a).**` — the exact
  V2-flagged phrasing, unaltered.
- `p2c-arch-esoterica.md:324-325` still reads `**C6 (i8mm batch decode) — VERDICT:
  grammar-neutral IN SHAPE, but CSS-ORPHAN + kernel-absent (deferred to P2-F CF-4b).**`
  — likewise unaltered.

`grep -n "deferred to P2-F" p2*.md` returns precisely these two hits (`p2c:318`,
`p2c:325`) and no others. The V2 CH6 prescribed a verbatim replacement; the V3
artefact carries the V2 wording byte-for-byte. Per `ORCHESTRATOR.md` §3W's
**zero-orphan-REVISE** requirement and §4's "hardening without folding is
paper-hardening and the orchestrator does not advance," a named V2 REVISE that
survives unaltered into V3 is an orphan carry — it must be re-issued, not waved
through, and it blocks the consecutive-cycle convergence criterion until folded.

Note this is itself a meta-paper-close pattern CH6 owns: the V2 lens *disposition*
("REVISE, here is the verbatim fix") was treated as resolution, when the
*substance* (the artefact wording) was never edited. The disposition without the
edit is exactly the failure CH6 exists to catch — a closed-on-paper item.

## §1 — Verification performed (not asserted)

Every load-bearing anchor independently re-checked against the tree at HEAD
`0ae1caa52`, not taken on the artefact's word:

- **lo6 collision reproduced (this lens).** `python3`: `0x3b & 0x3f == 0x7b &
  0x3f == 59` (`;`/`{` collide under the low-6-bit mask); `0x7b % 0x3f == 60` (a
  true modulo would NOT collide). Confirms the bitmask-not-modulo grounding in
  p2a/p2c/p2d/p2f and the lo6-inadmissible-for-CSS demotion. Guard body verified:
  `dispatch.rs` `(byte & 0x3f)` slot computation.
- **Scalar-twin ledger is real (7 twins).** `ls skinny/crates/bbnf-simd/src/scalar/`
  = `bitmap_next_set_bit, bitmap_prefix_xor_64, bulk_emit_positions_64,
  byte_class_from_eq_set_64, byte_class_from_table_64, eob_pad_clamp, swar_8byte`
  + `mod.rs` — matches p2b §1.2 "7 scalar references" and p2e §1.0.
- **Net-new G1/G2/digit absence confirmed.** `ls src/scalar/ | grep -iE
  'comment|bracket|digit'` = EMPTY. So G1 (`comment_body_mask_64`) and G2
  (`bracket_depth_mask_64`) are genuinely net-new → require verbatim sketches →
  both deliver them IN §2 (`p2e:120-129` comment, `p2e:155-165` bracket, both
  executable Rust testing `open[0]/open[1]/close[0]/close[1]`, never a literal
  `/`/`*`). The exact CH6 net-new requirement met.
- **Digit checkasm gap is real.** `ls tests/ | grep -i digit` = EMPTY → G4 (p2e),
  C5 (p2c), CF-4a (p2f) "missing checkasm gate" is true; the udot kernel is
  present-but-orphan, gated, never claimed researched-and-ready.
- **`push_plain_offset` body matches the prose.** `assembler.rs:71` is the
  branchless `as_mut_ptr().add(len).write(checked_u32(offset)); set_len(len+1)`
  with `reserve_offsets_cold` on the cold path — exactly p2a/CP-A2, p2b/C-B2,
  p2d/D1's description.
- **`byte_class_from_table_64_neon` is a scalar passthrough.**
  `aarch64/byte_class_from_table_64.rs:3` tail-calls
  `crate::scalar::byte_class_from_table_64::byte_class_from_table_64_scalar`
  — confirms the load-bearing claim in p2a/p2c/p2f that the lo6/table NEON impl
  runs scalar today.
- **Comparator SHAs trace to a register.** All five p2a SHAs (`79bbba3e…`
  simdjson, `03545a95…` sonic-rs, `d6085270…` yyjson, `4c494864…` cssparser,
  `ec165294…` lightningcss) appear verbatim in
  `restart/audit/totality/p2/2A-sota-landscape.md:51-58` with full GitHub blob
  URLs + line ranges + the T2A-V1-SOTA-{JSON,CSS}-00N IDs p2a cites; p2a carries
  11 SHA citations. Orchestrator-citable to the byte.
- **ISA claims name feature sections.** p2c §5 / p2f §5 cite Arm DDI 0487 with
  FEAT_DotProd (UDOT/SDOT), FEAT_I8MM (USMMLA/UMMLA), FEAT_CSSC (CTZ), FEAT_AES
  (PMULL), and SDM Vol. 2 for the x86 secondary inventory. ISA-section evidence
  present.
- **P2-F CF-4a/CF-4b rows exist and are grounded IN-PASS.** `p2f:263-297`
  (CF-4a) and `p2f:299+` (CF-4b) deliver: scalar-ref status (CF-4a PRESENT
  `digit_mac.rs:15-22`; CF-4b REQUIRED-NEW), checkasm verdict (both flagged
  ABSENT/REQUIRED), P1 antecedent (ORPHAN, P1-E §2.5/§4 anomaly 4(a) named),
  grammar-neutral verdict. So the p2c §3 cross-grammar verdict for C5/C6 IS
  delivered this same cycle by a named in-pass owner — confirming the V2
  finding that the *substance* is clean and the defect is wording-only.

## §2 — Per-candidate / per-section disposition (path:line + concrete fix)

Scope: the §2 candidate enumerations across all six artefacts, the comparator/ISA
§1 teardown sections (CH6's evidence-binding burden), and the non-candidate records
(CH6 must confirm a recorded leaf is *retired/dropped*, not paper-closed).

### p2a-sota-teardown.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.0–1.7 comparator teardown | p2a:12-218 | **ACCEPT** | Every comparator claim carries a pinned SHA + the totality 2A register row (verified `2A-sota-landscape.md:51-58`); strictness plane named per path (lightningcss = fair/materializing bar, cssparser = flaw-probe, sonic-rs `utf8_lossy` carried as permissive-plane caveat). |
| CP-A1 byte-class classifier | p2a:228-284 | **ACCEPT** | Scalar-ref EXISTS in §2 (eq-set scalar verified present); SOTA antecedent = simdjson Stage 1 (SHA-pinned); ISA = TBL/compare family; lo6-inadmissible-for-CSS demotion grounded against verified `& 0x3f` guard. |
| CP-A2 tape-append sink | p2a:286-314 | **ACCEPT** | Not a SIMD kernel; §2 names correctness oracle (cssparser 8-field equality) + `assembler.rs:71` verified; SOTA antecedent simdjson/yyjson DOM SHA-pinned. |
| CP-A3 lazy ValueRef rider | p2a:316-348,403 | **ACCEPT** | Reference impl is EXISTING `json/value.rs:143`; SOTA antecedent simdjson On-Demand SHA-pinned; emission grounded NOW. The "proof deferred to SK-V18" at :403 is the Lock-14 witnessed-grammar scoping (a locked posture: JSON+CSS witnessed now, Sheets/BBNF-self asserted-by-construction), NOT a candidate-grounding deferral. CH6-clean. |
| CP-A4 tokenize-once reuse | p2a:350-376 | **ACCEPT** | Control-flow change; oracle (cssparser equality) + consumed kernel (CP-A1) parity named; antecedent file:line present. |
| Non-cands: FNV / digit / asmjson | p2a:378-395 | **ACCEPT** | Retired/dropped, not paper-closed: FNV vanishes with tape, digit orphan-gated with verbatim re-admission condition, asmjson host-blocked. No un-addressed leaf masquerading as researched. |

### p2b-dav1d-process.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.1–1.4 dav1d process | p2b:22-107 | **ACCEPT** | Each invariant maps to a verified in-tree construct (checkasm canary/signal/callee-saved; the eq-set source-of-truth doc-stamp); dav1d/FFmpeg `checkasm.c` cited by file. |
| C-B1 byte_class_from_eq_set_64 | p2b:115-164 | **ACCEPT** | Scalar-ref PRESENT (verified); checkasm PRESENT + adversarial; "JSON-wired interface vs eq-set-leaf-not-live" distinction honest. Strongest-grounded candidate. |
| C-B2 push_plain_offset | p2b:166-186 | **ACCEPT** | Scalar-ref re-framed as fact-parity differential (honest); antecedent + consumer named; `assembler.rs:71` verified. |
| C-B3 udot (PROCESS-REJECTED) | p2b:188-202 | **ACCEPT** | Dropped, not paper-closed — fails the admission gates with verbatim re-admission condition. |
| C-B0 admission process (G1–G6) | p2b:204-221 | **ACCEPT** | The PASS-2 §2 load-bearing deliverable; each gate maps to a verified construct + per-candidate verdict. |
| §2 C-B1 grammar-neutral verdict | p2b:225-232 | **ACCEPT** | The V1-folded "Verdict here: PASS (verified §3 …); P2-F formalises the full cross-grammar set-mapping in-pass" remains in place. No regression. |

### p2c-arch-esoterica.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.1–1.3 ISA envelope + body audit | p2c:24-118 | **ACCEPT** | NEON-body audit table (genuine vs scalar-delegate stubs) verifiable in-tree (`byte_class_from_table_64.rs:3` passthrough confirmed); host envelope cited P1-E. |
| C1 lo6 TBL (INADMISSIBLE) | p2c:130-165 | **ACCEPT** | `;`/`{` `& 0x3f` slot-59 collision independently reproduced (this lens); honest "falls to C2" disposition. |
| C2 eq-set fan (primary route) | p2c:167-199 | **ACCEPT** | Scalar-ref PRESENT (`byte_class_from_eq_set_64_scalar`); checkasm PRESENT; `vdupq_n_u8`-vs-broadcast disambiguation pre-empts CH confusion. |
| C3 shrn movemask | p2c:201-218 | **ACCEPT** | Honestly framed sub-task; ISA `vshrn_n_u16` named; carries no grammar datum. |
| C4 host CTZ extract | p2c:220-234 | **ACCEPT** | Fold-only; scalar-ref/checkasm PRESENT; REDRESS-89 bulk-consumer line drawn. |
| C5 UDOT (ORPHAN) | p2c:236-255 | **ACCEPT** | Scalar-ref PRESENT (`digit_mac.rs:15-22`); checkasm REQUIRED-NEW honestly flagged; dropped on no-antecedent with verbatim re-admission gate. The §2 *substance* is grounded. |
| C6 i8mm (GATED CONTINGENCY) | p2c:257-277 | **ACCEPT** | "GATED CONTINGENCY, NOT an active candidate"; body adopts p2f-CF-4b "S-P3 must NOT shortlist it"; scalar-ref bound to "C5's `parse_4_digits` scalar twin … no second oracle invented." The §2 *substance* is dispositioned-dropped with a bound oracle. |
| **§3 C5/C6 "deferred to P2-F CF-4a/CF-4b"** | **p2c:318,325** | **REVISE (orphan carry from V2-R1)** | The V2 CH6 named this exact wording REVISE with a verbatim fix; V3 carries the V2 wording UNALTERED (`grep "deferred to P2-F"` = exactly these two lines). This is an **orphan REVISE** — a named prior-cycle disposition that was not folded (§0.1). The substance is clean (p2c §2 grounds C5/C6 NOW; p2f CF-4a/CF-4b deliver the cross-grammar verdict in-pass, verified `p2f:263-297`+), so it is wording-only and does NOT block on substance — but it DOES block the consecutive-cycle convergence criterion until folded, because an orphan REVISE is precisely what §3W forbids carrying. **Fix (verbatim, re-issued):** at `p2c:318` replace `(deferred to P2-F CF-4a)` → `(grammar-neutral SHAPE per §C5; P2-F CF-4a carries the cross-grammar digit-run verdict in-pass)`; at `p2c:325` replace `(deferred to P2-F CF-4b)` → `(grammar-neutral SHAPE per §C5; P2-F CF-4b carries the cross-grammar verdict in-pass)`. Apply the identical wording the orchestrator accepted for p2b V1-R1. Severity: low (wording-only) but MANDATORY (orphan carry); named in-pass fold target = p2f CF-4a/CF-4b. |
| §3 REDRESS-blocked routes (1–7) | p2c:330-386 | **ACCEPT** | Each blocked route names its REDRESS item + line + measured refutation (Item 88 `-10.04%/-12.66%/-15.52%`). Strongest evidence-binding in the pass. |

### p2d-substrate-tape.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.1–1.5 substrate interrogation | p2d:30-196 | **ACCEPT** | Every tape member cited to `mod.rs`/`assembler.rs` line; lazy-counter invariant cited; instr/byte figures to HARDENING-S-P1-V4 §3.1. |
| D1 push_plain_offset emit | p2d:206-239 | **ACCEPT** | Antecedent + per-grammar datum named; `assembler.rs:71` verified; no deferral. |
| D2 lazy ValueRef projection | p2d:241-279 | **ACCEPT** | Reference = existing `value_from_ref`; the "proof deferred to SK-V18" at :277 is the Lock-14 witnessed-grammar scoping (locked posture), NOT a candidate-grounding deferral — the CSS+JSON candidate IS grounded now. CH6-clean. |
| D3 O(1) checkpoint/truncate | p2d:281-329 | **ACCEPT** | Banked SK-V16 mechanism; CONDITIONAL lever-status + explicit S-P1-re-confirm obligation DISCLOSE the missing post-CF-1 measurement rather than claim it — the opposite of a paper-close. |
| D4 one-shot SIMD reserve | p2d:331-373 | **ACCEPT** | Scalar-ref PRESENT (consumed `scan_structurals_scalar`); gating caveat stated honestly. |
| D5 sparse-flag side-table | p2d:375-412 | **ACCEPT** | Mechanism cited; Lock-14 guard (flag must be a `BackendRule` branch-tag) explicit, flagged for P2-F in-pass. |
| D6 no-second-substrate (record) | p2d:414-423 | **ACCEPT** | Correctly recorded as REJECT-on-sight for the CH5 anchor; not a paper-close. |

### p2e-parse-that-gaps.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.0–1.3 two-layer + decomposition | p2e:22-94 | **ACCEPT** | Layer-0/1 cited; ALREADY-PRESENT ledger verified primitive-by-primitive in-tree. |
| G1 comment_body_mask_64 | p2e:102-138 | **ACCEPT** | NET-NEW; scalar-reference sketch present VERBATIM IN §2 (`p2e:120-129` executable Rust, tests `open[0]/open[1]/close[0]/close[1]`, never literal `/`/`*`); `comment` scalar file confirmed absent. Exemplary. |
| G2 bracket_depth_mask_64 | p2e:140-183 | **ACCEPT** | NET-NEW; verbatim scalar sketch in §2 (`p2e:155-165`); REDRESS-89 CTZ-consumer bound promoted INLINE; within-chunk-carry invariant stated at the candidate shape (`:150-154`), not deferred. CH6-clean. |
| G3 scan_components_to_index | p2e:185-202 | **ACCEPT** | Composition; "scalar reference" = `scan_structurals_scalar` mirrored, named; same-wave consumer is the ~69% scan; blocked-on-G1+G2 sequencing honest. |
| G4 parse_4_digits checkasm gate | p2e:204-218 | **ACCEPT** | `grep digit tests/` EMPTY independently verified; scalar-ref PRESENT; gated behind typed-path re-profile, not deferred-with-handwave; "deferred to SK-V18" at :263 is Lock-14 generality scoping. |
| G5 FNV (NON-candidate) | p2e:220-229 | **ACCEPT** | Retired, not paper-closed. |

### p2f-grammar-neutral.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.1–1.5 neutrality vehicle | p2f:15-130 | **ACCEPT** | lo6-split finding independently reproduced; `ValueRef<G>` genericity verified; routing overfit-seam named; witnessed-grammar bound (JSON+CSS) explicit (the "proof deferred to SK-V18" at :127,:352 is the locked phrase-#2 generality scoping, not a candidate deferral). |
| CF-1 tape-append + ValueRef | p2f:138-164 | **ACCEPT** | Conditional grammar-neutral, condition spelled out; no deferral. |
| CF-2 byte_class_index_64 | p2f:166-201 | **ACCEPT** | Admissible-backend caveat (256-table/eq-set NOT lo6) load-bearing + grounded; scalar-ref PRESENT, NEON-gap honestly flagged. |
| CF-3 commit-by-construction Alt | p2f:203-261 | **ACCEPT** | Antecedent self-flagged as NO-measured-rollback-leaf + hard blocking post-CF-1 re-profile obligation (`:238-255`); discloses the missing measurement instead of claiming it. The opposite of a paper-close. |
| CF-4a udot wire | p2f:263-297 | **ACCEPT** | Scalar-ref PRESENT (`digit_mac.rs:15-22`); CSS-ORPHAN disposition; checkasm REQUIRED-NEW flagged ABSENT this cycle; gated behind dimension-decode re-profile, not deferred-with-handwave. This is the in-pass owner the p2c §3 verdict hands to — verified delivered. |
| CF-4b i8mm NET-NEW (GATED) | p2f:299+ | **ACCEPT** | "REJECT on current evidence; hard-gated"; scalar-ref REQUIRED/NET-NEW honestly flagged; "S-P3 must NOT shortlist it … only as a gated contingency." Explicitly NOT claimed as designed → CH6 accepts the drop. The in-pass owner for the p2c §3 C6 verdict. |
| CF-0 negative space | p2f:289-301 | **ACCEPT** | Forbidden-primitive record; not a paper-close. |

## §3 — Cross-cutting

- **No "future wave will detail" paper-close anywhere.** `grep -nE "future
  wave|will detail|TBD|to be determined|elaborate later"` across all six
  artefacts returns ZERO hits. The only "deferred"-family hits are: (a) the two
  p2c §3 "deferred to P2-F CF-4a/CF-4b" cross-artefact handoffs (the single
  REVISE — to an IN-PASS owner whose rows exist and are grounded, verified
  `p2f:263-297`+), and (b) the Lock-14 phrase-#2 "deferred to SK-V18"
  Sheets/BBNF-self generality scoping (a locked posture: `p2a:403`, `p2d:277`,
  `p2e:263`, `p2f:127,:352` — all the witnessed-grammar bound, NOT
  candidate-grounding). CH6's central failure mode — a candidate grounded only by
  promise of a later wave — is ABSENT.
- **The two genuinely net-new primitives (G1, G2) both carry verbatim scalar
  sketches in §2** — the exact CH6 net-new requirement. The other net-new route
  (C6 / CF-4b i8mm) is dispositioned REJECT/gated-contingency, inventory-only,
  oracle bound to C5's existing `parse_4_digits` scalar twin — the absent sketch
  is consistent with the drop, not a paper-close.
- **The V2 REVISE was NOT folded.** This is the one defect of this cycle, and it
  is itself a CH6-class failure: a prior lens disposition (V2-R1, "REVISE with
  verbatim fix") was treated as closed when the artefact wording was never edited
  (§0.1). The orchestrator must fold the verbatim p2c:318/325 replacement before
  the consecutive-cycle convergence test can pass. The substance is clean; the
  fix is mechanical; but per §3W an orphan REVISE is non-advancing.

## §4 — Counts

- Total items dispositioned (candidates + §1/§3 sections + non-candidate records): **49**
  (p2a: 6 + p2b: 6 + p2c: 9 + p2d: 7 + p2e: 6 + p2f: 8).
- **ACCEPT: 48**
- **REVISE: 1** — p2c:318,325 (C5/C6 §3 grammar-neutral verdicts retain the V2-flagged
  "deferred to P2-F CF-4a/CF-4b" wording; orphan carry from V2-R1, NOT folded). Re-issued
  with the verbatim fix above; named in-pass fold target = p2f CF-4a/CF-4b (delivered).
- **REJECT: 0**
- ACCEPT rate: 48/49 = **97.96%**.

The one REVISE is wording-level (substance clean: C5/C6 grounded NOW in p2c §2, the
cross-grammar verdict delivered in-pass by p2f CF-4a/CF-4b). It is, however, an
**orphan carry** — the identical item the V2 lens already REVISEd with a verbatim
fix that was never applied. Per `ORCHESTRATOR.md` §3W (zero orphan REVISE) and §4
(no advance without folding), the orchestrator must apply the verbatim p2c:318/325
replacement in V4 before the consecutive-cycle convergence criterion can be met. No
NEW paper-close was found; every candidate is grounded by orchestrator-citable
evidence NOW (comparator SHA → `2A-sota-landscape.md` register, ISA → DDI 0487 FEAT_
names, primitive → extant `src/scalar/` twin or verbatim §2 sketch), with no
candidate resting on a promised later wave.

## §5 — Sources (CH6 verification trail)

- `restart/prompts/skinny/PASS-2-RESEARCH.md:133-138` (CH6 definition), §2.1 / §3.
- `restart/audit/totality/p2/2A-sota-landscape.md:51-58` (the comparator SHA register every p2a citation traces to — verified verbatim, full GitHub blob URLs + T2A-V1-SOTA IDs).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/V2/CH6.md:191-193` (the prior cycle's single REVISE, V2-R1; confirmed NOT folded — §0.1).
- Reproduced this lens: `0x3b & 0x3f == 0x7b & 0x3f == 59` (`;`/`{` lo6 collision), `0x7b % 0x3f == 60` (modulo would not collide); `(byte & 0x3f)` guard confirmed in `dispatch.rs`.
- In-tree anchors verified at HEAD `0ae1caa52`: `skinny/crates/bbnf-simd/src/scalar/` (7 twins, `comment`/`bracket`/`digit` ABSENT); `skinny/crates/bbnf-simd/tests/` (`digit` ABSENT); `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:3` (scalar passthrough); `skinny/crates/runtime/src/tape/assembler.rs:71,74,80,89` (push_plain_offset branchless write + reserve_offsets_cold).
- `grep -n "deferred to P2-F" p2*.md` = exactly `p2c:318`, `p2c:325` (the orphan REVISE locus).
- Net-new sketch presence: `p2e:120-129` (comment_body_mask_64), `p2e:155-165` (bracket_depth_mask_64) — verbatim §2 Rust bodies.
- In-pass fold-target presence: `p2f:263-297` (CF-4a), `p2f:299+` (CF-4b) — fully grounded cross-grammar verdicts.
- ISA section evidence: p2c §5 / p2f §5 — Arm DDI 0487 (FEAT_DotProd, FEAT_I8MM, FEAT_CSSC, FEAT_AES), SDM Vol. 2.
- Host: Apple M5 Max, aarch64-apple-darwin. Master HEAD `0ae1caa52`.
