# SK-V17 S-P2 CHALLENGE — CH6 ANTI-PAPER-CLOSE (V2)

Lens: CH6 ANTI-PAPER-CLOSE. Pass: S-P2 Research. Cycle: V2. Date: 2026-05-29.
Subject: `restart/skinny/tranches/sk-v17/research/p2/{p2a..p2f}.md` (all six at Cycle V2).
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

## §0.1 — V1→V2 fold verification (the two prior REVISE items)

The V1 CH6 (`p2/hardening/V1/CH6.md`) returned exactly two REVISE items, both
wording-level with named in-§3 fold targets. Both are confirmed folded in V2:

- **V1-R1 (p2b §2 C-B1 "Defer the full cross-grammar mapping to P2-F").** V2
  `p2b-dav1d-process.md:230-232` now reads: "Verdict here: PASS (verified §3 —
  byte-set membership carries no grammar role); P2-F formalises the full
  cross-grammar set-mapping in-pass." The "Defer … to P2-F" deferral phrasing is
  GONE; the §3 verdict (`p2b:225-232`) stands as the grounding. **FOLDED.**
- **V1-R2 (p2c §2 C6 i8mm framing softer than its p2f twin).** V2
  `p2c-arch-esoterica.md:257,262-267,288` now: (a) the C6 header is "GATED
  CONTINGENCY, NOT an active candidate (twins P2-F CF-4b)"; (b) the body adopts
  p2f-CF-4b's "S-P3 must NOT shortlist it; it admits only if the CF-4b re-profile
  gate fires"; (c) the scalar-ref line now names the reuse-oracle: "The parity
  oracle is **C5's `parse_4_digits` scalar twin** (`digit_mac.rs:15-22`) … no
  second oracle invented." Both prescribed V1 fixes applied. **FOLDED.**

No orphan REVISE carried from V1 (ORCHESTRATOR §3W zero-orphan requirement met).

## §1 — Verification performed (not asserted)

Every load-bearing anchor re-checked against the tree at HEAD `0ae1caa52`, not
taken on the artefact's word:

- **lo6 collision (the load-bearing C1-inadmissible / CF-2 finding) reproduced.**
  `python3`: `0x3b & 0x3f == 0x7b & 0x3f == 59` (`;`/`{` collide); `0x7b % 0x3f
  == 60` (a true modulo would NOT collide — confirming the artefacts' bitmask-not-
  modulo phrasing in p2a:244-248, p2c:155-163, p2d:150-158, p2f:65-68). The guard
  body `dispatch.rs:101-113` confirmed: `let slot = (byte & 0x3f) as usize`. All
  four artefacts' assertion is correct.
- **Scalar-twin ledger is real.** `ls src/scalar/` = 7 twins (bitmap_next_set_bit,
  bitmap_prefix_xor_64, bulk_emit_positions_64, byte_class_from_eq_set_64,
  byte_class_from_table_64, eob_pad_clamp, swar_8byte) + mod.rs — matches p2b §1.2
  "7 scalar references" and p2e §1.0.
- **The eq-set scalar doc-stamp p2b §1.1 cites is verbatim.** `src/scalar/byte_class_from_eq_set_64.rs`
  carries "this is the source-of-truth implementation. The vector bodies are
  strictly correctness-equivalent fan-outs" (the dav1d scalar-oracle-first
  invariant p2b §1.1 quotes at `:17-18`). Present.
- **`push_plain_offset` body matches the prose.** `assembler.rs:71` is the
  branchless `as_mut_ptr().add(len).write(checked_u32(offset)); set_len(len+1)`
  with `reserve_offsets_cold` on the cold path — exactly p2a/CP-A2, p2b/C-B2,
  p2d/D1's description. (Minor: it takes `offset: usize` and stores u32 via
  `checked_u32`; p2d D1 says "usize" correctly, p2a CP-A2 says "single branchless
  u32 offset" which is accurate about the *stored* value. Not a CH6 defect.)
- **`byte_class_from_table_64_neon` is a scalar passthrough.** `aarch64/byte_class_from_table_64.rs:1-4`
  tail-calls `byte_class_from_table_64_scalar` — confirms the load-bearing claim in
  p2a:248-250, p2c:105, p2f:78-79 that the lo6/table NEON impl runs scalar today.
- **Net-new G1/G2 are genuinely net-new and carry verbatim sketches.** `ls src/scalar/`
  has NO `comment`/`bracket` file → G1/G2 require sketches → both deliver verbatim
  Rust fn bodies IN §2 (`p2e:120-129` comment_body_mask_64, `p2e:156-164`
  bracket_depth_mask_64). The exact CH6 net-new requirement.
- **Digit checkasm gap is real.** `ls tests/ | grep -i digit` = EMPTY → G4 (p2e),
  C5 (p2c), CF-4a (p2f) "missing checkasm gate" is true; the udot kernel is
  present-but-orphan, gated, never claimed as researched-and-ready.
- **Comparator SHAs trace to a register.** Every p2a SHA (`79bbba3e…` simdjson,
  `03545a95…` sonic-rs, `d6085270…` yyjson, `4c494864…` cssparser, `ec165294…`
  lightningcss) appears verbatim in `restart/audit/totality/p2/2A-sota-landscape.md:51-58`
  with full GitHub blob URLs + line ranges + the T2A-V1-SOTA-{JSON,CSS}-00N IDs
  p2a cites. Orchestrator-citable to the byte.
- **ISA claims name feature sections.** p2c §5 (`:464-471`) / p2f §5 (`:414-417`)
  cite Arm DDI 0487 with FEAT_DotProd (UDOT/SDOT), FEAT_I8MM (USMMLA/UMMLA),
  FEAT_CSSC (CTZ), FEAT_AES (PMULL), and SDM Vol. 2 for the x86 secondary
  inventory. ISA-section evidence present.

## §2 — Per-candidate / per-section disposition (path:line + concrete fix)

Scope: the §2 candidate enumerations across all six artefacts, the comparator/ISA
§1 teardown sections (CH6's evidence-binding burden), and the non-candidate records
(CH6 must confirm a recorded leaf is *retired/dropped*, not paper-closed).

### p2a-sota-teardown.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.0–1.7 comparator teardown | p2a:12-218 | **ACCEPT** | Every comparator claim carries a pinned SHA + the totality 2A register row (verified `2A-sota-landscape.md:51-58`); strictness plane named per path (§1.5 lightningcss = fair bar, §1.6 cssparser = flaw-probe, §1.2 sonic-rs `utf8_lossy` carried as permissive-plane caveat). |
| CP-A1 byte-class classifier | p2a:228-284 | **ACCEPT** | Scalar-ref EXISTS in §2 (eq-set scalar verified `:26`); SOTA antecedent = simdjson Stage 1 (`parse_many.md:54-57` SHA-pinned); ISA = `vqtbl4q_u8`/`vceqq_u8` ARM TBL/compare family; the lo6-inadmissible-for-CSS demotion is grounded against the verified `& 0x3f` guard. |
| CP-A2 tape-append sink | p2a:286-314 | **ACCEPT** | Not a SIMD kernel; §2 names correctness oracle (cssparser 8-field equality) + `assembler.rs:71` verified; SOTA antecedent simdjson DOM / yyjson DOM SHA-pinned. |
| CP-A3 lazy ValueRef rider | p2a:316-348 | **ACCEPT** | Reference impl is the EXISTING `json/value.rs:143`; SOTA antecedent simdjson On-Demand `basics.md:344-350` SHA-pinned. Grounded as isomorphic emission NOW, no future-wave deferral. |
| CP-A4 tokenize-once reuse | p2a:350-376 | **ACCEPT** | Control-flow change; oracle (cssparser equality) + consumed kernel (CP-A1) parity named; antecedent file:line present. |
| Non-cands: FNV / digit / asmjson | p2a:378-395 | **ACCEPT** | Retired/dropped, not paper-closed: FNV vanishes with tape, digit orphan-gated with verbatim re-admission condition, asmjson host-blocked (`ARCHITECTURE.md:1206`). No un-addressed leaf masquerading as researched. |

### p2b-dav1d-process.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.1–1.4 dav1d process | p2b:22-107 | **ACCEPT** | Each of the 3 invariants maps to a verified in-tree construct (`checkasm_common.rs` canary/signal/callee-saved; the `:17-18` source-of-truth doc-stamp verified); dav1d/FFmpeg `checkasm.c` cited by file. |
| C-B1 byte_class_from_eq_set_64 | p2b:115-164 | **ACCEPT** | Scalar-ref PRESENT (verified `:26`); checkasm PRESENT + adversarial (`checkasm_byte_class_from_eq_set_64.rs` verified present); the "JSON-wired interface vs eq-set-leaf-not-live" distinction (`:131-146`) is honest and verified (eq-set is NOT a `SelectedBackend` arm). Strongest-grounded candidate. |
| C-B2 push_plain_offset | p2b:166-186 | **ACCEPT** | Scalar-ref N/A re-framed as fact-parity differential (honest); antecedent + consumer named. |
| C-B3 udot (PROCESS-REJECTED) | p2b:188-202 | **ACCEPT** | Dropped, not paper-closed — fails G1/G2/G4 with verbatim re-admission condition. Exemplary CH6 handling. |
| C-B0 admission process (G1–G6) | p2b:204-221 | **ACCEPT** | The PASS-2 §2 load-bearing deliverable; each gate maps to a verified construct + per-candidate clear/owe verdict. |
| §2 C-B1 grammar-neutral verdict | p2b:225-232 | **ACCEPT** (was V1-REVISE) | V1's "Defer … to P2-F" replaced by "Verdict here: PASS (verified §3 …); P2-F formalises the full cross-grammar set-mapping in-pass." The deferral phrasing is gone; the §3 verdict stands. Prior REVISE folded. |

### p2c-arch-esoterica.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.1–1.3 ISA envelope + body audit | p2c:24-118 | **ACCEPT** | The NEON-body audit table (genuine vs scalar-delegate stubs) is verifiable in-tree (`byte_class_from_table_64.rs:1-4` passthrough confirmed); host envelope cited P1-E:11/14. |
| C1 lo6 TBL (INADMISSIBLE) | p2c:130-165 | **ACCEPT** | `;`/`{` `& 0x3f` slot-59 collision independently reproduced (this lens); honest "falls to C2" disposition, not a deferred fix. |
| C2 eq-set fan (primary route) | p2c:167-199 | **ACCEPT** | Scalar-ref PRESENT (`byte_class_from_eq_set_64_scalar`); checkasm PRESENT; per-64-byte op count concrete; the `vdupq_n_u8`-vs-§0.4-broadcast disambiguation (`:191-194`) pre-empts a CH-confusion. |
| C3 shrn movemask | p2c:201-218 | **ACCEPT** | Honestly framed sub-task (no orphan-kernel risk); ISA `vshrn_n_u16` named. |
| C4 host CTZ extract | p2c:220-234 | **ACCEPT** | Fold-only; scalar-ref/checkasm PRESENT; REDRESS-89 bulk-consumer line drawn. |
| C5 UDOT (ORPHAN) | p2c:236-255 | **ACCEPT** | Scalar-ref PRESENT (`digit_mac.rs:15-22`); checkasm REQUIRED-NEW honestly flagged; dropped on no-antecedent with verbatim re-admission gate. |
| C6 i8mm (GATED CONTINGENCY) | p2c:257-277 | **ACCEPT** (was V1-REVISE) | V1's softer framing folded: header now "GATED CONTINGENCY, NOT an active candidate"; body adopts p2f-CF-4b "S-P3 must NOT shortlist it"; scalar-ref now names the reuse-oracle "C5's `parse_4_digits` scalar twin … no second oracle invented" (`:262-267`). The net-new kernel is dispositioned-dropped with a bound oracle — consistent with the drop, not a paper-close. Prior REVISE folded. |
| §3 C5/C6 "deferred to P2-F CF-4a/CF-4b" | p2c:318,325 | **REVISE** | The §3 grammar-neutral verdicts for C5/C6 say "(deferred to P2-F CF-4a)" / "(deferred to P2-F CF-4b)". This is a cross-artefact handoff to an *in-pass* owner — P2-F delivers the named CF-4a/CF-4b rows (verified `p2f:238-287`, fully grounded) — so the SUBSTANCE is clean (the candidate is dropped/gated NOW in p2c §2 with full grounding; only the cross-grammar *generalisation* verdict is handed to P2-F, who delivers it this same cycle). BUT the literal phrasing "deferred to P2-F" is the exact wording V1-R1 corrected in p2b (which the orchestrator deemed reads-as-deferral). For cross-artefact consistency, apply the identical fix. **Fix:** replace "(deferred to P2-F CF-4a)"→"(grammar-neutral SHAPE per §C5; P2-F CF-4a carries the cross-grammar digit-run verdict in-pass)" and likewise for C6/CF-4b. Severity: low — wording only, named in-pass fold target (p2f CF-4a/CF-4b), zero substance change. |
| §3 REDRESS-blocked routes (1–7) | p2c:330-386 | **ACCEPT** | Each blocked route names its REDRESS item + line + measured refutation (Item 88 `-10.04%/-12.66%/-15.52%`). Strongest evidence-binding in the pass. |

### p2d-substrate-tape.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.1–1.5 substrate interrogation | p2d:30-196 | **ACCEPT** | Every tape member cited to `mod.rs`/`assembler.rs` line; lazy-counter invariant cited `mod.rs:80-88` + alphaC ledger; instr/byte figures to HARDENING-S-P1-V4 §3.1. |
| D1 push_plain_offset emit | p2d:206-239 | **ACCEPT** | Antecedent + per-grammar datum named (`lower/offset_tape.rs`); no deferral. |
| D2 lazy ValueRef projection | p2d:241-279 | **ACCEPT** | Reference = existing `value_from_ref`; Sheets/BBNF-self proof "deferred to SK-V18" is the Lock-14 phrase-#2 witnessed-grammar scoping (a locked posture), NOT a candidate-grounding deferral — the CSS+JSON candidate IS grounded now. CH6-clean. |
| D3 O(1) checkpoint/truncate | p2d:281-329 | **ACCEPT** | Banked SK-V16 mechanism (`8153236e8`); the CONDITIONAL lever-status + the explicit S-P1-re-confirm obligation (`:305-322`) DISCLOSE the missing post-CF-1 measurement rather than claim it — the opposite of a paper-close. The CH1-V1-R3 fold (recognition-control-loop framing, not measured rollback) is incorporated and honest. |
| D4 one-shot SIMD reserve | p2d:331-373 | **ACCEPT** | Scalar-ref PRESENT (consumed `scan_structurals_scalar`); gating caveat (behind D1/D2 + NEON scan) stated honestly. |
| D5 sparse-flag side-table | p2d:375-412 | **ACCEPT** | Mechanism cited (`assembler.rs:93-113`, `flags_at mod.rs:144-150`); Lock-14 guard (flag must be a `BackendRule` branch-tag) explicit, flagged for P2-F in-pass. |
| D6 no-second-substrate (record) | p2d:414-423 | **ACCEPT** | Correctly recorded as REJECT-on-sight for CH5 anchor; not a paper-close. |

### p2e-parse-that-gaps.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.0–1.3 two-layer + decomposition | p2e:22-94 | **ACCEPT** | Layer-0/1 cited (`build.rs:1/46`); ALREADY-PRESENT ledger (§1.2) verified primitive-by-primitive in-tree. |
| G1 comment_body_mask_64 | p2e:102-138 | **ACCEPT** | NET-NEW; scalar-reference sketch present VERBATIM IN §2 (`p2e:120-129` executable Rust, tests `open[0]/open[1]/close[0]/close[1]`, never literal `/`/`*`). `comment` scalar file confirmed absent → net-new → sketch required → delivered. Exemplary. |
| G2 bracket_depth_mask_64 | p2e:140-183 | **ACCEPT** | NET-NEW; verbatim scalar sketch in §2 (`p2e:156-164`); the REDRESS-89 CTZ-consumer bound promoted INLINE to the candidate shape (`:169-177`, the V1 CHALLENGE fold) — bulk-default CTZ blocked, parity-gated consumer admissible. CH6-clean. |
| G3 scan_components_to_index | p2e:185-202 | **ACCEPT** | Composition; "scalar reference" = `scan_structurals_scalar` mirrored, named; same-wave consumer is the ~69% scan; blocked-on-G1+G2 sequencing honest. |
| G4 parse_4_digits checkasm gate | p2e:204-218 | **ACCEPT** | `grep digit tests/` EMPTY independently verified; scalar-ref PRESENT; gated behind typed-path re-profile, not deferred-with-handwave. |
| G5 FNV (NON-candidate) | p2e:220-229 | **ACCEPT** | Retired, not paper-closed. |

### p2f-grammar-neutral.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.1–1.5 neutrality vehicle | p2f:15-130 | **ACCEPT** | The lo6-split finding (§1.2) independently reproduced; `ValueRef<G: EventGrammar>` genericity verified `mod.rs:175`; routing overfit-seam (`W5C_REQUEST_FACT_PROFILES` `codegen/src/lib.rs:336`) named; the witnessed-grammar bound (JSON+CSS, not four-grammar) explicit. |
| CF-1 tape-append + ValueRef | p2f:138-164 | **ACCEPT** | Conditional grammar-neutral, condition spelled out (routing derived-from-grammar, §1.4); no deferral. |
| CF-2 byte_class_index_64 | p2f:166-201 | **ACCEPT** | Admissible-backend caveat (256-table/eq-set NOT lo6) load-bearing + grounded; scalar-ref PRESENT, NEON-gap honestly flagged to P2-E/C. |
| CF-3 commit-by-construction Alt | p2f:203-236 | **ACCEPT** | Antecedent honestly self-flagged as NO-measured-rollback-leaf + a hard blocking post-CF-1 re-profile obligation (`:211-230`); discloses the missing measurement instead of claiming it. The opposite of a paper-close (CH1 owns the antecedent-strength verdict). |
| CF-4a udot wire | p2f:238-265 | **ACCEPT** | Scalar-ref PRESENT; CSS-ORPHAN disposition; gated behind dimension-decode re-profile, not deferred-with-handwave. |
| CF-4b i8mm NET-NEW (GATED) | p2f:267-287 | **ACCEPT** | "REJECT on current evidence; hard-gated", scalar-ref "REQUIRED, NET-NEW" honestly flagged, "S-P3 must NOT shortlist it … only as a gated contingency." Explicitly NOT claimed as designed → CH6 accepts the drop. The cleaner framing that p2c-C6 now mirrors (V1-R2 folded). |
| CF-0 negative space | p2f:289-301 | **ACCEPT** | Forbidden-primitive record; not a paper-close. |

## §3 — Cross-cutting

- **No "future wave will detail" anywhere.** Grep across all six artefacts for
  `future wave|will detail|TBD|to be determined|elaborate later|researched/designed
  later` returns ZERO hits. The only "deferred"-family hits are: (a) the two
  p2c §3 "deferred to P2-F CF-4a/CF-4b" cross-artefact handoffs (the single REVISE
  below — to an IN-PASS owner whose rows exist and are grounded), and (b) the
  Lock-14 phrase-#2 "deferred to SK-V18" Sheets/BBNF-self generality scoping
  (a locked posture, not a candidate-grounding deferral). CH6's central failure
  mode — a candidate grounded only by promise of a later wave — is ABSENT.
- **The two genuinely net-new primitives (G1, G2) both carry verbatim scalar
  sketches in §2** — the exact CH6 net-new requirement. The other net-new route
  (C6 / CF-4b i8mm) is dispositioned REJECT/gated-contingency, explicitly
  inventory-only, with its oracle bound to C5's existing `parse_4_digits` scalar
  twin — so the absent sketch is consistent with the drop, not a paper-close.
- **Both prior V1 REVISE items folded clean** (§0.1); zero orphan REVISE carries
  into V2.
- **The single V2 REVISE (p2c §3 C5/C6 "deferred to P2-F" wording)** is the
  residual of the same wording pattern V1 corrected in p2b — applying the
  identical fix to p2c's §3 verdicts brings the pass to full consistency. It is
  wording-only (substance clean: candidate dropped/gated NOW in p2c §2, the
  generalisation verdict delivered IN-PASS by p2f CF-4a/CF-4b), has a named fold
  target, and does not block convergence.

## §4 — Counts

- Total items dispositioned (candidates + §1/§3 sections + non-candidate records): **49**
  (p2a: 6 + p2b: 6 + p2c: 9 + p2d: 7 + p2e: 6 + p2f: 8, summing the rows above).
- **ACCEPT: 48**
- **REVISE: 1** — p2c:318,325 (C5/C6 §3 grammar-neutral verdicts use "deferred to
  P2-F CF-4a/CF-4b"; replace with in-pass-handoff wording per the §2 fix; named
  fold target = p2f CF-4a/CF-4b, which deliver in-pass).
- **REJECT: 0**
- ACCEPT rate: 48/49 = **97.96%**.

The one REVISE is wording-level, orphan-free (named concrete fold target), and
does not block convergence (≥95% for this cycle; consecutive-cycle criterion is
the consolidator's call against V1's 95.7%). No paper-close was found; every
candidate is grounded by orchestrator-citable evidence (comparator SHA →
`2A-sota-landscape.md` register, ISA → DDI 0487 FEAT_ names, primitive →
extant `src/scalar/` twin or verbatim §2 sketch) NOW, with no candidate resting
on a promised later wave. The pass clears the CH6 bar.

## §5 — Sources (CH6 verification trail)

- `restart/prompts/skinny/PASS-2-RESEARCH.md:133-138` (CH6 definition), §2.1 / §3.
- `restart/audit/totality/p2/2A-sota-landscape.md:51-58` (the comparator SHA register every p2a citation traces to — verified verbatim, full GitHub blob URLs + T2A-V1-SOTA IDs).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/V1/CH6.md` (the prior cycle's two REVISE items, both confirmed folded §0.1).
- Reproduced this lens: `0x3b & 0x3f == 0x7b & 0x3f == 59` (`;`/`{` lo6 collision), `0x7b % 0x3f == 60` (modulo would not collide); guard body `dispatch.rs:101-113` `(byte & 0x3f)` confirmed.
- In-tree anchors verified at HEAD `0ae1caa52`: `skinny/crates/bbnf-simd/src/dispatch.rs:101-113`; `src/scalar/` (7 twins, `comment`/`bracket` ABSENT); `tests/` (checkasm + parity present, `digit` ABSENT); `src/scalar/byte_class_from_eq_set_64.rs` (source-of-truth doc-stamp); `src/aarch64/byte_class_from_table_64.rs:1-4` (scalar passthrough); `crates/runtime/src/tape/assembler.rs:71` (push_plain_offset branchless write).
- Net-new sketch presence: `p2e:120-129` (comment_body_mask_64), `p2e:156-164` (bracket_depth_mask_64) — verbatim §2 Rust bodies.
- ISA section evidence: `p2c:464-471` / `p2f:414-417` — Arm DDI 0487 (FEAT_DotProd, FEAT_I8MM, FEAT_CSSC, FEAT_AES), SDM Vol. 2.
- Host: Apple M5 Max, aarch64-apple-darwin. Master HEAD `0ae1caa52`.
