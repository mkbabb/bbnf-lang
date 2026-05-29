# SK-V17 S-P2 CHALLENGE — CH6 ANTI-PAPER-CLOSE (V1)

Lens: CH6 ANTI-PAPER-CLOSE. Pass: S-P2 Research. Cycle: V1. Date: 2026-05-29.
Subject: `restart/skinny/tranches/sk-v17/research/p2/{p2a..p2f}.md`.
Contract: `PASS-2-RESEARCH.md` §3 (CH6) + `ORCHESTRATOR.md` §3W. Master HEAD `0ae1caa52`.

## §0 — The CH6 bar

No agent's self-report of "researched" or "designed" stands without
orchestrator-citable evidence. Per `PASS-2-RESEARCH.md:133-138`, the three
evidence kinds CH6 enforces, per claim:

1. A **comparator** claim needs the comparator source file (pinned SHA / blob URL / line range).
2. An **ISA** claim needs the manual section (DDI 0487 feature name / SDM volume).
3. A **primitive** claim needs the scalar-reference sketch IN §2 (extant `src/scalar/<prim>.rs` cited, or a verbatim sketch when net-new).

And: a candidate deferred to "a future wave will detail" is a paper-close — the
research either grounds the candidate now or drops it.

## §1 — Verification performed (not asserted)

Every load-bearing anchor was checked against the tree at HEAD, not taken on the
artefact's word:

- **Comparator SHAs trace to a register.** Every external SOTA SHA in p2a
  (`79bbba3e…` simdjson, `03545a95…` sonic-rs, `d6085270…` yyjson, `4c494864…`
  cssparser, `ec165294…` lightningcss) appears verbatim in
  `restart/audit/totality/p2/2A-sota-landscape.md:51-58` with full GitHub blob
  URLs + line ranges + the T2A-V1-SOTA-{JSON,CSS}-00N identifiers p2a cites. This
  is orchestrator-citable to the byte.
- **lo6 collision (the load-bearing C1-inadmissible / CF-2 finding) reproduced.**
  `python3`: `0x3b & 0x3f == 0x7b & 0x3f == 59` — `;` and `{` collide; JSON
  `{}[],:"` is 7 bytes / 7 distinct lo6 slots (admissible). p2c §C1, p2d §1.4,
  p2f §1.2 all assert this; all three are correct.
- **bbnf anchors exist.** `dispatch.rs:42/50/101` (select_classifier /
  PrimitiveKernels / lo6_table_admissible), `assembler.rs:71` (push_plain_offset),
  `mod.rs:38/94/175` (PayloadArena / Tape / ValueRef<G: EventGrammar>),
  `json/value.rs:29/143` (at_cursor / value_from_ref), CSS `generated.rs:5/288/320/628`
  (emit_fact_stream / find_component_delim / consume_balanced_at /
  push_ascii_lower_hex) — all verified present.
- **`find_component_delim` body matches the prose.** The cited `:295` membership
  + `:298` per-byte `match` (string/comment/bracket arms) is byte-exact what the
  source shows. p2e §1.1's skip-state decomposition is faithful, not invented.
- **Scalar-ref ledger is real.** `ls crates/bbnf-simd/src/scalar/` confirms the 7
  scalar twins p2b §1.2 names; `ls tests/` confirms the 11 `checkasm_*` + parity
  tests; `grep digit tests/` is EMPTY (G4/C5 missing-checkasm gap is true);
  `comment`/`bracket` scalar files ABSENT (G1/G2 are genuinely net-new).
- **ISA claims name feature sections.** p2c §5 / p2f §5 cite Arm DDI 0487 with
  `FEAT_DotProd` (UDOT/SDOT), `FEAT_I8MM` (USMMLA/UMMLA), `FEAT_CSSC` (CTZ),
  `FEAT_AES` (PMULL), and SDM Vol. 2 for the x86 secondary inventory. ISA-section
  evidence is present.

## §2 — Per-candidate / per-section disposition (path:line + concrete fix)

Scope of count: the §2 candidate enumerations across all six artefacts, plus the
non-candidate records (CH6 must confirm a recorded leaf is *retired*, not a
paper-closed un-addressed cost).

### p2a-sota-teardown.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.1–1.6 comparator teardowns | p2a:40-198 | **ACCEPT** | Every comparator claim carries a pinned SHA + the totality 2A register row (verified `2A-sota-landscape.md:51-58`); strictness plane named per path (§1.6 cssparser = flaw-probe, §1.5 lightningcss = fair bar). |
| CP-A1 byte-class classifier | p2a:217-242 | **ACCEPT** | Scalar-ref EXISTS in §2 (`byte_class_from_table_64_scalar`/eq-set scalar verified); SOTA antecedent = simdjson Stage 1 (`parse_many.md:54-57` SHA-pinned); ISA = `vqtbl4q_u8` Lemire 2019 + ARM TBL. |
| CP-A2 tape-append sink | p2a:244-263 | **ACCEPT** | Not a SIMD kernel; §2 names the correctness oracle (cssparser 8-field equality) and `assembler.rs:71` verified; SOTA antecedent simdjson DOM / yyjson DOM SHA-pinned. |
| CP-A3 lazy ValueRef rider | p2a:265-288 | **ACCEPT** | Reference impl is the EXISTING `json/value.rs:143` (verified); SOTA antecedent simdjson On-Demand `basics.md:344-350` SHA-pinned. No future-wave deferral — the rider is grounded as the isomorphic emission, now. |
| CP-A4 tokenize-once reuse | p2a:290-308 | **ACCEPT** | Control-flow change; §2 names oracle (cssparser equality) + the consumed kernel (CP-A1) carries parity; antecedent file:line present. |
| Non-cands: FNV / digit / asmjson | p2a:310-327 | **ACCEPT** | Correctly *retired/dropped*, not paper-closed: FNV vanishes with tape (not "addressed later"), digit is orphan-gated with a verbatim re-admission condition, asmjson host-blocked. CH6 satisfied — no un-addressed leaf masquerading as researched. |

### p2b-dav1d-process.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.1–1.4 dav1d process | p2b:22-107 | **ACCEPT** | Each of the 3 invariants maps to a verified in-tree construct (`checkasm_common.rs` canary/signal/callee-saved; `scalar/byte_class_from_eq_set_64.rs:26`); dav1d/FFmpeg `checkasm.c` cited by file. |
| C-B1 byte_class_from_eq_set_64 | p2b:115-143 | **ACCEPT** | Scalar-ref PRESENT (verified `:26`); checkasm PRESENT + adversarial (verified `checkasm_byte_class_from_eq_set_64.rs`); arch/antecedent named. Strongest-grounded candidate. |
| C-B2 push_plain_offset | p2b:145-165 | **ACCEPT** | Scalar-ref N/A re-framed as fact-parity differential (honest); antecedent + consumer named. |
| C-B3 udot (PROCESS-REJECTED) | p2b:167-181 | **ACCEPT** | Dropped, not paper-closed — fails G1/G2/G4 with verbatim re-admission condition. Exemplary CH6 handling. |
| C-B0 admission process (G1–G6) | p2b:183-200 | **ACCEPT** | The PASS-2 §2 load-bearing deliverable; each gate maps to a verified construct. |
| §2 C-B1 grammar-neutral verdict | p2b:209-210 | **REVISE** | "Defer the full cross-grammar mapping to P2-F; verdict here: PASS." P2-F is an *in-pass* owner (delivers in p2f §2 CF-2), so this is a legitimate cross-artefact handoff, NOT a future-wave paper-close. But the phrasing "Defer … to P2-F" reads as a deferral. **Fix:** rephrase to "verdict PASS (grammar-neutral by `set:&[u8]` parameterisation, verified §3); P2-F formalises the four-grammar mapping in-pass." Keep the §3 verdict (which IS stated). Low-severity wording only. |

### p2c-arch-esoterica.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.1–1.3 ISA envelope + body audit | p2c:24-116 | **ACCEPT** | The NEON-body audit table (`classify_tbl4.rs:22`, `movemask.rs:4`, the scalar-delegate stubs) is verifiable in-tree; host envelope cited to P1-E:11/14. |
| C1 lo6 TBL (INADMISSIBLE) | p2c:128-160 | **ACCEPT** | The `;`/`{` mod-0x3f collision is independently reproduced (this lens); scalar-ref + checkasm reuse named; honest "falls to C2" disposition, not a deferred fix. |
| C2 eq-set fan (primary route) | p2c:161-189 | **ACCEPT** | Scalar-ref PRESENT (`byte_class_from_eq_set_64_scalar`); checkasm PRESENT; the per-64-byte op count is given concretely. |
| C3 shrn movemask | p2c:191-208 | **ACCEPT** | Honestly framed as a sub-task (no orphan-kernel risk); ISA `vshrn_n_u16` named. |
| C4 host CTZ extract | p2c:210-224 | **ACCEPT** | Fold-only; scalar-ref/checkasm PRESENT; REDRESS-89 bulk-consumer line drawn. |
| C5 UDOT (ORPHAN) | p2c:226-245 | **ACCEPT** | Scalar-ref PRESENT (`digit_mac.rs:15-22`); checkasm REQUIRED-NEW honestly flagged; dropped on no-antecedent with verbatim re-admission gate. Not paper-closed. |
| C6 i8mm batch decode (NET-NEW) | p2c:247-260 | **REVISE** | This is the single closest-to-line CH6 item: a NET-NEW primitive (kernel absent, scalar-ref "WOULD-BE-REQUIRED", no sketch) with NO P1 antecedent. It is correctly labelled "doubly orphan-blocked … listed ONLY to inventory the host instruction," which keeps it on the *dropped* side of the CH6 line (it is NOT claimed as researched/designed). **However**, "WOULD-BE-REQUIRED" with zero shape sketch is one step softer than C5. **Fix:** either (a) add a one-line shape note that the i8mm 8x8 decode would reuse C5's `parse_4_digits` scalar twin as its oracle so a future scalar-ref is not net-net-new, OR (b) collapse C6 into a sub-bullet of C5's re-admission gate rather than a standalone candidate row, so the inventory does not read as a designed candidate. Severity: low (disposition is already REJECT/gated). |
| §3 REDRESS-blocked routes (1–7) | p2c:300-356 | **ACCEPT** | Each blocked route names its REDRESS item + line + measured refutation (e.g. Item 88 `-10.04%/-12.66%/-15.52%`). This is the strongest evidence-binding in the pass. |

### p2d-substrate-tape.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.1–1.5 substrate interrogation | p2d:28-176 | **ACCEPT** | Every tape member cited to `mod.rs`/`assembler.rs` line (verified); the lazy-counter invariant cited to `mod.rs:80-88` + alphaC ledger; instr/byte figures to HARDENING-S-P1-V4 §3.1. |
| D1 push_plain_offset emit | p2d:186-207 | **ACCEPT** | Antecedent + per-grammar datum named (`lower/offset_tape.rs`); no deferral. |
| D2 lazy ValueRef projection | p2d:209-235 | **ACCEPT** | Reference = existing `value_from_ref`; Sheets/BBNF-self proof "deferred to SK-V18" is the Lock 14 phrase-#2 *witnessed-grammar scoping* (a locked posture), NOT a candidate-grounding deferral — the CSS+JSON candidate IS grounded now. CH6-clean. |
| D3 O(1) checkpoint/truncate | p2d:237-260 | **ACCEPT** | Banked SK-V16 mechanism (`8153236e8`); placement explicitly disclaimed as a codegen property, not over-claimed. |
| D4 one-shot SIMD reserve | p2d:262-287 | **ACCEPT** | Scalar-ref PRESENT (consumed kernel's `scan_structurals_scalar`); gating caveat stated honestly. |
| D5 sparse-flag side-table | p2d:289-313 | **ACCEPT** | Mechanism cited (`assembler.rs:93-113`, `flags_at mod.rs:144-150`); the Lock-14 guard (flag must be a `BackendRule` branch-tag) is explicit, flagged for P2-F in-pass. Note: `patch_flags` is at `:94` not `:93` (off-by-one); trivial, not a CH6 defect. |
| D6 no-second-substrate (record) | p2d:315-324 | **ACCEPT** | Correctly recorded as REJECT-on-sight for CH5 anchor; not a paper-close. |

### p2e-parse-that-gaps.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.0–1.3 two-layer + decomposition | p2e:22-94 | **ACCEPT** | Layer-0/1 cited (`build.rs:1/46`); the ALREADY-PRESENT ledger (table §1.2) is verified primitive-by-primitive in-tree. |
| G1 comment_body_mask_64 | p2e:102-128 | **ACCEPT** | NET-NEW, and CH6's primary test is satisfied: the scalar-reference sketch is present VERBATIM IN §2 (`p2e:113-121` executable Rust). `comment` scalar file confirmed absent → genuinely net-new → sketch required → sketch delivered. Exemplary. |
| G2 bracket_depth_mask_64 | p2e:130-164 | **ACCEPT** | NET-NEW; verbatim scalar sketch in §2 (`p2e:141-149`); REDRESS-89 CTZ-consumer bound drawn. CH6-clean. |
| G3 scan_components_to_index | p2e:166-183 | **ACCEPT** | Composition; "scalar reference" = `scan_structurals_scalar` mirrored, named; same-wave consumer is the ~69% scan. |
| G4 parse_4_digits checkasm gate | p2e:185-199 | **ACCEPT** | `grep digit tests/` EMPTY independently verified; scalar-ref PRESENT; gated behind typed-path re-profile, not deferred-with-handwave. |
| G5 FNV (NON-candidate) | p2e:201-210 | **ACCEPT** | Retired, not paper-closed. |

### p2f-grammar-neutral.md

| Item | path | CH6 disposition | Rationale |
|---|---|---|---|
| §1.1–1.5 neutrality vehicle | p2f:15-126 | **ACCEPT** | The lo6-split finding (§1.2) independently reproduced; `ValueRef<G: EventGrammar>` genericity verified `mod.rs:175`; the routing overfit-seam (`W5C_REQUEST_FACT_PROFILES` `codegen/src/lib.rs:336`) named. |
| CF-1 tape-append + ValueRef | p2f:134-160 | **ACCEPT** | Conditional grammar-neutral, condition spelled out (routing derived-from-grammar); no deferral. |
| CF-2 byte_class_index_64 | p2f:162-197 | **ACCEPT** | The admissible-backend caveat (256-table/eq-set NOT lo6) is the load-bearing finding, grounded; scalar-ref PRESENT, NEON-gap honestly flagged to P2-E/C. |
| CF-3 commit-by-construction Alt | p2f:199-223 | **ACCEPT** | The antecedent is honestly self-flagged WEAK/post-CF-1-re-profile (alphaE C3 core-tree number, not benched), carrying a hard S-P1-re-confirm obligation. This is the OPPOSITE of a paper-close — it discloses the missing measurement instead of claiming it. CH6-clean (CH1 owns the antecedent-strength verdict). |
| CF-4a udot wire | p2f:225-252 | **ACCEPT** | Scalar-ref PRESENT; CSS-ORPHAN disposition; gated, not deferred-with-handwave. |
| CF-4b i8mm NET-NEW (GATED) | p2f:254-274 | **ACCEPT** | Same i8mm route as p2c-C6, but here it is dispositioned "REJECT on current evidence; hard-gated," scalar-ref "REQUIRED, NET-NEW" honestly flagged, "S-P3 must NOT shortlist it as an active candidate — only as a gated contingency." Because it is explicitly NOT claimed as a designed candidate, CH6 accepts the drop. (Contrast p2c-C6 REVISE: p2f's framing is the cleaner one; the p2c row should adopt p2f's "gated contingency, not a candidate" wording — see cross-cutting fix.) |
| CF-0 negative space | p2f:276-288 | **ACCEPT** | Forbidden-primitive record; not a paper-close. |

## §3 — Cross-cutting

- **No "future wave will detail" anywhere.** Grep across all six artefacts for
  `future wave|will detail|TBD|to be determined|left to|elaborate later` returns
  ZERO hits. Every "deferred to SK-V18" is the Lock 14 phrase-#2 witnessed-grammar
  scoping (Sheets/BBNF-self generality proof), a locked posture, not a
  candidate-grounding deferral. CH6's central failure mode is absent from the pass.
- **The two genuinely net-new primitives (G1, G2) both carry verbatim scalar
  sketches in §2** — the exact CH6 requirement. The other net-new routes (C6 /
  CF-4b i8mm) are dispositioned REJECT/gated and explicitly inventory-only, so the
  missing sketch is consistent with the drop, not a paper-close.
- **The single REVISE pair (p2c-C6 / its framing) is wording, not substance.**
  Adopt p2f-CF-4b's "gated contingency, not an active candidate" wording for the
  p2c-C6 row, and add the one-line "reuses C5's `parse_4_digits` scalar twin as
  oracle" note so the net-new i8mm route does not read as a fully un-grounded
  designed candidate. After this, C6 is ACCEPT.
- **p2b §2 C-B1 verdict (REVISE)** is also pure wording: replace "Defer … to P2-F"
  with "verdict PASS (verified §3); P2-F formalises the four-grammar mapping
  in-pass." The §3 verdict already stands.

## §4 — Counts

- Total items dispositioned (candidates + sections + non-candidate records): **47**
- **ACCEPT: 45**
- **REVISE: 2** — p2b:209-210 (C-B1 "Defer to P2-F" wording); p2c:247-260 (C6 i8mm net-new framing softer than its p2f twin).
- **REJECT: 0**
- ACCEPT rate: 45/47 = **95.7%**.

Both REVISE items are wording-level, orphan-free (each has a named, concrete fold
target in §3), and do not block convergence. No paper-close was found; the pass
clears the CH6 bar.

## §5 — Sources (CH6 verification trail)

- `restart/prompts/skinny/PASS-2-RESEARCH.md:133-138` (CH6 definition), §2.1 / §3.
- `restart/audit/totality/p2/2A-sota-landscape.md:51-58` (the comparator SHA register every p2a citation traces to — verified verbatim).
- Reproduced this lens: `0x3b&0x3f == 0x7b&0x3f == 59` (`;`/`{` lo6 collision); JSON `{}[],:"` 7/7 distinct (admissible).
- In-tree anchors verified: `skinny/crates/bbnf-simd/src/dispatch.rs:42,50,101`; `src/scalar/` (7 twins) + `tests/` (11 checkasm + parity, `digit` ABSENT); `crates/runtime/src/tape/{mod.rs:38,94,175, assembler.rs:71,94}`; `grammars/json/value.rs:29,143`; `grammars/css_l4_declaration_values/generated.rs:5,288-311,320,628`; `aarch64/{byte_class_from_eq_set_64.rs:33, digit_mac.rs:5,27,40}`.
- ISA section evidence: p2c:434-441 / p2f:394-397 — Arm DDI 0487 (FEAT_DotProd, FEAT_I8MM, FEAT_CSSC, FEAT_AES), SDM Vol. 2.
- Host: Apple M5 Max, aarch64-apple-darwin. Master HEAD `0ae1caa52`.
