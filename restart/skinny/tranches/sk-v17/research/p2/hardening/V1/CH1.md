# SK-V17 S-P2 RESEARCH — CHALLENGE CH1 CORRECTNESS (V1)

Lens: CH1 CORRECTNESS. Pass: S-P2 Research. Cycle: V1. Date: 2026-05-29.
Authority: `restart/prompts/skinny/PASS-2-RESEARCH.md` §3 (CH1) + ORCHESTRATOR §3W/§3Z.
Subject: `restart/skinny/tranches/sk-v17/research/p2/{p2a-sota-teardown, p2b-dav1d-process,
p2c-arch-esoterica, p2d-substrate-tape, p2e-parse-that-gaps, p2f-grammar-neutral}.md`.
Input ground truth (LOCKED): `research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md §3`
(commit `0ae1caa52`; P1 baseline HEAD `6496fecae`).
CH1 charter: (a) every candidate primitive traces to a NAMED S-P1 hot leaf — else REJECT
as a speculative kernel; (b) SOTA-comparator claims cited to the correct source AND the
correct strictness plane; (c) ISA claims cited to the architecture reference manual.

## §0 — Verification method (orchestrator-citable)

Every load-bearing bbnf `file:line` claim was re-resolved against source this cycle, not
inherited. Confirmed at source:

- **Hot leaf, exact.** `find_component_delim` (`css_l4_declaration_values/generated.rs:288`)
  is the `while pos < self.bytes.len()` loop with `delimiters.contains(&byte)` membership
  leaf + per-byte `match` dispatch (string/comment/balanced-bracket skip arms);
  `consume_balanced_at` (`:320`) is byte-for-byte the SAME inner loop differing only in the
  membership test (`byte == close` vs `.contains`). Confirms the "ONE byte-class scan target"
  framing every artefact carries.
- **Tape substrate, exact.** `push_plain_offset` (`assembler.rs:71`) is one bounds-checked
  branchless `ptr.write(checked_u32(offset)) + set_len` with `#[cold] reserve_offsets_cold`
  off the hot path; `OffsetFlags::GRAMMAR_BIT0/BIT1` exist (`mod.rs:22-23`); `patch_flags`
  (`assembler.rs:93`) is the cursor-ordered sparse side-table append; `ValueRef<…,G:
  EventGrammar = AnyGrammar>` (`mod.rs:175`) IS generic over grammar G (P2-F §1.1
  load-bearing claim is true).
- **Dispatch + neutrality vehicle, exact.** `select_classifier(alphabet: &'static [u8;64])`
  (`dispatch.rs:42`); `PrimitiveKernels` fn-table; `lo6_table_admissible` (`dispatch.rs:101`)
  uses `(byte & 0x3f)` slot-distinctness. `byte_class_from_table_64_neon`
  (`aarch64/byte_class_from_table_64.rs:2`) IS a scalar passthrough today (P2-C/P2-F finding
  is true).
- **The C1 lo6-collision finding, independently recomputed.** `;`=0x3b, `{`=0x7b; `0x3b &
  0x3f = 0x3b` and `0x7b & 0x3f = 0x3b` — they COLLIDE. So the JSON `classify_tbl4` lo6
  backend is structurally inadmissible for every CSS delimiter set containing `;{`; the
  `lo6_table_admissible` guard correctly forces scalar/eq-set. P2-C C1→C2 and P2-F §1.2 are
  CORRECT (this is the single strongest correctness contribution of the cycle).
- **JSON antecedents.** `json/value.rs:143` `value_from_ref` (lazy `JsonNodeKind::at_cursor`
  projection); `json/scan.rs` `scan` (NEON `classify_structural_terminator_block_from_table`
  + `prefix_xor_64`/`escape_mask_64` string-skip suppression). Both present and as described.
- **Orphan / non-candidate.** `digit_mac.rs:27` `parse_4_digits_dotprod` (udot asm + scalar
  fallback) present; `tests/ | grep digit` = EMPTY (G4 checkasm gap confirmed). i8mm
  grep-clean-absent confirmed (zero `usmmla`/`ummla`/`is_aarch64_feature_detected!("i8mm")`
  in `bbnf-simd/src/`). `W5C_REQUEST_FACT_PROFILES` const present (`codegen/src/lib.rs:336`,
  the Lock-14 overfit re-entry seam).
- **Comparator strictness planes.** `css_canon_bench.rs:250` `assert!(n>=50)`;
  `CssparserFullParseProbe` (`:294`) `RuleBodyItemParser` returns `()` — materializes nothing
  → flaw-probe plane, correctly classed. lightningcss = materializing full-CSSOM (locked
  profile §3.3 ~30% typed-node build+drop) = the fair >SOTA bar.
- **REDRESS items.** Item 88 (PMULL prefix-XOR rejected) + Item 89 (CTZ bulk consumer
  rejected), incl. the `:2595` "PMULL … remains rejected by Item 88, and the CSSC CTZ/bulk
  consumer …" line P2-C cites. `ext/x86/{x86inc,x86util,bbnf}.asm` + `LICENSE-VENDOR`
  (Layer-0 vendored, P2-E §1.0).

## §1 — Candidate-to-hot-leaf traceability ledger (the CH1 core test)

Every §2 candidate across all six artefacts, with its named S-P1 antecedent and the CH1
verdict. The S-P1 named hot-leaf pool (LOCKED §3.3): `find_component_delim` 59.24% (scan) +
`consume_balanced_at` 10.31% (folds to ONE scan target, ~69%); `emit_fact_stream` 25.01% +
~64% alloc floor (string/tape); `push_ascii_lower_hex` 8.98% (FNV diagnostic — explicitly
NO primitive); the orphan `parse_4_digits_dotprod` (zero CSS antecedent).

| Artefact / candidate | Named P1 antecedent | Traces? | CH1 |
|---|---|---|---|
| P2-A CP-A1 byte-class classifier | `find_component_delim`+`consume_balanced_at` ~69% | YES | ACCEPT |
| P2-A CP-A2 `push_plain_offset` sink | `emit_fact_stream` 25% + ~64% floor | YES | ACCEPT |
| P2-A CP-A3 lazy `ValueRef` rider | the String floor (replaces `emit_fact_stream`) | YES (indirect, named) | ACCEPT |
| P2-A CP-A4 tokenize-once reuse | `find_component_delim`/`find_colon_before`/`parse_declaration` 2–3× re-walk | YES | ACCEPT |
| P2-A non-cands (FNV, digit, asmjson) | recorded as non-candidate / orphan / host-blocked | N/A | ACCEPT (correctly excluded) |
| P2-B C-B1 `byte_class_from_eq_set_64` | `find_component_delim`+`consume_balanced_at` ~69% | YES | ACCEPT |
| P2-B C-B2 `push_plain_offset` | `emit_fact_stream` 25% + ~64% floor | YES | ACCEPT |
| P2-B C-B3 udot digit kernel | NONE — process-REJECTED (G1/G2/G4 fail) | correctly NONE | ACCEPT (rejection sound) |
| P2-B C-B0 admission process | the deliverable (process, not a kernel) | N/A | ACCEPT |
| P2-C C1 lo6 TBL classify | ~69% scan leaf; INADMISSIBLE (lo6 collision) → C2 | YES | ACCEPT |
| P2-C C2 eq-set fan classify | ~69% scan leaf (admissible route) | YES | ACCEPT |
| P2-C C3 shrn movemask | mask-extract on the scan leaf (sub-task) | YES (folded) | ACCEPT |
| P2-C C4 host CTZ first-match | index-extract on the scan leaf (fold-only) | YES (folded) | ACCEPT |
| P2-C C5 UDOT digit | NONE — orphan, re-admission-gated | correctly NONE | ACCEPT (orphan-flagged) |
| P2-C C6 i8mm batch | NONE — net-new, doubly orphan-blocked | correctly NONE | ACCEPT (orphan-flagged) |
| P2-D D1 `push_plain_offset` emit | `emit_fact_stream` 24.59% + 57.63% floor | YES | ACCEPT |
| P2-D D2 lazy `ValueRef` projection | the typed-materialisation cost (`emit_fact_stream`) | YES | ACCEPT |
| P2-D D3 O(1) checkpoint/truncate | recognition control loop 28.87%+2.45% | YES (weak, see §2-R3) | REVISE |
| P2-D D4 one-shot SIMD reserve | the 57.63% floor (grow churn), gated behind D1/D2+scan | YES | ACCEPT |
| P2-D D5 sparse-flag side-table | mechanism for D2's cheap kind-disambiguation | YES (indirect, guarded) | ACCEPT |
| P2-D D6 second substrate | NONE — REJECT-on-sight (Lock 1 anchor) | correctly NONE | ACCEPT |
| P2-E G1 `comment_body_mask_64` | comment-skip arm of the ~69% scan leaf | YES | ACCEPT |
| P2-E G2 `bracket_depth_mask_64` | `consume_balanced_at` 11.05% recursion | YES | ACCEPT |
| P2-E G3 `scan_components_to_index` | whole ~69% scan (same-wave consumer of G1/G2) | YES | ACCEPT |
| P2-E G4 digit checkasm gate | NONE — gated behind W1/W2 typed re-profile | correctly NONE | ACCEPT (gated) |
| P2-E G5 FNV/hex | non-candidate (8.98% diagnostic, retires) | N/A | ACCEPT |
| P2-F CF-1 tape-append + projection | `emit_fact_stream` 24.59% + ~57.63% floor | YES | ACCEPT (conditional, §1.4 own) |
| P2-F CF-2 byte-class classifier | ~68% scan leaf | YES | ACCEPT |
| P2-F CF-3 commit-by-construction Alt | ~31% inlined recognition control (WEAK/post-CF-1) | YES (self-flagged weak) | REVISE |
| P2-F CF-4a udot wire | NONE — orphan, gated | correctly NONE | ACCEPT (orphan-flagged) |
| P2-F CF-4b i8mm net-new | NONE — REJECT on current evidence, hard-gated | correctly NONE | ACCEPT (orphan-flagged) |
| P2-F CF-0 negative space | proves NOT-needed (unicode/dispatch/FNV) | N/A | ACCEPT |

**Result:** No candidate is a speculative kernel admitted WITHOUT a P1 antecedent. The four
orphans (B-B3/C5/C6, G4, CF-4a/CF-4b — all one underlying udot/i8mm digit family) are each
explicitly marked NONE-antecedent and gated behind a future typed-path re-profile, never
proposed as active — exactly the CH1-correct disposition. The single CH1 weakness is the
speculative-rollback candidate (P2-D D3 / P2-F CF-3): its antecedent is the inlined
recognition control loop, which the locked profile classes as recognition control, NOT as
measured speculative checkpoint/rollback self-time — both artefacts self-flag the gap, so it
is a REVISE (tighten the antecedent claim), not a REJECT.

## §2 — Dispositions (path:line + concrete fix)

### REVISE

**R1 — P2-C §2 C1 / P2-D §1.4 / P2-F §1.2 §3 §5: "mod 0x3f" is imprecise diction for a
`& 0x3f` low-6-bit MASK.** (`p2c:154,288,381`; `p2d:136,276`; `p2f:52,59,191,295,329,392`.)
The source guard `lo6_table_admissible` (`dispatch.rs:101`, verified) computes
`(byte & 0x3f)` — the low 6 bits, NOT `byte % 0x3f` (modulo by 63). For the load-bearing
`;`/`{` collision the two happen to coincide (`0x7b & 0x3f = 0x3b`; `0x7b % 0x3f = 0x3c` —
they would NOT coincide under true modulo). The CONCLUSION (collision, lo6 inadmissible for
CSS) is independently re-verified CORRECT, so this does not flip any verdict; but the prose
"distinct mod 0x3f" / "→ slot 59" mislabels the operation. **Fix:** replace "mod 0x3f" with
"low-6-bit (`& 0x3f`)" throughout; the slot computation and collision claim stand. CH1
REVISE (cosmetic-correctness; no candidate disposition changes).

**R2 — `json/scan.rs:219` line cite names the right symbol but is ±2 lines off the symbol
definition.** (`p2a:7,52,233,242,419`; `p2b:135`; `p2c:114,415`; `p2d` cites the fn body
correctly via `:22-30`; `p2e:320`.) The verified `scan` fn opens at `json/scan.rs:207`; the
`classify_structural_terminator_block_from_table` call the artefacts attribute to `:219` is
at `:217-218`; `prefix_xor_64`/`escape_mask_64` are `:237-239`. The cited symbol is correct
and the antecedent is real; only the bare `:219` anchor is imprecise. **Fix:** cite
`json/scan.rs:207` (`fn scan`) / `:217` (classify call) / `:239` (prefix_xor). Note P2-D
already cites the correct `:22-30`/`:32` range — adopt that precision pass-wide. CH1 REVISE
(provenance precision; antecedent itself is sound).

**R3 — P2-D §2 D3 / P2-F §2 CF-3: the speculative-checkpoint antecedent overstates what the
LOCKED profile measured.** (`p2d:247-253`; `p2f:206-217`.) The locked §3.3 attributes the
recognition plane to ~69% scan + 28.87%+2.45% `parse_stylesheet`/`parse_block`/
`parse_block_item` classed as "structural (recognition control loop)" — NOT as measured
speculative checkpoint/rollback self-time. P2-F CF-3 honestly self-flags this ("WEAK on the
benched skinny path … the ~31% … is a core-tree number … NOT re-confirmed here … carries a
hard S-P1-re-confirm obligation"); P2-D D3 is weaker — it asserts the control loop "carries
the speculative checkpoint/rollback machinery" as if measured. **Fix:** P2-D D3 must adopt
P2-F CF-3's framing verbatim — the antecedent is the inlined recognition control loop
re-profiled on the typed tape path AFTER D1/CF-1 lands (a plane P1 could not measure), with
an explicit S-P1-re-confirm obligation; it must NOT claim a measured speculative-rollback hot
leaf. With that re-framing the candidate traces (control loop is a named leaf) and survives;
without it, it borders on a speculative own-compute hypothesis (CH1). REVISE both to the
re-confirm framing.

**R4 — P2-A §1.0 / P2-A §1.5 carry "2.0–3.6×" and N=100 lightningcss medians (667–1015
Mbps) that are NOT the LOCKED V4 figures.** (`p2a:20,170`.) The LOCKED profile §3.1 (the
authority CH1 binds against) reports V4 within-harness full÷lcss of 2.01–3.09× and
lightningcss 833–1261 Mbps; the "3.6×" upper bound and the 667–1015 Mbps band are the V3/run
spread / P1-E N=100 snapshot, not the converged V4 lock. The numbers are real (the §3.1 run
table shows tailwind 3.50/3.00/3.09 across the three runs, so 3.6× is from an earlier
snapshot) but mixing snapshot bands into a V1 S-P2 artefact that elsewhere cites the V4 lock
is an internal-consistency slip. **Fix:** quote the LOCKED V4 band (2.01–3.09× BEATS;
lightningcss 833–1261 Mbps) as the load-bearing figure, and if the run-spread is cited, label
it explicitly as the 3-run stability band per §3.1, not as the headline ratio. CH1 REVISE
(comparator-figure provenance; the >SOTA-recognition direction is unaffected and correct).

### ACCEPT (load-bearing, called out)

- **P2-C C1→C2 lo6-collision pivot is CH1-exemplary.** Independently recomputed (`;`/`{`
  collide on low-6-bit). It correctly demotes the "reuse JSON `classify_tbl4` for CSS" route
  to scalar-fallback-only and elevates the eq-set fan (`byte_class_from_eq_set_64`, a genuine
  NEON body verified at `aarch64/byte_class_from_eq_set_64.rs:33`) as the admissible CSS scan
  route. P2-F §1.2 reaches the same conclusion. This is a correctness finding the SOTA
  teardown (P2-A CP-A1, which still names `byte_class_index_64`/`vqtbl4q_u8` lo6 cascade as
  the shape) does NOT carry — see N1.
- **Orphan discipline (udot/i8mm) is uniformly correct.** Every one of B-B3/C5/C6/G4/CF-4a/
  CF-4b names "NONE" antecedent, cites the LOCKED orphan-blocked clause (§3.3) + the
  profile-first re-admission gate (re-profile typed `ValueRef` path AFTER W1/W2), and is
  flagged inventory-only/gated-contingency, never proposed. CH1's "speculative kernel →
  REJECT" rule is satisfied because none is proposed as active.
- **SOTA citations correctly source-and-plane keyed.** simdjson (Stage-1/On-Demand, arXiv
  1902.08318 + `parse_many.md`/`basics.md` @ `79bbba3e`), sonic-rs (`README.md:60-66/78-90` @
  `03545a95`, with the SK-V6 `utf8_lossy` permissive caveat carried), yyjson (`README.md:10-18`
  @ `d6085270`, used as the refutation anchor that scalar-can-be-SOTA, supporting tape-first
  lever order), asmjson (correctly host-blocked, x86 AVX-512 only). lightningcss = materializing
  full-CSSOM (fair >SOTA bar); cssparser = token-scan flaw probe (parity oracle, NOT a SOTA
  bar) — both planes correctly classed per §8.1. No SOTA-beat claim rests on a permissive
  comparator.
- **ISA citations correctly manual-keyed.** Arm ARM (DDI 0487) for TBL/TBX (`vqtbl4q_u8`),
  SHRN/SHRN2 (`vshrn_n_u16` movemask), UDOT/SDOT (FEAT_DotProd), USMMLA/UMMLA (FEAT_I8MM),
  RBIT+CLZ / FEAT_CSSC CTZ, PMULL (FEAT_AES). x86 secondary (SDM Vol.2: VPSHUFB, GF2P8AFFINEQB,
  VPCOMPRESSB, VPCLMULQDQ) inventoried as out-of-scope. All routes correctly aarch64-only.
- **REDRESS pre-block citations verified.** Items 88/89 exist with the stated PMULL/CTZ
  refutations; the SVE/x86 host-block and the FNV-diagnostic exclusion are correctly carried.

## §3 — Notes (no disposition; for the consolidation / sister lenses)

- **N1 (cross-artefact inconsistency — flag for the aggregator, CH2-adjacent).** P2-A CP-A1
  describes the byte-class classifier as `byte_class_index_64` via `vqtbl4q_u8` lo6-table
  movemask-cascade (`p2a:217-242`), while P2-C C1/P2-F §1.2 prove that exact lo6-TBL route is
  INADMISSIBLE for CSS (the `;`/`{` collision) and the admissible route is the eq-set fan. The
  underlying candidate is the same hot leaf with a valid scalar reference, so CH1 ACCEPTs both;
  but the SHAPE descriptions diverge. The V2 fold should reconcile P2-A CP-A1's shape to name
  the eq-set/256-table primitive (or `lo6_table_admissible`-gated fallback) so S-P3 does not
  shortlist a route P2-C proved scalar-falls-back. Not a CH1 REJECT (antecedent + scalar ref
  are real), but a consistency item the aggregator should route to V2.
- **N2.** Commit-mismatch in frontmatter: artefacts variously cite master HEAD `0ae1caa52`
  (P2-B/E/F) and `6496fecae` (P1 baseline; P2-A/C/D bracket both). The dispatch names master
  HEAD `0ae1caa52` and P1 baseline `6496fecae`; both SHAs are internally consistent with the
  LOCKED consolidation. No correctness impact; noted for provenance uniformity (CH6-adjacent).
- **N3.** The "~69%" combined scan figure (P2-C/D/E/F) sums 59.24%+10.31% = 69.55% from the
  V4 lock (or 56.52%+11.05%≈67.6% from the P1-E N=100 snapshot); artefacts use "~68%/~69%"
  interchangeably. Both are within the locked range; harmless.

## §4 — Counts + verdict

Disposition unit = §2 candidate primitive (the load-bearing artefact per §2.1) + the §1
findings/sources rows that carry a load-bearing factual claim. Across the six artefacts:

- **Candidate primitives dispositioned:** 31 (CP-A1..A4 + 3 non-cand; C-B0..B3; C1..C6;
  D1..D6; G1..G5; CF-0..CF-4b).
- **ACCEPT:** 29 candidates (incl. all orphan-flagged and non-candidate rows, which are
  CH1-correctly excluded with a named gate).
- **REVISE:** 2 candidates (P2-D D3 + P2-F CF-3 — the speculative-checkpoint antecedent
  overstatement, R3) + 3 cross-cutting findings/provenance REVISEs (R1 mod-0x3f diction, R2
  `scan.rs:219` line precision, R4 P2-A comparator-figure provenance) applied at the
  artefact-finding level.
- **REJECT:** 0. No candidate is a speculative kernel admitted without a P1 antecedent; no
  SOTA-beat rests on a permissive comparator; no ISA claim is uncited.

**Candidate-level ACCEPT rate: 29/31 = 93.5%.** Counting the 5 distinct REVISE items against
the full dispositioned surface (31 candidates + R1/R2/R4 as 3 finding-level items = 34):
**29/34 = 85.3%** ACCEPT if findings are weighted as units. Either way **below the §3Z 95%
bar for V1** — driven entirely by REVISE, ZERO REJECT. All five REVISEs are tractable
(re-frame two antecedents to the re-confirm obligation; one diction fix; one line-cite
precision pass; one comparator-figure provenance fix) and fold cleanly into V2; none is a
candidate that fails grammar-neutrality or re-opens REDRESS (those are CH2/CH3's planes).

**CH1 V1 verdict: REVISE (not converged).** The candidate pool is correctness-sound — every
active candidate traces to a named S-P1 hot leaf, the orphan discipline is exemplary, and the
SOTA/ISA citations are correctly source-and-plane keyed. The five REVISEs are provenance/
framing precision, not substantive defects. Fold R1–R4 into V2; CH1 expects ≥95% on the
re-emission.
