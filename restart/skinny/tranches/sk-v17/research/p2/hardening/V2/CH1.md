# SK-V17 S-P2 RESEARCH — CHALLENGE CH1 CORRECTNESS (V2)

Lens: CH1 CORRECTNESS. Pass: S-P2 Research. Cycle: V2. Date: 2026-05-29.
Authority: `restart/prompts/skinny/PASS-2-RESEARCH.md` §3 (CH1) + ORCHESTRATOR §3W/§3Z.
Subject: `restart/skinny/tranches/sk-v17/research/p2/{p2a-sota-teardown, p2b-dav1d-process,
p2c-arch-esoterica, p2d-substrate-tape, p2e-parse-that-gaps, p2f-grammar-neutral}.md` (all
V2, dated 2026-05-29).
Input ground truth (LOCKED): `research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md §3`
(master HEAD `0ae1caa52`; P1 baseline `6496fecae`).
Prior cycle: V1 CH1 returned REVISE (29/31 candidate-level ACCEPT = 93.5%) with five folds
R1–R4 (R3 a candidate pair). This cycle re-resolves every load-bearing claim at source and
audits whether the V1 folds landed.
CH1 charter: (a) every candidate primitive traces to a NAMED S-P1 hot leaf — else REJECT as
a speculative kernel; (b) SOTA-comparator claims cited to the correct source AND the correct
strictness plane; (c) ISA claims cited to the architecture reference manual.

## §0 — Verification method (orchestrator-citable; re-resolved at source this cycle)

Every load-bearing `file:line` was re-resolved against the tree at master HEAD
`0ae1caa5235ee867c5c081f186b6452c51e44a14` (confirmed `git rev-parse HEAD`). Confirmed at
source this cycle:

- **The C1/C2/§1.2 lo6-collision finding — INDEPENDENTLY RE-COMPUTED, EXACT.**
  `lo6_table_admissible` (`bbnf-simd/src/dispatch.rs:101`) computes `let slot = (byte & 0x3f)`
  at `:106` and rejects on `seen[slot]` — a low-6-bit MASK, verified line-for-line. `;`=0x3b
  → `0x3b & 0x3f = 0x3b` (slot 59); `{`=0x7b → `0x7b & 0x3f = 0x3b` (slot 59) — they COLLIDE.
  Under true modulo `0x7b % 0x3f = 0x3c` (slot 60) they would NOT — re-computed in Python this
  cycle. The guard is a mask, the collision is real, the lo6/`classify_tbl4` route is therefore
  structurally inadmissible for any CSS alphabet carrying `;{`. `select_classifier(alphabet:
  &'static [u8;64])` is at `:42`; `SelectedBackend` is `Scalar`/`NeonTbl4` only (no eq-set arm)
  — `select_backend:90` falls to `Scalar` when `lo6_table_admissible` is false. The eq-set fan
  is a SEPARATE kernel, not a dispatch arm.
- **The eq-set NEON body is genuine; the table NEON body is a passthrough.**
  `byte_class_from_eq_set_64_neon` (`aarch64/byte_class_from_eq_set_64.rs:33`) is a real NEON
  fan; its `movemask_u8x16` is at `:79` (the "slow movemask" duplicate P2-C C3 flags). Scalar
  twin at `scalar/byte_class_from_eq_set_64.rs:26`. `lib.rs` exposes `prim::byte_class_from_eq_set_64`
  at `:282` (aarch64 → neon `:289`), consumed by `find_ascii_set_member64:209` at `:216`. By
  contrast `byte_class_from_table_64_neon` (`aarch64/byte_class_from_table_64.rs:2`) tail-calls
  `byte_class_from_table_64_scalar` — a SCALAR PASSTHROUGH. So P2-A's "CSS through the lo6/table
  path would claim a SIMD win it silently runs scalar" risk (§4) is a true statement of source.
- **Hot leaves, exact.** `emit_fact_stream` (`generated.rs:5`), `parse_block_item` (`:209`),
  `parse_declaration` (`:242`), `find_component_delim` (`:288`), `find_colon_before` (`:313`),
  `consume_balanced_at` (`:320`), `consume_comment_at` (`:342`), `consume_string_at` (`:353`),
  `fnv64` (`:619`), `push_ascii_lower_hex` (`:628`), `parse_stylesheet` (`:118`),
  `parse_block` (`:189`) — every cite resolved exactly. (P2-A/E cite `parse_declaration` at
  `:247`; the fn opens `:242` and the `b";}"` membership is in its body — within tolerance, not
  a defect.)
- **Tape substrate, exact.** `TapeBuilder` (`assembler.rs:42`), `push_offset` (`:62`),
  `push_plain_offset` (`:71`), `reserve_offsets_cold` (`:89`), `patch_flags` (`:94`), `finish`
  (`:115`), `CapacityPlan` (`:14`). `Tape` (`mod.rs:94`), `PayloadArena` (`:38`), `write_count`
  (`:81`), `allocation_count` (`:86`), `flags_at` (`:144`), `GRAMMAR_BIT0/BIT1` (`:22-23`),
  `DocumentView` (`:227`). `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` (`:175`)
  IS generic over grammar G — P2-F §1.1 / P2-D D2 load-bearing claim is TRUE (re-verified).
- **JSON antecedents, exact.** `json/scan.rs`: `scan_structurals` (`:22`), `scan_structurals_scalar`
  (`:32`), `scalar_parity_report` (`:38`), `structural_capacity_for` (`:47`), the NEON `scan`
  fn (`:207`), `classify_structural_terminator_block_from_table` call (`:219`), `escape_mask_64`
  (`:237`), `prefix_xor_64` (`:239`). `json/value.rs:143` `value_from_ref`. The V1 R2 fold
  (cite `:207`/`:217-219`/`:239` not the bare `:219`) is APPLIED pass-wide; the remaining
  `:217-219` bracket spans the multi-line call whose statement-line is `:219` — precise.
- **Orphan / non-candidate, exact.** `parse_4_digits` (`digit_mac.rs:5`), `parse_4_digits_dotprod`
  udot kernel (`:27`, the `udot …` at `:40`), scalar twin (`:15`). `ls tests/ | grep -i digit`
  = EMPTY (G4 checkasm gap confirmed). i8mm grep-clean-absent confirmed (zero `usmmla`/`ummla`/
  `is_aarch64_feature_detected!("i8mm")` in `bbnf-simd/src/`). `W5C_REQUEST_FACT_PROFILES`
  (`codegen/src/lib.rs:336`, referenced `:299`) — the Lock-14 overfit re-entry seam.
- **Comparator strictness planes, exact.** `css_canon_bench.rs:250` `assert!(n >= 50)`;
  `CssparserFullParseProbe` (`:294`, `parse_value` `:337`) — the token-scan flaw-probe plane,
  materializes nothing, correctly classed as the parity oracle NOT a >SOTA bar. lightningcss =
  materializing full-CSSOM = the fair >SOTA bar. No SOTA-beat claim rests on a permissive
  comparator (§8.1 discipline holds).
- **V1 R4 fold — APPLIED.** P2-A now quotes the LOCKED V4 band (full÷lcss 2.01–3.09× BEATS;
  fact_stream 0.60–0.77×; lightningcss 833–1261 Mbps) as load-bearing (`p2a:18-39,179-182,521`),
  with the 3-run spread explicitly labelled NON-load-bearing disclosure (`p2a:24-27`). The
  stale "3.6×" / "667–1015 Mbps" V3 snapshot band is GONE (grep-clean).
- **V1 R1 fold — APPLIED in all bodies, ONE residual in a sources annotation (R1-resid, §2).**

## §1 — Candidate-to-hot-leaf traceability ledger (the CH1 core test)

Every §2 candidate across all six artefacts, with its named S-P1 antecedent and the V2 CH1
verdict. LOCKED named hot-leaf pool (§3.3): `find_component_delim` 56.52–59.24% (scan) +
`consume_balanced_at` 10.31–11.05% → ONE scan target ~69%; `emit_fact_stream` 24.59–25.01% +
~57.63–64% alloc floor (string/tape); `push_ascii_lower_hex` 8.98–9.11% (FNV diagnostic —
explicitly NO primitive); the orphan udot/i8mm digit family (zero CSS antecedent).

| Artefact / candidate | Named P1 antecedent | Traces? | V2 CH1 |
|---|---|---|---|
| P2-A CP-A1 byte-class classifier (eq-set fan for CSS, lo6 table for JSON) | `find_component_delim`+`consume_balanced_at` ~69% | YES | ACCEPT (shape reconciled to eq-set — V1 N1 folded) |
| P2-A CP-A2 `push_plain_offset` sink | `emit_fact_stream` 24.59% + ~57.63% floor | YES | ACCEPT |
| P2-A CP-A3 lazy `ValueRef` rider | the String floor (replaces `emit_fact_stream`) | YES (named indirect) | ACCEPT |
| P2-A CP-A4 tokenize-once reuse | `find_component_delim`/`find_colon_before`/`parse_declaration` 2–3× re-walk | YES | ACCEPT |
| P2-A non-cands (FNV, digit, asmjson) | non-candidate / orphan / host-blocked | N/A | ACCEPT (correctly excluded) |
| P2-B C-B1 `byte_class_from_eq_set_64` | `find_component_delim`+`consume_balanced_at` ~69% | YES | ACCEPT |
| P2-B C-B2 `push_plain_offset` | `emit_fact_stream` 25.01% + ~64% floor | YES | ACCEPT |
| P2-B C-B3 udot digit kernel | NONE — process-REJECTED (G1/G2/G4 fail) | correctly NONE | ACCEPT (rejection sound) |
| P2-B C-B0 admission process | the deliverable (process, not a kernel) | N/A | ACCEPT |
| P2-C C1 lo6 TBL classify | ~69% scan leaf; INADMISSIBLE (lo6 collision) → C2 | YES | ACCEPT |
| P2-C C2 eq-set fan classify | ~69% scan leaf (admissible route) | YES | ACCEPT |
| P2-C C3 shrn movemask | mask-extract on the scan leaf (fold-only) | YES (folded) | ACCEPT |
| P2-C C4 host CTZ first-match | index-extract on the scan leaf (fold-only) | YES (folded) | ACCEPT |
| P2-C C5 UDOT digit | NONE — orphan, re-admission-gated | correctly NONE | ACCEPT (orphan-flagged) |
| P2-C C6 i8mm batch | NONE — net-new, doubly orphan-blocked | correctly NONE | ACCEPT (orphan-flagged) |
| P2-D D1 `push_plain_offset` emit | `emit_fact_stream` 24.59% + 57.63% floor | YES | ACCEPT |
| P2-D D2 lazy `ValueRef` projection | the typed-materialisation cost (`emit_fact_stream`) | YES | ACCEPT |
| P2-D D3 O(1) checkpoint/truncate | recognition control loop 28.87%+2.45%; re-confirm obligation | YES (re-framed) | ACCEPT (V1 R3 folded) |
| P2-D D4 one-shot SIMD reserve | the 57.63% floor (grow churn), gated behind D1/D2+scan | YES | ACCEPT |
| P2-D D5 sparse-flag side-table | mechanism for D2's cheap kind-disambiguation | YES (indirect, guarded) | ACCEPT |
| P2-D D6 second substrate | NONE — REJECT-on-sight (Lock 1 anchor) | correctly NONE | ACCEPT |
| P2-E G1 `comment_body_mask_64` | comment-skip arm of the ~69% scan leaf | YES | ACCEPT |
| P2-E G2 `bracket_depth_mask_64` | `consume_balanced_at` 11.05% recursion | YES | ACCEPT |
| P2-E G3 `scan_components_to_index` | whole ~69% scan (same-wave consumer of G1/G2) | YES | ACCEPT |
| P2-E G4 digit checkasm gate | NONE — gated behind W1/W2 typed re-profile | correctly NONE | ACCEPT (gated) |
| P2-E G5 FNV/hex | non-candidate (8.98% diagnostic, retires) | N/A | ACCEPT |
| P2-F CF-1 tape-append + projection | `emit_fact_stream` 24.59% + ~57.63% floor | YES | ACCEPT (conditional §1.4) |
| P2-F CF-2 byte-class classifier | ~68% scan leaf | YES | ACCEPT |
| P2-F CF-3 commit-by-construction Alt | recognition control 28.87%+2.45%; re-confirm obligation | YES (re-framed) | ACCEPT (V1 R3 folded) |
| P2-F CF-4a udot wire | NONE — orphan, gated | correctly NONE | ACCEPT (orphan-flagged) |
| P2-F CF-4b i8mm net-new | NONE — REJECT on current evidence, hard-gated | correctly NONE | ACCEPT (orphan-flagged) |
| P2-F CF-0 negative space | proves NOT-needed (unicode/dispatch/FNV) | N/A | ACCEPT |

**Result:** No candidate is a speculative kernel admitted WITHOUT a P1 antecedent. The orphan
family (B-B3 / C5 / C6 / G4 / CF-4a / CF-4b — all the one underlying udot/i8mm digit kernel) is
uniformly marked NONE-antecedent and hard-gated behind a future typed-`ValueRef` re-profile,
never proposed as active. **The two V1 CH1 weaknesses (D3 / CF-3 speculative-checkpoint
antecedent overstatement, V1 R3) are FOLDED:** both now state in frontmatter AND §2 that the
LOCKED profile classes 28.87%+2.45% as a *recognition control loop*, NOT measured speculative
checkpoint/rollback self-time, and both carry the explicit S-P1-re-confirm obligation (P2-D
`p2d:19-23,300-322,431`; P2-F `p2f:211-230,309,323-327`). D3 now uses CF-3's framing verbatim
(the V1 R3 instruction). They trace (the control loop is a named leaf) and survive as
CONDITIONAL/re-confirm candidates, not measured-rollback hypotheses.

## §2 — Dispositions (path:line + concrete fix)

### REVISE

**R1-resid — P2-C §5 sources annotation retains "mod-0x3f collision guard" diction (incomplete
V1 R1 fold).** (`p2c:443`.) Every BODY occurrence in P2-C correctly reads "low-6-bit mask
(`byte & 0x3f`)" — `:154-160` independently re-verifies the collision and states "specific to
the `& 0x3f` MASK … under a *true* modulo `0x7b % 0x3f = 0x3c` would NOT collide"; `:306,:410`
likewise. The single residual is the §5 Sources line: "`lo6_table_admissible` :101 — the
**mod-0x3f** collision guard". This mislabels the operation as a modulo in exactly the spot
V1 R1 named. The CONCLUSION is unaffected (the body is correct and re-verified at source this
cycle), so no candidate verdict flips. **Fix:** in `p2c:443` replace "the mod-0x3f collision
guard" with "the low-6-bit (`& 0x3f`) collision guard". CH1 REVISE (cosmetic-correctness
provenance; one line; no candidate disposition changes). All other R1 sites across all six
artefacts are clean (P2-A `:243-247`, P2-B `:150-152,293-294`, P2-D `:150-156,357`, P2-F
`:52,65-68,196,411` all read `& 0x3f` / low-6-bit + the modulo-disambiguation note).

### ACCEPT (load-bearing, called out)

- **The lo6-collision pivot (P2-C C1→C2 / P2-F §1.2 / P2-A CP-A1 / P2-B §2 / P2-D §1.4) is
  CH1-exemplary and now PASS-WIDE CONSISTENT.** Independently re-computed at source: `;`/`{`
  collide on the low-6-bit mask; the guard correctly forces scalar/eq-set; the eq-set fan is a
  genuine NEON body and the table-NEON is a scalar passthrough. **V1 N1 (cross-artefact shape
  divergence) is FOLDED:** P2-A CP-A1 no longer names a "`byte_class_index_64`/`vqtbl4q_u8` lo6
  cascade" as the CSS shape — its §2 (`p2a:228-253`) now names the eq-set fan
  (`byte_class_from_eq_set_64_neon`) as the CSS route, with the lo6 table explicitly JSON-only,
  carrying a dedicated §4 risk (`p2a:429-434`) forbidding the unearned-SIMD lo6 route on CSS.
  All five artefacts that touch the scan route now agree on the eq-set fan for CSS — S-P3 will
  not be handed a route P2-C proved scalar-falls-back.
- **Orphan discipline (udot/i8mm) is uniformly correct.** Each of B-B3/C5/C6/G4/CF-4a/CF-4b
  names "NONE" antecedent, cites the LOCKED orphan-blocked clause (§3.3) + the profile-first
  re-admission gate (re-profile typed `ValueRef` path AFTER W1/W2), and is flagged
  inventory-only / gated-contingency, never proposed. The udot kernel's MISSING checkasm gate
  (`ls tests/ | grep digit` EMPTY) is correctly named as the G4 deliverable, itself sequenced
  behind the antecedent-supplying re-profile. CH1's "speculative kernel → REJECT" is satisfied
  because none is proposed as active.
- **SOTA citations correctly source-and-plane keyed.** simdjson (Stage-1/On-Demand, arXiv
  1902.08318 + `parse_many.md:54-57`/`basics.md:344-350` @ `79bbba3e`), sonic-rs (`README.md:60-66/78-90`
  @ `03545a95`, SK-V6 `utf8_lossy` permissive caveat carried `p2a:110-112`), yyjson (`README.md:10-18`
  @ `d6085270`, the scalar-can-be-SOTA refutation anchor supporting tape-first lever order),
  asmjson (host-blocked, x86 AVX-512-only, `ARCHITECTURE.md:1206,1284`). lightningcss =
  materializing full-CSSOM (fair >SOTA bar, comparator-flame ~30% typed-node build+drop);
  cssparser = token-scan flaw probe (parity oracle, NOT a SOTA bar) — both planes correctly
  classed per §8.1 and re-verified at `css_canon_bench.rs:250,294`.
- **ISA citations correctly manual-keyed.** Arm ARM (DDI 0487) for TBL/TBX (`vqtbl4q_u8`),
  SHRN/SHRN2 (`vshrn_n_u16` movemask), CMEQ (`vceqq_u8`), UDOT/SDOT (FEAT_DotProd),
  USMMLA/UMMLA (FEAT_I8MM), RBIT+CLZ / FEAT_CSSC CTZ, PMULL (FEAT_AES). x86 secondary (SDM
  Vol.2: VPSHUFB, GF2P8AFFINEQB, VPCOMPRESSB, VPCLMULQDQ) inventoried as out-of-scope. All
  active routes aarch64-only; i8mm grep-clean-absent re-confirmed.
- **The substrate-union claim (P2-D §1.4) and the `ValueRef<G: EventGrammar>` genericity
  (P2-F §1.1) are TRUE at source** — re-verified `mod.rs:175`. No candidate proposes a second
  substrate; D6 is the explicit REJECT-on-sight anchor.

## §3 — Notes (no disposition; for the consolidation / sister lenses)

- **N1 (V1 N1 RESOLVED).** The V1 cross-artefact shape inconsistency (P2-A CP-A1 naming the
  lo6 cascade vs P2-C/P2-F proving it inadmissible) is folded; all artefacts now name the
  eq-set fan for CSS. No action.
- **N2 (provenance uniformity — CH6-adjacent).** Frontmatter SHAs: P2-A/B/C/D/F cite master
  HEAD `0ae1caa52`; some bracket the P1 baseline `6496fecae` (P2-E `:318`, P2-D `:533`). Both
  SHAs are internally consistent with the LOCKED consolidation and `git rev-parse HEAD` =
  `0ae1caa52…` this cycle. No correctness impact; CH6's plane.
- **N3 (~68%/~69% scan figure).** Artefacts use "~68%" (56.52%+11.05%≈67.6%, P1-E N=100
  snapshot) and "~69%" (59.24%+10.31%=69.55%, V4 lock) interchangeably; both within the locked
  range, harmless. P2-A/C/E carry the full 56.52–59.24% / 10.31–11.05% bands, the most precise
  framing.
- **N4 (P2-E `parse_declaration` cite).** P2-A/E cite `parse_declaration:247`; the fn opens
  `:242` (the membership over `b";}"` is in its body). Within-tolerance, not a defect; if a
  pass-wide precision sweep runs it could be tightened to `:242`.

## §4 — Counts + verdict

Disposition unit = §2 candidate primitive (the §2.1 load-bearing artefact) + the §1
findings/sources rows carrying a load-bearing factual claim. Across the six artefacts:

- **Candidate primitives dispositioned:** 31 (CP-A1..A4 + 3 non-cand; C-B0..B3; C1..C6;
  D1..D6; G1..G5; CF-0..CF-4b).
- **ACCEPT:** 31 candidates. Every active candidate traces to a named S-P1 hot leaf; the orphan
  family is uniformly NONE-antecedent + hard-gated, never proposed; the two V1 R3 candidates
  (D3/CF-3) are re-framed to the re-confirm obligation and now ACCEPT.
- **REVISE:** 1 finding-level item (R1-resid — the single `p2c:443` "mod-0x3f" sources-line
  diction slip; the body is correct and source-re-verified, no candidate flips).
- **REJECT:** 0. No candidate is a speculative kernel admitted without a P1 antecedent; no
  SOTA-beat rests on a permissive comparator; no ISA claim is uncited.

**Candidate-level ACCEPT rate: 31/31 = 100%.** Counting R1-resid against the full surface
(31 candidates + 1 finding = 32): **31/32 = 96.9%** ACCEPT. Both exceed the §3Z 95% bar.

**CH1 V2 verdict: ACCEPT (converged on the CH1 plane).** All four V1 folds landed: R1 applied
in every body (one residual sources-line slip remains, R1-resid, trivial), R2 line-cites
tightened pass-wide, R3 D3/CF-3 re-framed to the recognition-control + re-confirm obligation,
R4 P2-A re-anchored to the LOCKED V4 band, and the V1 N1 shape inconsistency reconciled to the
eq-set fan. The candidate pool is correctness-sound: every active candidate traces to a named
S-P1 hot leaf, the orphan discipline is exemplary, the lo6-collision finding is independently
re-verified at source this cycle, and the SOTA/ISA citations are correctly source-and-plane
keyed. The lone REVISE is a one-line cosmetic diction fix that flips no verdict. CH1 returns
≥95% for V2; with a clean V1→V2 fold trajectory this is the second consecutive cycle the CH1
candidate pool is antecedent-sound (V1 had ZERO REJECT, V2 has ZERO REJECT and 100%
candidate-level ACCEPT). Fold R1-resid into V3 if another cycle runs; CH1 is otherwise
convergence-ready on its plane.
