# SK-V17 S-P2 RESEARCH — CHALLENGE CH1 CORRECTNESS (V3)

Lens: CH1 CORRECTNESS. Pass: S-P2 Research. Cycle: V3. Date: 2026-05-29.
Authority: `restart/prompts/skinny/PASS-2-RESEARCH.md` §3 (CH1) + ORCHESTRATOR §3W/§3Z.
Subject: `restart/skinny/tranches/sk-v17/research/p2/{p2a-sota-teardown, p2b-dav1d-process,
p2c-arch-esoterica, p2d-substrate-tape, p2e-parse-that-gaps, p2f-grammar-neutral}.md`.
Input ground truth (LOCKED): `research/p1/{p1a..p1f}.md` +
`research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md §3` (master HEAD `0ae1caa52`;
P1 baseline `6496fecae`).
Prior cycle: V2 CH1 returned ACCEPT (31/31 candidate-level = 100%; 31/32 = 96.9% with the
lone R1-resid finding), naming ONE fold for V3: R1-resid (the `p2c:443` "mod-0x3f"
sources-line diction slip). This cycle (a) re-resolves every load-bearing `file:line`
against the tree at master HEAD, (b) audits whether the V2 R1-resid fold landed, and
(c) re-tests the full candidate-to-hot-leaf traceability ledger.
CH1 charter: (a) every candidate primitive traces to a NAMED S-P1 hot leaf — else REJECT
as a speculative kernel; (b) SOTA-comparator claims cited to the correct source AND the
correct strictness plane; (c) ISA claims cited to the architecture reference manual.

## §0 — Verification method (orchestrator-citable; re-resolved at source this cycle)

`git rev-parse HEAD` = `0ae1caa5235ee867c5c081f186b6452c51e44a14` (confirmed). Every
load-bearing claim below was re-resolved line-for-line against the tree this cycle.

- **The lo6-collision finding — INDEPENDENTLY RE-COMPUTED, EXACT.** `lo6_table_admissible`
  opens at `bbnf-simd/src/dispatch.rs:101`; the slot computation is `let slot = (byte & 0x3f)`
  at `:106` (`grep -n "byte & 0x3f"` = `:106`), rejecting on `seen[slot]` — a low-6-bit MASK,
  verified at source. `;`=0x3b → `0x3b & 0x3f = 0x3b` (slot 59); `{`=0x7b → `0x7b & 0x3f = 0x3b`
  (slot 59) — they COLLIDE under the mask; under true modulo `0x7b % 0x3f = 0x3c` (slot 60) they
  would NOT. The guard is a mask, the collision is real, the lo6/`vqtbl4q_u8` route is therefore
  structurally inadmissible for any CSS alphabet carrying `;{`. `select_classifier(alphabet:
  &'static [u8;64])` is at `:42`. The artefacts cite the fn at `:101` and the mask at `:106`
  consistently (P2-B `:149`, P2-C `:154`/`:157`, P2-D `:151`/`:155`, P2-F `:53`/`:67`/`:449`) —
  all correct against source.
- **The eq-set NEON body is genuine; the table NEON body is a scalar passthrough.**
  `byte_class_from_eq_set_64_neon` (`aarch64/byte_class_from_eq_set_64.rs:33`) is a real
  `vld1q_u8` + `vceqq_u8` per-member fan with `movemask_u8x16` pack (`:76`) — re-read at source.
  By contrast `byte_class_from_table_64_neon` (`aarch64/byte_class_from_table_64.rs:2`) tail-calls
  `crate::scalar::byte_class_from_table_64::byte_class_from_table_64_scalar` — a SCALAR
  PASSTHROUGH, confirmed at source. P2-A CP-A1's / P2-C C1's "routing CSS through the lo6/table
  path would claim a SIMD win it silently runs scalar" is a TRUE statement of source.
- **Hot leaves, exact.** `emit_fact_stream` (`css_l4_declaration_values/generated.rs:5`),
  `parse_block_item` (`:209`), `parse_declaration` (`:242`), `find_component_delim` (`:288`),
  `find_colon_before` (`:313`), `consume_balanced_at` (`:320`), `fnv64` (`:619`),
  `push_ascii_lower_hex` (`:628`) — every cite resolved exactly (`grep -n "fn …"`). (P2-A/E cite
  `parse_declaration:247`; the fn opens `:242` and the `b";}"` membership is in its body —
  within-tolerance carryover, N4, not a defect.)
- **Tape substrate, exact.** `TapeBuilder` (`assembler.rs:42`), `push_offset` (`:62`),
  `push_plain_offset` (`:71`), `reserve_offsets_cold` (`:89`), `patch_flags` (`:94`), `finish`
  (`:115`). `Tape` (`mod.rs:94`), `PayloadArena` (`:38`), `DocumentView` not separately needed;
  `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` (`mod.rs:175`) IS generic over
  grammar G — re-read at source (the `_grammar: PhantomData<fn() -> G>` field confirms). P2-F
  §1.1 / P2-D §1.4 load-bearing genericity claim is TRUE.
- **JSON antecedents, exact.** `json/scan.rs`: `scan_structurals` (`:22`), the NEON `scan` fn
  (`:207`), `classify_structural_terminator_block_from_table` call (`:219`). `json/value.rs:143`
  `value_from_ref`. P2-A frontmatter brackets the call as `:217-218`; the statement-line is `:219`
  — within the multi-line call span, precise.
- **Orphan / non-candidate, exact.** `parse_4_digits` (`digit_mac.rs:5`), `parse_4_digits_dotprod`
  udot kernel (`:27`, the `udot …` asm at `:40`), scalar twin (`:15`), `sdot` (`:63`). `ls
  bbnf-simd/tests/ | grep -i digit` = EMPTY (G4/C5 checkasm gap confirmed). i8mm grep-clean-absent
  re-confirmed (zero `i8mm`/`usmmla`/`ummla` in `bbnf-simd/src/`).
- **Comparator strictness planes, exact.** `css_canon_bench.rs:250` `assert!(n >= 50, "N must be
  >= 50 (SK-V17 telemetry-honesty gate)")`; `CssparserFullParseProbe` (`:294`, `parse_value`
  `:337`, `StyleSheetParser` driver `:286`) — the token-scan flaw-probe plane, materializes
  nothing, correctly classed as the parity oracle NOT a >SOTA bar. lightningcss =
  materializing full-CSSOM = the fair >SOTA bar. No SOTA-beat claim rests on a permissive
  comparator (§8.1 discipline holds).
- **LOCKED-profile antecedent figures, exact.** `HARDENING-S-P1-V4 §3.3`: `find_component_delim`
  59.24% / 56.52% (`:143`), `consume_balanced_at` 10.31% / 11.05% (`:144`, folds into ONE NEON
  byte-class target), recognition control loop `28.87 + 2.45` classed **structural** (`:145`,
  `:249`), `emit_fact_stream` 25.01% / 24.59% → `push_plain_offset` lever (`:159`),
  `push_ascii_lower_hex` 8.98% / 9.11% = NONE/FNV-diagnostic (`:160`), udot/i8mm orphan-blocked
  (`:169`). The P1-E §3.3 driver-frame lines `:182` (28.87%) and `:184` (2.45%) re-resolved.

## §1 — Candidate-to-hot-leaf traceability ledger (the CH1 core test)

Every §2 candidate across all six artefacts, with its named S-P1 antecedent and the V3 CH1
verdict. LOCKED named hot-leaf pool: `find_component_delim` 56.52–59.24% (scan) +
`consume_balanced_at` 10.31–11.05% → ONE scan target ~69%; `emit_fact_stream` 24.59–25.01% +
~57.63–64% alloc floor (string/tape); recognition control loop 28.87%+2.45% (structural);
`push_ascii_lower_hex` 8.98–9.11% (FNV diagnostic — explicitly NO primitive); the orphan
udot/i8mm digit family (zero CSS antecedent).

| Artefact / candidate | Named P1 antecedent | Traces? | V3 CH1 |
|---|---|---|---|
| P2-A CP-A1 byte-class classifier (eq-set fan CSS / lo6 table JSON) | `find_component_delim`+`consume_balanced_at` ~69% | YES | ACCEPT |
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
| P2-C C3 shrn movemask | mask-extract on the scan leaf (sub-task) | YES | ACCEPT |
| P2-C C4 host CTZ first-match | index-extract on the scan leaf (fold-only) | YES | ACCEPT |
| P2-C C5 UDOT digit | NONE — orphan, re-admission-gated | correctly NONE | ACCEPT (orphan-flagged) |
| P2-C C6 i8mm batch | NONE — net-new, doubly orphan-blocked | correctly NONE | ACCEPT (orphan-flagged) |
| P2-D D1 `push_plain_offset` emit | `emit_fact_stream` 25.01% + 57.63% floor | YES | ACCEPT |
| P2-D D2 lazy `ValueRef` projection | the typed-materialisation cost (`emit_fact_stream`) | YES | ACCEPT |
| P2-D D3 O(1) checkpoint/truncate | recognition control loop 28.87%+2.45% (structural); re-confirm obligation | YES (re-framed) | ACCEPT |
| P2-D D4 one-shot SIMD reserve | the 57.63% floor (grow churn), gated behind D1/D2 | YES | ACCEPT |
| P2-D D5 sparse-flag side-table | mechanism for D2's kind-disambiguation | YES (indirect, guarded) | ACCEPT |
| P2-D D6 second substrate | NONE — REJECT-on-sight (Lock 1 anchor) | correctly NONE | ACCEPT |
| P2-E G1 `comment_body_mask_64` | comment-skip arm of the ~69% scan leaf | YES | ACCEPT |
| P2-E G2 `bracket_depth_mask_64` | `consume_balanced_at` 11.05% recursion | YES | ACCEPT |
| P2-E G3 `scan_components_to_index` | whole ~69% scan (same-wave consumer of G1/G2) | YES | ACCEPT |
| P2-E G4 digit checkasm gate | NONE — gated behind typed re-profile | correctly NONE | ACCEPT (gated) |
| P2-E G5 FNV/hex | non-candidate (8.98% diagnostic, retires) | N/A | ACCEPT |
| P2-F CF-1 tape-append + projection | `emit_fact_stream` 24.59% + ~57.63% floor | YES | ACCEPT |
| P2-F CF-2 byte-class classifier | ~69% scan leaf | YES | ACCEPT |
| P2-F CF-3 commit-by-construction Alt | recognition control 28.87%+2.45% (structural); re-confirm obligation | YES (re-framed) | ACCEPT |
| P2-F CF-4a udot wire | NONE — orphan, CH1 REJECT on current evidence, gated | correctly NONE | ACCEPT (orphan-flagged) |
| P2-F CF-4b i8mm net-new | NONE — CH1 REJECT on current evidence, hard-gated | correctly NONE | ACCEPT (orphan-flagged) |
| P2-F CF-0 negative space | proves NOT-needed (unicode/dispatch/FNV) | N/A | ACCEPT |

**Result:** No candidate is a speculative kernel admitted WITHOUT a P1 antecedent. The orphan
family (C-B3 / C5 / C6 / G4 / CF-4a / CF-4b — all the ONE underlying udot/i8mm digit kernel)
is uniformly marked NONE-antecedent and hard-gated behind a future typed-`ValueRef` re-profile.
This cycle CF-4a/CF-4b strengthen the disposition: each now self-states "CH1 REJECT on current
evidence; HARD-GATED behind re-profile" (`p2f` CF-4a `:291`, CF-4b `:320`) — i.e. they dispose
themselves exactly as CH1 would, and are listed ONLY as gated contingencies, never proposed
active. C5/C6 (`p2c:236-277`) carry the identical NONE-antecedent + re-admission-gate framing.

**The V1 R3 candidates (D3 / CF-3) are FULLY FOLDED and verified at source.** Both now state in
both frontmatter-adjacent prose AND §2 that P1-E measured ZERO speculative-rollback / checkpoint
self-time on either benched plane, that the LOCKED profile classes 28.87%+2.45% as a
**recognition control loop / structural** (re-verified `HARDENING-S-P1-V4 §3.3 :145,:249`;
P1-E §3.3 `:182,:184`), and that the speculative-rollback share is a HYPOTHESIS gated on a hard
post-CF-1 typed-tape re-profile, NOT a measured antecedent (P2-D D3 `:281-330`, P2-F CF-3
`:220-262`). They TRACE (the recognition control loop is a named structural leaf) and survive
as CONDITIONAL / re-confirm candidates, not as measured-rollback hypotheses. This is the V1 R3
instruction applied verbatim and is CH1-exemplary.

## §2 — Dispositions (path:line + concrete fix)

### REVISE

**R-V3-1 — P2-A frontmatter is stamped `Cycle: V2` while all five siblings (P2-B/C/D/E/F) are
`Cycle: V3`.** (`p2a:3` reads "Pass: S-P2 Research. Cycle: V2."; `p2b:3`/`p2c:3`/`p2d:3`/`p2e:3`/
`p2f:3` all read "Cycle: V3.") This is a provenance-staleness slip: a V3 fold cycle ran (the
siblings re-stamped, R1-resid landed pass-wide) but P2-A was not re-stamped to V3. **No P2-A
candidate verdict flips** — every CP-A1..A4 antecedent, the eq-set-fan-for-CSS routing, the
LOCKED V4 band (full÷lcss 2.01–3.09×, fact_stream 0.60–0.77×, lightningcss 833–1261 Mbps), and
the lo6-collision pivot were all re-resolved at master HEAD this cycle and are correct; the V2
R1-resid never touched P2-A (P2-A's `:243-247` already reads `& 0x3f` / low-6-bit MASK). The
defect is purely the cycle stamp. **Fix:** in `p2a:3` change "Cycle: V2." to "Cycle: V3." (and,
if a precision sweep runs, optionally tighten the `parse_declaration:247` cite to `:242`, the
N4 carryover, and the `:217-218` call bracket to `:219`). CH1 REVISE (cosmetic provenance; one
line; no candidate disposition changes; CH6-adjacent plane). This is the ONLY open item.

### ACCEPT (load-bearing, called out)

- **The V2 R1-resid fold LANDED, pass-wide clean.** `p2c:443` now reads "the low-6-bit (`& 0x3f`)
  collision guard" (re-read at source) — the V2 "mod-0x3f collision guard" mislabel is GONE.
  `grep -rn "the mod-0x3f collision guard\|the modulo collision guard"` across all six artefacts =
  EMPTY. Every remaining "modulo" occurrence (P2-A `:246`, P2-B `:149,:152,:294`, P2-C `:154,:157`,
  P2-D `:151,:155`, P2-F `:53,:67,:449-450`) is in the correct disambiguation context ("would NOT
  collide under TRUE modulo `0x7b % 0x3f = 0x3c`"), with the guard itself consistently named the
  low-6-bit `(byte & 0x3f)` mask. The V2 fold is closed.
- **The lo6-collision pivot (P2-C C1→C2 / P2-F §1.2 / P2-A CP-A1 / P2-B §2 / P2-D §1.4) is
  CH1-exemplary and PASS-WIDE CONSISTENT.** Independently re-computed at source this cycle:
  `;`/`{` collide on slot 59 under the `& 0x3f` mask at `dispatch.rs:106`; the guard forces
  scalar/eq-set; the eq-set fan (`byte_class_from_eq_set_64.rs:33`) is a genuine NEON body and the
  table-NEON (`byte_class_from_table_64.rs:2`) is a scalar passthrough. All five artefacts that
  touch the scan route name the eq-set fan for CSS and the lo6 table as JSON-only — S-P3 will
  not be handed a route P2-C proved scalar-falls-back.
- **Orphan discipline (udot/i8mm) is uniformly correct and STRENGTHENED this cycle.** Each of
  C-B3/C5/C6/G4/CF-4a/CF-4b names "NONE" antecedent, cites the LOCKED orphan-blocked clause
  (`HARDENING-S-P1-V4 §3.3 :169`, P1-E §4.4a) + the profile-first re-admission gate (re-profile
  the typed `ValueRef` path AFTER CF-1/W1-W2), and is flagged inventory-only / gated-contingency.
  CF-4a/CF-4b additionally self-state "CH1 REJECT on current evidence" — they pre-empt the CH1
  test rather than awaiting it. The udot kernel's MISSING checkasm gate (`tests/ | grep digit`
  EMPTY) is correctly named as the G4/C5 deliverable, sequenced behind the antecedent-supplying
  re-profile. CH1's "speculative kernel → REJECT" is satisfied because none is proposed as active.
- **SOTA citations correctly source-and-plane keyed.** simdjson (Stage-1/On-Demand, arXiv
  1902.08318 + `parse_many.md:54-57`/`basics.md:344-350` @ `79bbba3e`), sonic-rs
  (`README.md:60-66/78-90` @ `03545a95`, SK-V6 `utf8_lossy` permissive caveat carried `p2a:110-112`),
  yyjson (`README.md:10-18` @ `d6085270`, the scalar-can-be-SOTA refutation anchor for tape-first
  lever order), asmjson (host-blocked, x86 AVX-512-only, `ARCHITECTURE.md:1206,1284`). lightningcss
  = materializing full-CSSOM (fair >SOTA bar, comparator-flame ~30% typed-node build+drop);
  cssparser = token-scan flaw probe (parity oracle, NOT a SOTA bar) — both planes correctly classed
  per §8.1 and re-verified at `css_canon_bench.rs:250,294,337`.
- **ISA citations correctly manual-keyed.** Arm ARM (DDI 0487) for TBL/TBX (`vqtbl4q_u8`),
  SHRN/SHRN2 (`vshrn_n_u16` movemask), CMEQ (`vceqq_u8`), UDOT/SDOT (FEAT_DotProd),
  USMMLA/UMMLA (FEAT_I8MM), RBIT+CLZ / FEAT_CSSC CTZ, PMULL (FEAT_AES). x86 secondary inventoried
  as out-of-scope/REDRESS-blocked. All active routes aarch64-only; i8mm grep-clean-absent re-confirmed.
- **The substrate-union claim (P2-D §1.4) and the `ValueRef<G: EventGrammar>` genericity (P2-F
  §1.1) are TRUE at source** — re-verified `mod.rs:175` (the `_grammar: PhantomData<fn() -> G>`
  field). No candidate proposes a second substrate; D6 is the explicit REJECT-on-sight anchor.

## §3 — Notes (no disposition; for the consolidation / sister lenses)

- **N1 (~68%/~69% scan figure).** Artefacts use "~68%" (56.52%+11.05%≈67.6%, P1-E N=100 snapshot)
  and "~69%" (59.24%+10.31%=69.55%, V4 lock) interchangeably; both within the locked range,
  harmless. The full 56.52–59.24% / 10.31–11.05% bands are the most precise framing and appear
  in P2-A/C/E.
- **N2 (`parse_declaration` cite — N4 carryover).** P2-A/E cite `parse_declaration:247`; the fn
  opens `:242` (the `b";}"` membership is in its body). Within-tolerance, not a defect; a
  pass-wide precision sweep could tighten it.
- **N3 (SHA provenance — CH6 plane).** Frontmatter SHAs cite master HEAD `0ae1caa52`; some bracket
  P1 baseline `6496fecae`. Both internally consistent with the LOCKED consolidation and
  `git rev-parse HEAD` this cycle. No correctness impact; CH6's plane. The P2-A cycle-stamp slip
  (R-V3-1) is the one provenance item rising to a CH1 disposition because it diverges from its
  siblings rather than merely being a duplicate SHA.

## §4 — Counts + verdict

Disposition unit = §2 candidate primitive (the §2.1 load-bearing artefact) + the §1
findings/sources rows carrying a load-bearing factual claim. Across the six artefacts:

- **Candidate primitives dispositioned:** 31 (CP-A1..A4 + 3 non-cand; C-B0..B3; C1..C6;
  D1..D6; G1..G5; CF-0..CF-4b).
- **ACCEPT:** 31 candidates. Every active candidate traces to a named S-P1 hot leaf; the orphan
  family is uniformly NONE-antecedent + hard-gated (CF-4a/CF-4b now self-REJECT on current
  evidence), never proposed; the two V1 R3 candidates (D3/CF-3) remain re-framed to the
  recognition-control + post-CF-1 re-confirm obligation and ACCEPT.
- **REVISE:** 1 finding-level item (R-V3-1 — the `p2a:3` "Cycle: V2" stamp diverging from its
  five V3 siblings; cosmetic provenance, no candidate flips).
- **REJECT:** 0. No candidate is a speculative kernel admitted without a P1 antecedent; no
  SOTA-beat rests on a permissive comparator; no ISA claim is uncited.

**Candidate-level ACCEPT rate: 31/31 = 100%.** Counting R-V3-1 against the full surface
(31 candidates + 1 finding = 32): **31/32 = 96.9%** ACCEPT. Both exceed the §3Z 95% bar.

**CH1 V3 verdict: ACCEPT (converged on the CH1 plane — second consecutive cycle).** The V2
R1-resid fold landed pass-wide (grep-clean; `p2c:443` corrected). Every load-bearing `file:line`
re-resolved exactly at master HEAD `0ae1caa52` this cycle: the lo6-collision (mask at `:106`, fn
at `:101`) independently re-computed, the eq-set NEON body / table passthrough re-confirmed, all
hot leaves and tape lines exact, `ValueRef<G: EventGrammar>` genericity confirmed at source. The
candidate pool is correctness-sound: every active candidate traces to a named S-P1 hot leaf, the
orphan discipline is exemplary (and stronger this cycle, with CF-4a/CF-4b self-REJECTing on
current evidence), the D3/CF-3 R3 re-framing is verified against the LOCKED structural-control-loop
classification, and the SOTA/ISA citations are correctly source-and-plane keyed. The lone REVISE
is a one-line cycle-stamp fix that flips no verdict. CH1 returns ≥95% for V3 with zero REJECT and
zero orphan REVISE — V2 (100% candidate-level) and V3 (100% candidate-level) are two consecutive
cycles of an antecedent-sound CH1 candidate pool. Fold R-V3-1 (and optionally the N2/N3 precision
items) into V4 if another cycle runs; CH1 is otherwise convergence-ready on its plane.
