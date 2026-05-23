# CH4 COST — S-P0 Overfit Audit Hardening V2 Disposition

Lens binding: `restart/prompts/ORCHESTRATOR.md §3W CH4` ("LOC budget,
risk class, wave alignment, and hard cap are stated and realistic;
same-wave consumer present per kernel/primitive"). Dispatch scope from
`CHALLENGE-V2-ADDENDUM.md §1` CH4 row:

> verify V2 folds introduce zero LOC/risk/cap drift; SK-V14 SYNTHESIS
> C-1..C-5 unchanged.

Plus from `CHALLENGE-V2-ADDENDUM.md §1` CH3 row crossing into cost
territory: "verify F-V2-SYNTHESIS-2 co-derivation note preserves
wave-cost arithmetic" — the co-derivation note re-attributes three
sequencing signals (A3/A5/A6 cross-confirms of the 8→9 / 64→67 /
R4-before-PRUNE-2 deltas) to a single piece of evidence (`css_pretty`
addition), which has direct CH4 implications for the S-P3 risk-
weighting of PRUNE-4.

V2 reviews 5 modified S-P0 files (commit `1735882a5`, +113/-55):
A3 (1 fold), A4 (3 folds), A5 (1 fold + bonus), A6 (1 fold),
SYNTHESIS (5 folds). A1, A2 STAND. The cost surface under V2 review:
do any of the 11 V2 fold packets perturb the §3.1 coverage arithmetic
(74 → C-1..C-5), the C-N envelope ceilings (5.65k–8.38k total), the
PRUNE-4 sub-wave count (9), the 30-min wall-clock cap per sub-wave, or
the §2.4 CH7-companion attribution paths?

## §0 — Disposition summary

- **V2 ACCEPT-rate: 100 % (16 / 16 sectioned dispositions).**
- **V2 REJECT count: 0.**
- **V2 REVISE count: 0.**
- **Critical findings: 0.**
- **CH4 V2 verdict: ACCEPT.** All 11 V2 fold packets are envelope-
  neutral on the CH4 surface. The §3.1 coverage table arithmetic
  preserves 41 + 7 + 11 + 4 + 11 = 74; the C-1..C-5 LOC envelopes
  (2.8k–3.4k / 600–1.08k / 1.2k–2.0k / 800–1.4k / 250–500) hold
  verbatim; the PRUNE-4 sub-wave count remains 9 with the same
  9 × 30 = 270-min cluster wall-clock; the V5 alpha-hardening
  roster-count-agnostic phrasing absorbs the V2 §1.2 census reframing
  ("11 NEW *categories* = 20 NEW *rows*") without re-arithmetic; the
  F-V2-SYNTHESIS-2 co-derivation note actually *reduces* the
  S-P3 risk-weighting on PRUNE-4 (three cross-checks of one evidence,
  not three independent regression signals); the F-V2-SYNTHESIS-5
  lint-glob extension (runtime-only → runtime+codegen) preserves the
  C-3 200-LOC validator allocation OR the LOCKS.md companion-lint
  attribution path. No fold required.

### §0.1 — V2 cost-axis verification points

Per CHALLENGE-V2-ADDENDUM §1 CH4 + §1 CH3 cost-relevant clauses:

**Point (1): V2 folds introduce zero LOC/risk/cap drift.** VERIFIED:

- *F-V2-A3-1 (H3 HIGH → LOW; H6 freestanding HIGH; L8 records
  reclassification).* The 30-finding aggregate distribution shifts
  from 11C/7H/5M/7L to 11C/6H/5M/8L — total unchanged at 30. The
  §3.1 coverage table maps all 30 A3 findings to **C-1** regardless
  of within-tier severity, so the rebalance is envelope-neutral.
  Specifically: H3's reclassification from HIGH to LOW (test-fixture
  call site `decision_csp.rs:235` inside `#[cfg(test)] mod tests`)
  drops from the "Tier-2 HIGH" 7-violation slate to "Tier-4 LOW"
  bulk-rename PR after PRUNE-3 — same C-1 bucket, lower implementation
  risk (bulk rename, not architectural rewrite). H6 (CSS L4 entry-rule
  absence) is now freestanding HIGH with its own derivation chain
  (production call site `passes/src/lib.rs:478` is grammar-generic via
  `finalize_rule(&grammar.name)`; the gap is acceptance-test coverage,
  closed by R4's CSS L4 entry-rule constraint test). The R4 binding
  was already in C-3 (1.2k–2.0k envelope); the H6 acceptance-test
  addition is bounded by a single constraint test (~20-50 LOC), well
  within R4's allocation. **C-1 envelope HELD (2.8k–3.4k); C-3
  envelope HELD (1.2k–2.0k); risk class downgraded for H3→L8 (VERY
  HIGH → MED-LOW for that specific finding within C-1).**

- *F-V2-A4-1 (NEW-1 scope-extension framing).* The fold reframes
  the JSON `json_sink_direct` chunk (~15% of `generated.rs`) as the
  V13-HONEST minority share, preserves V13 §7.1 row 1 HONEST verdict,
  and clarifies that V2 extends scope to the 5 pass-through template
  files + the `include_str!()` template body (~85% majority). Cost-
  axis impact: zero. The PRUNE-2 deletion surface (250–500 envelope
  for revert / REDRESS scribe) covers the same hand-written template
  files regardless of framing; the C-1 deletion of the 8 `*_provider.rs`
  files is unchanged; the C-3 R4 regen-css generative pipeline that
  replaces the 7 CSS templates + 1 JSON template is unchanged. **All
  three C-N envelopes HELD; no risk drift.**

- *F-V2-A4-2 (json_provider line-cite refresh).* `sed -n` verification
  cites: config=:48, generated=:60, parser=:64, view=:68, value=:72;
  ranges :80-100 + :60-78 + :48-78; methodology line count "99 → 101".
  (Independent `wc -l` returns 100 — CH1 may want to dispose this off-
  by-one, but the cost-axis is line-citation cosmetic with zero LOC
  envelope implication.) The `include_str!` mechanism is unchanged;
  the 5 pass-through files (`config_rs`, `generated_rs`, `parser_rs`,
  `view_rs`, `value_rs`) remain the PRUNE-2 deletion surface inside
  the 250–500 C-5 envelope. **C-5 envelope HELD; line-cite refresh
  is documentary-only; zero cost surface touched.**

- *F-V2-A4-3 (NEW-2 "Three" → "Four").* The fixture-lookup scanner
  count tightens from 3 to 4 (adding `at_rules_and_media` via the
  `CAPTURED_W2_INPUT` short-circuit alongside the 3 `CANONICAL_FIXTURE`
  short-circuits in `nested_layout`, `stylesheet_selectors`,
  `vendor_and_custom_atrules`). All 4 fixture-lookup templates fold
  into PRUNE-2 (C-5 250–500) for revert + R4 (C-3 1.2k–2.0k) for the
  generative replacement. The 4-vs-3 finding count is enumeration
  precision, not envelope expansion: PRUNE-2 already deletes all 7
  CSS template directories, and R4 already generates all 7 from the
  15 `.bbnf` files. **C-3 + C-5 envelopes HELD; one additional fixture
  short-circuit is +0 LOC delta in the deletion target.**

- *F-V2-A5-1 (verdict-line FAIL-at-HEAD pattern across §0:11 + §3:
  102-107 + §4 row 4 + §5 closing bonus).* The verdict reframes from
  "PARTIAL PASS" to "FAIL at HEAD, PASS conditioned on C-5 (PRUNE-1
  + PRUNE-2) + C-4 (PRUNE-5) landing". Cost-axis impact: the
  sequencing edge **C-5 → C-4** (revert before wiring) is now
  explicit at A5 §3:102-107 + §4.1 + §5 closing. This binds the
  same R3 R-target sequencing already in SK-V14 SYNTHESIS §0.3 R3
  ("PRUNE waves before any new admit attempt"); the audit confirms
  the ordering from the decision-engine angle without expanding
  C-4 (800–1.4k) or C-5 (250–500). The §4 row 4 fold extends the
  LOW finding's posture from "no-op pending C-4" to "Preserve through
  PRUNE-5; gate-rejection invariant inside C-4 entry-gates" — i.e.,
  the block-ID chain stays as a gate-rejection check inside the C-4
  entry-gate manifest. **C-4 entry-gate manifest is bounded by the
  C-4 800–1.4k envelope; gate-rejection invariant ≈ 20-40 LOC of gate
  check logic (negligible against 800–1.4k); zero envelope expansion.**
  The bonus §5 closing fold preserves coherence with the verdict-line
  pattern across A5 — same cost surface, no drift.

- *F-V2-A6-1 (LegacyPath both-readings-preserved disambiguation).*
  The fold adds the V13-vs-V14 disambiguation paragraph: V13 Pattern G
  CLEAN reading did not enumerate LegacyPath; V14 records as scope-
  extension over V13 Pattern G, not reversal. Cost-axis impact: zero.
  The C-1 PRUNE-4 typed-path collapse already incorporates the
  LegacyPath rewrite as a sub-task per synthesis §3.1 line 305 ("fold
  as PRUNE-4 sub-task OR open small 'C-6 typed-path collapse'"); CH4
  V1 §3.1 already disposed that the LegacyPath rewrite is bounded by
  4 files × ~50 LOC ≈ 200 LOC, well within C-1's 2.8k–3.4k headroom.
  The V2 V13-disambiguation framing does not change the rewrite cost
  or the C-1 absorption. **C-1 envelope HELD; no C-6 candidate
  required; ≤5-candidate alpha-E §1 ceiling preserved.**

**Point (2): SYNTHESIS V2 folds preserve C-1..C-5 mapping and
envelope arithmetic.** VERIFIED:

- *F-V2-SYNTHESIS-1 (census reconciliation: 54 CONFIRM / 20 NEW per
  per-axis column-sum; 11 NEW *categories*).* The reframing
  distinguishes per-row count (20) from per-category count (11) —
  both totaling to 74 with 54 CONFIRMS. The §3.1 coverage table
  arithmetic remains 41 + 7 + 11 + 4 + 11 = 74 (zero orphan;
  unchanged from V1). The V5 alpha-hardening F-V5-α-E-1 roster-count-
  agnostic phrasing absorbs the per-row vs per-category recount
  precisely because the C-N envelope arithmetic was always per-
  *finding* (per the §3.1 line-by-line mapping), not per-NEW-tally.
  **§3.1 coverage arithmetic HELD; no envelope drift from the census
  refinement.**

- *F-V2-SYNTHESIS-2 (co-derivation note: A3/A5/A6 cross-confirms of
  `css_pretty` addition are co-derived, not orthogonal).* This is the
  CH4-pivotal V2 fold. The note rebinds the S-P3 risk-weighting:
  PRUNE-4 sub-wave count delta (8→9), Pattern H file count delta
  (64→67), and R4-before-PRUNE-2 sequencing constraint are **one
  evidence + three cross-checks**, not three independent regression
  signals. Wave-cost arithmetic implication: the cluster wall-clock
  for the PRUNE-4 9-sub-wave cluster remains 9 × 30 = 270 min (vs
  V13-implicit 8 × 30 = 240 min for the same surface) — the V2
  co-derivation note does NOT add three independent +30-min waves
  on top of each other; it confirms one +30-min wall-clock delta (the
  9th sub-wave for `css_pretty`) with three audit-axis cross-checks
  (A3 `crates/core/src/runtime/css_pretty/` directory presence;
  A5 cross-references via decision-engine roster; A6 Pattern H +3 file
  delta). The S-P3 risk-weighting on PRUNE-4 should treat the
  9-sub-wave count as ONE evidence with HIGH confidence (three
  independent observers), not THREE evidence streams (which would
  triple-count the same finding). **Wave-cost arithmetic HELD at
  +30 min cluster wall-clock; the co-derivation note REDUCES S-P3
  risk-weight on PRUNE-4 by collapsing three cross-checks into one
  evidence + three observer confirmations; C-1 envelope HELD at
  2.8k–3.4k.**

- *F-V2-SYNTHESIS-3 (verdict-line phrasing aligned with A5).* The
  three SYNTHESIS sites (§0.1 A5 row, §0.2 lines 36-39, §5.1 bullet 2)
  re-render the A5 FAIL-at-HEAD verdict consistently. Cost-axis impact:
  same as F-V2-A5-1 above — the C-5 → C-4 sequencing edge is now
  explicit at the SYNTHESIS level; the C-4 + C-5 envelopes are
  preserved verbatim. **C-4 + C-5 envelopes HELD.**

- *F-V2-SYNTHESIS-4 (§1.2 NEW-2 "Three" → "Four"; CANONICAL_FIXTURE
  grep verification).* Same as F-V2-A4-3: the 4-vs-3 fixture-lookup
  count is enumeration precision, not envelope expansion; PRUNE-2 +
  R4 absorb the 4 templates inside their existing C-3 + C-5 envelopes.
  **C-3 + C-5 envelopes HELD.**

- *F-V2-SYNTHESIS-5 (§2.4 CH7-companion lint glob extended runtime →
  runtime+codegen).* The lint glob extends from
  `skinny/crates/runtime/src/grammars/**/*.rs` to
  `skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs`. The
  rationale cites the 42-file verification (runtime-side hits) + 8
  codegen-side template+provider files (independent verification:
  `grep -rln "@generated by skinny bbnf-codegen" skinny/crates/runtime
  /src/grammars skinny/crates/codegen/src` returns 42 runtime + 14
  codegen = 56 total, of which 8 are codegen-side template/provider
  files). The lint logic itself remains ~50-80 LOC of detector +
  manifest consultation per V1 CH4 §0.1 point 3 — extending the glob
  is one additional path constant + one additional match arm in the
  glob walker, ≈ +5-10 LOC. The C-3 200-LOC harness validator slice
  per α-E §5 line 411 absorbs this within its existing allocation;
  alternatively the LOCKS.md companion-lint amendment path remains
  zero source LOC. **C-3 envelope HELD (1.2k–2.0k); LOCKS.md
  alternative HELD (zero source LOC); the lint glob extension is
  envelope-neutral under both attribution paths.**

**Point (3): SK-V14 SYNTHESIS C-1..C-5 unchanged.** VERIFIED via
direct read of `restart/skinny/tranches/sk-v14/SYNTHESIS.md:269-275`:

```
| **C-1** | … 2.8k – 3.4k | VERY HIGH (architectural; multi-wave) |
| **C-2** | … 600 – 1.08k | HIGH (harness + comparator surface) |
| **C-3** | … 1.2k – 2.0k | HIGH (xtask + corpora + dual-tree round-trip) |
| **C-4** | … 800 – 1.4k | VERY HIGH (Lock-1 substrate-ceiling surface) |
| **C-5** | … 250 – 500 (delete-heavy) | MED-LOW (revert + REDRESS scribe) |
```

Total envelope ≈ 5.65k – 8.38k per α-E §2; SK-V14 SYNTHESIS line
277 confirms the total verbatim. **No C-N envelope touched by any V2
fold.** Note that SK-V14 SYNTHESIS still cites "8 sub-waves" / "64
hand-written" at lines 95 + 271 — the audit-overfit SYNTHESIS at §2.3
+ §3.3 updates this to 9 / 67 via the `css_pretty` addition, with +30
min wall-clock on the C-1 cluster total absorbed by the V5 roster-
count-agnostic phrasing. This is a documentary update that S-P3 will
propagate into SK-V14 SYNTHESIS as part of C-1 wave manifest authoring
(SK-V14 SYNTHESIS will need to read "9 sub-waves / 67 files" post-S-P3
plan landing); the +30-min wall-clock delta is the only quantitative
change, and the 2.8k–3.4k LOC envelope is unchanged.

## §1 — Per-fold disposition table

| Fold | V1 → V2 change | V2 disposition | Cost-axis notes |
|---|---|---|---|
| **F-V2-A3-1** | H3 HIGH → LOW (test fixture); H6 freestanding HIGH; L8 added | ACCEPT | A3 30-finding total unchanged (11C/6H/5M/8L instead of 11C/7H/5M/7L); all map to C-1 + R4 closure under C-3; envelopes 2.8k–3.4k (C-1) and 1.2k–2.0k (C-3) HELD. H3→L8 risk class downgrades VERY HIGH → MED-LOW within the LOW bulk-rename tier. |
| **F-V2-A4-1** | NEW-1 scope-extension framing | ACCEPT | Preserves V13 §7.1 row 1 HONEST verdict on `json_sink_direct` chunk (~15%); reframes 5 pass-through files + template body (~85%) as scope-extension. Zero cost surface; PRUNE-2 deletion surface (C-5 250–500) unchanged; R4 generative replacement (C-3 1.2k–2.0k) unchanged. |
| **F-V2-A4-2** | json_provider line-cite refresh (sed verification) | ACCEPT | Documentary line citations; zero LOC envelope implication. CH1 territory for the 99-vs-101 line-count drift (independent `wc -l` says 100). |
| **F-V2-A4-3** | NEW-2 "Three" → "Four" CSS fixture-lookup scanners | ACCEPT | Enumeration precision; all 4 templates fold into PRUNE-2 + R4 under existing C-3 + C-5 envelopes; +0 LOC delta in deletion target. |
| **F-V2-A5-1** | Verdict-line FAIL-at-HEAD across §0:11 + §3:102-107 + §4 row 4 + §5 closing | ACCEPT | C-5 → C-4 sequencing edge now explicit; binds the existing SK-V14 SYNTHESIS §0.3 R3 ordering; §4 row 4 gate-rejection invariant ≈ 20-40 LOC inside C-4 800–1.4k envelope. Coherence fold at §5 closing preserves verdict-line consistency. |
| **F-V2-A6-1** | LegacyPath V13-vs-V14 disambiguation | ACCEPT | Scope-extension framing; LegacyPath rewrite bounded by 4 files × ~50 LOC ≈ 200 LOC absorbed in C-1 PRUNE-4 sub-task; no C-6 candidate required; ≤5-candidate ceiling preserved. |
| **F-V2-SYNTHESIS-1** | Census 63/11 → 54/20 per-row + 11 NEW categories | ACCEPT | §3.1 coverage arithmetic 41+7+11+4+11 = 74 unchanged; V5 F-V5-α-E-1 roster-count-agnostic phrasing absorbs the per-row vs per-category distinction; no envelope drift. |
| **F-V2-SYNTHESIS-2** | §1.3 co-derivation note (A3/A5/A6 css_pretty cross-confirms) | ACCEPT | Wave-cost arithmetic HELD (+30 min cluster wall-clock for 9-sub-wave PRUNE-4 vs 8-sub-wave V13 implicit); the co-derivation note REDUCES S-P3 risk-weight on PRUNE-4 by collapsing three observations to one evidence + three cross-checks. C-1 envelope HELD. |
| **F-V2-SYNTHESIS-3** | Verdict-line phrasing aligned with A5 across §0.1 + §0.2 + §5.1 | ACCEPT | Same C-5 → C-4 sequencing edge as F-V2-A5-1; C-4 + C-5 envelopes HELD. |
| **F-V2-SYNTHESIS-4** | §1.2 NEW-2 "Three" → "Four" with CANONICAL_FIXTURE grep verification | ACCEPT | Same as F-V2-A4-3 + SYNTHESIS-side mirror; C-3 + C-5 envelopes HELD. |
| **F-V2-SYNTHESIS-5** | §2.4 CH7-companion lint glob extended runtime → runtime+codegen | ACCEPT | Lint glob extension ≈ +5-10 LOC absorbed in C-3 200-LOC validator slice OR LOCKS.md companion-lint amendment (zero source LOC); both attribution paths envelope-neutral; the 8 codegen-side template+provider files independently verified via `grep -rln "@generated by skinny bbnf-codegen" skinny/crates/codegen/src` (returns 14 files, 8 of which are template+provider per file enumeration). |
| **SK-V14 SYNTHESIS C-1..C-5** | Unchanged | ACCEPT | All five envelopes (2.8k–3.4k / 600–1.08k / 1.2k–2.0k / 800–1.4k / 250–500) HELD verbatim; total 5.65k–8.38k preserved. SK-V14 SYNTHESIS will absorb "8→9 sub-waves" + "64→67 files" at S-P3 plan time as a documentary update within the same C-1 envelope. |
| **§3.1 coverage table arithmetic** | 41+7+11+4+11 = 74 | ACCEPT | Per-NEW-row recount (V1 implicit 11 → V2 explicit 20) does not perturb the per-finding mapping to C-N; arithmetic unchanged. |
| **§2.1 + §2.2 sequencing constraints** | Unchanged binding text | ACCEPT | R4 → PRUNE-2 + C-1 → C-4 sequencing edges preserved; the V2 A5 + SYNTHESIS verdict-line folds make C-5 → C-4 explicit, consistent with §0.3 R3 ordering. No envelope drift. |
| **§2.3 PRUNE-4 sub-wave count** | 9 unchanged | ACCEPT | The F-V2-SYNTHESIS-2 co-derivation note clarifies that the 8→9 delta is one evidence (`css_pretty` addition) with three observer confirmations (A3/A5/A6), not three independent +30-min waves; cluster wall-clock 9 × 30 = 270 min HELD. |
| **§2.4 CH7-companion extensions** | Lint glob extended; check-X unchanged | ACCEPT | Both attribution paths (C-3 sub-task OR LOCKS.md amendment) preserved; F-V2-SYNTHESIS-5 +5-10 LOC for glob extension absorbed in C-3 200-LOC validator slice. No C-6 candidate required. |

## §2 — Critical findings

**None.** No CH4-axis CRITICAL surfaces among the 11 V2 fold packets.

The cost surface is intact under V2: every V2 fold is either
documentary-precision (line-cite refresh, framing disambiguation,
verdict-line phrasing alignment) or census-arithmetic refinement (per-
row vs per-category count) that does not perturb the §3.1 coverage
mapping. The C-1..C-5 envelopes hold verbatim from V5 alpha-hardening
ACCEPT through V1 ACCEPT through V2 ACCEPT — a three-version unbroken
chain of envelope stability.

The F-V2-SYNTHESIS-2 co-derivation note is the V2 fold with the most
significant CH4 implication, and that implication is *favourable*:
the three audit-axis cross-confirms of the `css_pretty` addition
collapse from "three independent regression signals" to "one evidence
+ three observer confirmations", which reduces S-P3 risk-weighting on
PRUNE-4 (a higher-confidence single delta is easier to plan against
than three apparently-independent signals that would otherwise demand
defensive over-allocation).

## §3 — V3 fold recommendations

**None required.** CH4 V2 returns 100 % ACCEPT; no V2 fold introduces
LOC/risk/cap drift; SK-V14 SYNTHESIS C-1..C-5 hold verbatim. No fold
candidates surface for V3.

### §3.1 — Informational note (below action threshold)

S-P3 wave manifest authoring will need two documentary propagations
(neither a cost expansion):

1. **SK-V14 SYNTHESIS §3 line 271 + line 95** carries "8 sub-waves" +
   "64 hand-written" — the audit-overfit SYNTHESIS at §2.3 + §3.3
   updates this to 9 / 67 for the `css_pretty` addition. S-P3 should
   propagate the update with the +30-min wall-clock delta
   acknowledged within the existing C-1 2.8k–3.4k envelope (no
   re-arithmetic; the V5 roster-count-agnostic phrasing covers it).

2. **§2.4 CH7-companion attribution.** Both extensions (check-X
   subcommand pairing; Lock-14-companion lint with V2-extended glob)
   may attribute to C-3 (1.2k–2.0k headroom holds them) OR LOCKS.md
   amendment (zero source LOC for the lint; check-X stays in C-3
   either way). CH4 V1 §3.1 + V2 §0.1 point 2 above preserve both
   attribution paths as envelope-neutral; S-P3 chooses at plan time.

### §3.2 — Cross-lens hand-off (unchanged from V1)

- **CH1 (CORRECTNESS):** the json_provider line-count drift
  (V2 says "101 lines", `wc -l` says 100) is CH1 territory; CH4 takes
  the line citations as documentary-only with zero envelope
  implication.
- **CH2 (GENERALITY):** the F-V2-A3-1 H3 reclassification (test-
  fixture vs production-path) preserves Lock 14 generality scope;
  CH2 owns the generality claim, CH4 owns the LOC absorption.
- **CH3 (REGRESSION):** the F-V2-SYNTHESIS-2 co-derivation note +
  F-V2-A4-1 scope-extension framing + F-V2-A6-1 V13-disambiguation
  preserve V13 verdict integrity; CH3 owns the regression claim,
  CH4 confirms zero wave-cost arithmetic drift.
- **CH5 (HIDDEN COUPLING):** the F-V2-SYNTHESIS-5 lint glob
  extension (runtime → runtime+codegen) is the CH5-pivotal V2 fold;
  CH4 confirms the +5-10 LOC cost absorption inside C-3 200-LOC
  validator slice OR LOCKS.md amendment.
- **CH6 (ANTI-PAPER-CLOSE):** the F-V2-A5-1 + F-V2-SYNTHESIS-3
  verdict-line FAIL-at-HEAD pattern is the CH6-pivotal V2 fold; CH4
  confirms the C-5 → C-4 sequencing edge holds C-4 (800–1.4k) +
  C-5 (250–500) envelopes verbatim with the gate-rejection invariant
  ≈ 20-40 LOC inside the C-4 entry-gate manifest.
- **CH7 (OVERFIT-PRUNE):** the F-V2-SYNTHESIS-5 CH7-companion lint
  glob extension is CH7-authored at the audit-extension layer; CH4
  confirms the +5-10 LOC cost is absorbable inside C-3 or LOCKS.md.

## §4 — Summary

CH4 V2 verdict: **ACCEPT** at 100 % (16 / 16 sectioned dispositions).

All 11 V2 fold packets (A3 × 1; A4 × 3; A5 × 1 + bonus; A6 × 1;
SYNTHESIS × 5) are envelope-neutral on the CH4 cost surface. The
§3.1 coverage table arithmetic preserves 41 + 7 + 11 + 4 + 11 = 74;
the SK-V14 SYNTHESIS C-1..C-5 LOC envelopes (2.8k–3.4k / 600–1.08k /
1.2k–2.0k / 800–1.4k / 250–500; total 5.65k–8.38k) hold verbatim;
the PRUNE-4 sub-wave count remains 9 with the same 9 × 30 = 270-min
cluster wall-clock; the V5 alpha-hardening F-V5-α-E-1 roster-count-
agnostic phrasing continues to absorb the V2 §1.2 census reframing
("11 NEW *categories* = 20 NEW *rows*") without re-arithmetic.

The F-V2-SYNTHESIS-2 co-derivation note actually *reduces* the S-P3
risk-weighting on PRUNE-4 by collapsing three apparently-independent
regression signals (A3/A5/A6 cross-confirms of the 8→9 / 64→67 / R4-
before-PRUNE-2 deltas) into one evidence (`css_pretty` addition) with
three observer confirmations — a higher-confidence single delta that
S-P3 can plan against without defensive over-allocation. The F-V2-
SYNTHESIS-5 lint glob extension (runtime → runtime+codegen) costs
≈ +5-10 LOC absorbed in C-3 200-LOC validator slice OR LOCKS.md
companion-lint amendment (zero source LOC) — both attribution paths
envelope-neutral.

No V3 fold required; the audit's cost surface is sound through three
consecutive lens cycles (V5 alpha-hardening → V1 → V2), all 100 %
ACCEPT on the same envelope arithmetic. The V3 confirming pass over
unchanged V2 artefacts should likewise return 100 % ACCEPT on the
CH4 axis, closing the §3Z two-consecutive-cycle convergence chain.
