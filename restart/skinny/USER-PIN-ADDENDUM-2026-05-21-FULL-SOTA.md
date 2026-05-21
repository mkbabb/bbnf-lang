# USER PIN ADDENDUM — Full SOTA + Indefatigable Campaign (2026-05-21)

Authority: user directive `2026-05-21`. This addendum extends
`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md` (the
2026-05-20 pin) campaign-wide. It is BINDING and OVERRIDES every
contradicting clause in any tranche SPEC, HANDOFF, DISPATCH-PROMPT,
or implementation-agent prompt.

Verbatim user directive:

> This MUST get all of the JSON numbers for every path >SOTA. Full
> semantic parity with lightningcss. Aggressive. Indefatigable.

## Scope

Campaign-wide. Applies from the moment of this commit through SK-V13
→ SK-V14 → SK-V15 → ... until full ADMIT or per-row architectural-
level intrinsic-block proofs cover every remaining row and feature.

## The bar

### A1. Full CSS L4 lightningcss semantic parity

Every non-OUT_OF_SCOPE feature in the SK-V13 scoping CSS parity gap
matrix (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`)
admits as a generated row > lightningcss on the same corpus / output
plane / strict equality. PARITY target = 24 features (1 admitted at
SK-V12; 23 to land). At close, no feature is "PARTIAL". Each is either
ADMITTED-PARITY or carries an architectural-level intrinsic-block
proof (lightningcss-can-do / bbnf-cannot, NOT implementation-limited).
Implementation-limited blocks are REOPENS, not closes.

### A2. Every JSON path > SOTA, every row

All 17 corpora × 3 planes = 51 JSON rows. Each row's Track 1 must
exceed sonic-rs strict on the same plane / corpus / strict equality.
The 13 N-direct rows have their REDRESS-119 fixpoint LIFTED — each
must reopen with fresh material differential and admit > sonic-rs
strict OR record an architectural-level intrinsic-block proof.

### A3. `parse_only` re-pinned admission-eligible

USER PIN D6 is amended. `parse_only` is no longer diagnostic-only. The
17 `parse_only` rows must beat sonic-rs strict `parse_only` on the
same corpus, or carry intrinsic-block evidence. Strict-vs-strict
comparator and strict equality still apply.

### A4. Indefatigable campaign

If a tranche close leaves any row below SOTA or any non-OUT_OF_SCOPE
CSS feature short of PARITY, Pass Alpha brackets to the next tranche
immediately and continues. Successive tranches roll automatically
until ALL of A1+A2+A4 admit. The campaign closes only by full ADMIT or
by per-row / per-feature architectural-level intrinsic-block proofs.
Per project memories `[no-deferrals]` and `[execute-planned-architecture]`:
no retreat under contact.

## Disposition of prior REDRESS

- **REDRESS 119** (13-row direct fixpoint) and **REDRESS 120** (SK-V11
  close) are HISTORY only. They record measured implementations under
  the pre-pin / pre-decision-engine surface. Under this addendum each
  row is wave-eligible.
- Reopens cite the prior fixpoint REDRESS + name the material
  differential. Per `[abrogate-before-patch]`, waves may DELETE a
  residual subsystem rather than patch it.
- The 10 rows outside the prior 3-reopen shortlist (canada, mesh,
  random, gsoc-2018, instruments, numbers, unicode_mixed,
  unicode_escapes, distinct_values, y_string_unicode) are EQUALLY
  reopen-eligible. The pre-SK-V13 "no current kernel route" ranking
  was made before W1a, W2, W4, and the decision-engine fold. SK-V13
  W5–W9 (CSP+egraph+cost resolver) and W8/W12 (union substrate) will
  generate kernel routes that did not exist at ranking time. Every
  row must be re-attempted in light of W5–W12 outputs.

## Acceptance tests

### CSS row (every new row)

1. Strict equality vs lightningcss on the admitted corpus.
2. Feature-coverage match: every variant lightningcss accepts, the
   row accepts; every variant lightningcss rejects, the row rejects.
3. Track 1 > lightningcss Mbps + 1 on the same corpus.
4. Independent oracle (cssparser, or a hand-checked golden table for
   productions cssparser does not cover).

A row landed without all four is REVISE, not ADMIT.

### JSON row (every admission, every plane)

1. Strict equality vs sonic-rs strict on the same plane.
2. Track 1 > sonic-rs strict Mbps + 1.
3. Strict-mode comparator only (no lossy / permissive flags).
4. No silent demotion of previously admitted A/GO rows.

## Per-wave row-movement target

Every behavior wave moves at least one row toward SOTA OR records an
architectural-level intrinsic-block on the row family it touched. No
"support-only" landings: every primitive wires same-commit to a
consumer that moves a row. The existing §2.2 micro-prove gate stays
in force.

## Round-trip rule

A row reopen that fails twice in the same tranche escalates to the
user with intrinsic-block evidence. The user re-pin then decides:
(a) re-pin OUT_OF_SCOPE the family (architectural-level block
acknowledged), or (b) continue under fresh material differential.
Without user re-pin, the campaign continues to the next wave.

## Parallelism — aggressive

Per `[high-parallelization]`, dispatch independent waves CONCURRENTLY
when their file domains do not overlap.

- CSS expansion (W3, W4, W10.{N}) — `crates/runtime/src/grammars/css_l4_*/`
  + `crates/codegen/src/css_*`.
- JSON N-direct (W11.{N}) — JSON runtime consumers + `bbnf-simd`
  consumers.
- Union (W8, W12) — substrate.
- Decision-engine fold (W5–W9) — `ir/`, `passes/`, codegen `lower/`.

When CHALLENGE accepts a wave plan, worktree-fan the non-overlapping
waves so the redress dispatches run in parallel. Per
`[agent-orchestration]` commit before parallelizing; use worktrees for
overlap.

## Redress hard-cap amendment for the decision-engine fold

Redress hard cap raised from 30 → 45 min for the W5–W9 fold and W12
union-SIMD wave. These waves can move multiple rows per redress; extra
equality + parity work justifies the cap. Research + plan caps
unchanged (20 / 15 min).

## Wave-manifest extensions

S-P3 authors:

- **W10.{1..N}** — one per non-OUT_OF_SCOPE CSS feature in the parity
  matrix not yet ADMITTED. N ≈ 22 (24 PARITY target − 1 SK-V12 admit
  − the W3 stylesheet+selectors fan-out).
- **W11.{1..13}** — one per JSON N-direct residual row. Material
  differential cited per row; route is fresh kernel from W5–W9 outputs
  unless the row admits sooner.
- **W14.{1..K}** — `parse_only` admissions per the 17 parse-only rows
  that currently NO-GO under the D6 amendment.
- **Wn close** — Pass Alpha bracket → SK-V14 if A1+A2+A4 are not
  fully admitted at SK-V13 close.

## Close condition (replaces base CLOSE clause)

Each tranche (SK-V13 and successors) closes when all of these admit:

- **G1 / A1** every non-OUT_OF_SCOPE CSS L4 feature ADMITTED-PARITY
  > lightningcss, OR architectural-level intrinsic-block proof.
- **G2** decision-engine fold landed (bbnf-regex extracted; e-graph
  Language wired; cost as `egg::CostFunction`; CSP solver + cascade
  deletion); JSON regression-free.
- **G3** ≥1 union variant ADMITTED or architectural-block.
- **G4** zero aarch64 orphans (wired or deleted with REDRESS evidence).
- **G5 / A2** every JSON row > sonic-rs strict on its plane, OR
  architectural-level intrinsic-block per row.
- **G6** totality V1.1 ratified.
- **G7 / A4** no tranche close reduces (no silent demotion of admit
  rows; no retreat from pinned bar).

If a tranche close fails on any row/feature without architectural-
level intrinsic-block proof, the close is REJECT. Pass Alpha brackets
the next tranche and the campaign continues.

## Rolling status reporting

Every Pass Alpha bracket publishes a per-row table to
`restart/skinny/ROLLING-SOTA-DELTA.md`:

```
| row | plane | T1_current | T1_sota | margin | tranche_admitted |
```

covering all 51 JSON rows + every CSS feature. The rolling delta vs
the prior tranche is the campaign progress signal. Rows that move
backward (silent demotion) FAIL the bracket.

## No-deferral rule

No optimization is deferred to a "future wave" or "future tranche"
without a measured architectural-block proof for the deferral. Per
`[no-deferrals]`: integrate everything in the current pass. The single
exception is the Pass Alpha bracket between tranches — and even there
the bracket is an organizational seam, not a deferral seam: the
indefatigability clause means the work continues immediately on the
other side.

## Escalation (replaces base ESCALATION clause)

Surface to user IMMEDIATELY when:

- A row family proves architecturally intrinsic-block (not
  implementation-block); the user re-pin decides OUT_OF_SCOPE.
- A wave hits an abrogate criterion (e-graph OOM, CSP > 1 s per
  grammar, stale cost > 30 % of candidate expressions) — propose
  abrogate-before-patch.
- A round-trip rule triggers (a row failed reopen twice in-tranche).
- The campaign genuinely cannot move any row in a tranche (campaign-
  level abrogate).

NEVER surface "looks hard, please confirm" — the campaign is
indefatigable. Assume continuation, dispatch the next wave, escalate
only on architectural-level blocks.

## Effect on the base SK-V13 implementation prompt

This addendum is in force from Phase 0. The base prompt's CLOSE,
ESCALATION, goalset, and wave-shortlist are OVERRIDDEN where they
conflict. Where they do not conflict, the base prompt stands. Read
this addendum back as part of the authority list at Phase 0 and again
at every Pass Alpha bracket.
