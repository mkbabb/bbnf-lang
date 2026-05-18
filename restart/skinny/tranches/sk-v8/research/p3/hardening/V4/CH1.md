# SK-V8 S-P3 Hardening V4 CH1: Correctness, Links, And No-Paper-Close

Scope: V4-only CH1 review of the live S-P3 packet after
`restart/skinny/tranches/sk-v8/research/p3/p3-v4-exact-traceability-fold.md`.
Checked correctness, exact citation repair from V3, local doc/file link
integrity, falsifiability gates, strict comparator discipline, and the
G-Alpha/W0-only dispatch lock. No SK-V8 implementation wave was reviewed as
dispatched or implemented.

## Verdict

ACCEPT. Confidence: 96.

## Blockers

None.

## Evidence

- V3's blocking requirement was narrow and is folded. The V3 consolidation
  required replacing broad SPEC/HANDOFF bundles with exact named sections and
  replacing generic RESULTS/REDRESS placeholders with rows, entries, or live
  line spans (`restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:33-48`).
  V4 records the fold as exact SPEC/HANDOFF labels and current RESULTS/REDRESS
  anchors (`restart/skinny/tranches/sk-v8/research/p3/p3-v4-exact-traceability-fold.md:19-27`).

- Exact SPEC labels resolve to live headings. The labels used by P3-A through
  P3-F match the live SPEC headings for Section 0.1 through Section 11
  (`restart/skinny/tranches/sk-v8/SPEC.md:40`,
  `restart/skinny/tranches/sk-v8/SPEC.md:61`,
  `restart/skinny/tranches/sk-v8/SPEC.md:79`,
  `restart/skinny/tranches/sk-v8/SPEC.md:99`,
  `restart/skinny/tranches/sk-v8/SPEC.md:144`,
  `restart/skinny/tranches/sk-v8/SPEC.md:230`,
  `restart/skinny/tranches/sk-v8/SPEC.md:253`,
  `restart/skinny/tranches/sk-v8/SPEC.md:300`,
  `restart/skinny/tranches/sk-v8/SPEC.md:327`,
  `restart/skinny/tranches/sk-v8/SPEC.md:385`,
  `restart/skinny/tranches/sk-v8/SPEC.md:442`,
  `restart/skinny/tranches/sk-v8/SPEC.md:506`,
  `restart/skinny/tranches/sk-v8/SPEC.md:605`,
  `restart/skinny/tranches/sk-v8/SPEC.md:663`,
  `restart/skinny/tranches/sk-v8/SPEC.md:715`,
  `restart/skinny/tranches/sk-v8/SPEC.md:767`,
  `restart/skinny/tranches/sk-v8/SPEC.md:814`). P3-F's scope/output list uses
  those exact labels (`restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:5-10`).

- Exact HANDOFF labels resolve to live headings. The cited handoff labels match
  live sections for current measured state, substrate-ceiling finding, dispatch
  posture, entry gates, exit condition, pre-blocked routes, and G-Alpha decision
  (`restart/skinny/tranches/sk-v8/HANDOFF.md:24`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:56`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:98`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:119`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:151`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:191`). P3-C uses those labels to
  bind dispatch, entry, and close posture (`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:16`).

- The rejected broad citation bundles are absent from material live P3 claims.
  Search found no remaining live citation use of `SPEC Sections 0.1-0.5, 2,
  and 3-11`, `HANDOFF Sections 2, 3a, and 4-10`, generic RESULTS current-row
  placeholders, or generic REDRESS named-row placeholders in P3-A through P3-F.
  The only remaining references to broad bundles are retrospective descriptions
  of the rejected V3 shape (`restart/skinny/tranches/sk-v8/research/p3/p3-v4-exact-traceability-fold.md:21-27`;
  `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:83-88`).

- Local doc/file links resolve. A mechanical path check over live SPEC,
  HANDOFF, DISPATCH, P3-A through P3-F, and the V4 fold found no missing
  backticked local paths under `restart/`, `skinny/`, `docs/`, or
  `audit-specs/`. The checked file:line anchors were within range.

- The G-Alpha/W0-only lock is intact. SPEC states S-P3 itself dispatches no
  implementation, G-Alpha user signoff is required before any SK-V8 wave, and
  `G-Alpha closed` initially dispatches W0 only
  (`restart/skinny/tranches/sk-v8/SPEC.md:31-36`); Section 11 repeats that W0
  is the only authorized post-G-Alpha dispatch and that no W3 implementation
  dispatches from S-P2 or S-P3 alone
  (`restart/skinny/tranches/sk-v8/SPEC.md:814-825`). DISPATCH preserves the
  same rule (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6-9`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:211-222`), and HANDOFF
  agrees (`restart/skinny/tranches/sk-v8/HANDOFF.md:5-7`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:191-198`).

- All gates remain falsifiable, not prose-only. PASS-3 requires named corpus
  rows, concrete Mbps thresholds, `SK-V8-open` comparison, and strict-plane
  comparator deltas for CH1 (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:110-115`).
  P3-A summarizes row-level W0-W6 signals (`restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md:42-50`).
  P3-C binds entry gates, numeric thresholds, full-table maintain budgets,
  negative gates, and revert protocol for W0 through W6
  (`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:36-64`,
  `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:66-92`,
  `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:94-148`,
  `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:150-199`,
  `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:201-237`,
  `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:239-296`,
  `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:300-314`).

- Strict-vs-strict and no-paper-close discipline survived V4. SPEC requires
  strict admission to use same-run strict anchors on a matching output plane and
  treats lossy/permissive/sidecar evidence as planning only
  (`restart/skinny/tranches/sk-v8/SPEC.md:61-77`). P3-D gives executable
  strict-admission refusal predicates (`restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md:119-131`).
  SPEC forbids waves closing on "wired", "advisory", "future consumer",
  "integrated", or paper-close language without measured evidence
  (`restart/skinny/tranches/sk-v8/SPEC.md:250-251`), and P3-C/P3-E preserve the
  same miss-to-REDRESS and no-deferral rule
  (`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:32`,
  `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:18`,
  `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md:53`).

## Residual Non-Blocking Risks

- W3 Tier A remains cost-sensitive. V4 keeps the split/REVISE gate if the exact
  plan cannot fit scalar oracle, checkasm, production consumer, generated audit,
  measurement, reporting, and revert work inside 90 minutes
  (`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:152-158`;
  `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md:90-101`).

- Seed Mbps floors remain planning values. V4 correctly requires W2/W3/W4
  thresholds to be recomputed from `SK-V8-open` when W0 refreshes same-run
  anchors or bbnf row values
  (`restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md:312`).

- The P3-F shorthand `DISPATCH sections Wave Manifest, Conditional Gates, and
  Entry Condition` is understandable against live DISPATCH headings, but
  "Conditional Gates" is a shorthand for `Conditional Wave Gates`; it is not a
  SPEC/HANDOFF label and is not blocking for CH1
  (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:33`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:90`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:211`).

## Required Fold If REVISE

None.
