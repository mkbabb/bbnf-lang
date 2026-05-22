# SK-V13 S-P3 V2 CH5: Hidden Coupling

Commit under review: `9f8bbfce5`.

Verdict: ACCEPT.

Lens: verify the S-P3 V1 CH5 fold landed and no hidden coupling remains:
P3-B/C/D/E binding, owner-overlap and ledger serialization visibility,
two-witness Lock 14, same-wave zero-orphan for `bbnf-simd` including W9/C3, no
sidecar substrate/API expansion, and no Track 1/Track 2 dishonesty.

## Fold Items

1. P3-B/C/D/E are binding now. SPEC lists P3-A through P3-E and the V1
   consolidation in the authority set (`SPEC.md:17`-`:23`), and DISPATCH repeats
   those inputs plus ownership of cost/dependencies, formulas, telemetry, and the
   REDRESS route-state ledger (`DISPATCH-PROMPT.md:17`-`:34`). P3-F also states
   that P3-A through P3-E are present and binding for the V2 fold, with P3-B
   W0-W11 labels demoted to packing aliases under the folded W0-W15 plan
   (`p3f-spec-draft.md:27`-`:35`). This satisfies V1 CH5 item 3.

2. Owner overlap and ledger serialization are visible. P3-B says packed
   subwaves are legal only with disjoint file domains and serialized ledger
   writes (`p3b-wave-sequencing.md:22`-`:33`), then binds a single finalizer for
   `RESULTS.md`, `REDRESS.md`, and `ROLLING-SOTA-DELTA.md`
   (`p3b-wave-sequencing.md:217`-`:227`). SPEC requires W10.N subwaves to prove
   owner-path non-overlap and safe RESULTS/REDRESS serialization before
   concurrent dispatch (`SPEC.md:767`-`:772`), while DISPATCH mirrors this rule
   for W10.N/W11.N/W14.N (`DISPATCH-PROMPT.md:184`-`:186`). No hidden
   concurrent ledger writer remains.

3. The weak Lock 14 proof was replaced. SPEC now requires CSS L4 plus both
   Sheets and BBNF-self witnesses for fleet-wide grammar-neutral claims, with
   CSS plus only one non-CSS witness scoped to named grammars
   (`SPEC.md:384`-`:390`). DISPATCH carries the same two-witness rule
   (`DISPATCH-PROMPT.md:142`-`:146`), and P3-A marks fleet-wide claims
   conditional on this rule (`p3a-candidate-shortlist.md:10`-`:15`). This folds
   V1 CH5 item 1 without hiding the residual JSON-policy leak inventory that
   motivated the stricter rule (`sk-v13-scoping-value-api-union.md:63`-`:71`,
   `:113`-`:117`).

4. Same-wave zero-orphan now covers every `bbnf-simd` touch, including W9/C3.
   DISPATCH says any wave touching `skinny/crates/bbnf-simd/` or selecting a
   SIMD-generated consumer, explicitly including W9/C3, must exit with
   `orphan_count_after = 0`, strict checkasm status, scalar-reference status,
   delete/demote/revert protocol, and same-wave production consumer evidence
   (`DISPATCH-PROMPT.md:212`-`:217`). SPEC W9 repeats the predicate for
   `bbnf-simd` or C3 SIMD-first routing and forbids reliance on W12 cleanup
   (`SPEC.md:724`-`:751`). P3-C defines the same zero-orphan gate and preserves
   the REDRESS-126 demotion history (`p3c-falsifiability-gates.md:294`-`:334`).
   This folds V1 CH5 item 2.

5. No sidecar substrate or public substrate/API expansion remains authorized.
   SPEC non-negotiables forbid public substrate API, `UnionTape`, parallel
   substrate, parser-owned structural cursor, aux density table, retained class
   side vector, sidecar event vector, and second source scanner
   (`SPEC.md:287`-`:292`). SPEC W9 preserves single-substrate ownership and
   rejects class columns, retained structural indexes, parser-owned cursors,
   aux tables, sidecar vectors, second scans, and public `UnionTape`
   (`SPEC.md:736`-`:746`). P3-C and P3-D make the same substrate/cardinality
   constraints gate-consumed rather than prose (`p3c-falsifiability-gates.md:273`-`:278`,
   `p3d-telemetry-schema.md:198`-`:207`). The route ledger keeps exact
   sidecar/cursor/API replays blocked (`p3e-preblocked-ledger.md:60`-`:87`,
   `SPEC.md:953`-`:986`).

6. Track 1 / Track 2 honesty holds. P3-C states JSON Track 2 is
   correctness/independence evidence only: it cannot be the SOTA anchor, cannot
   call Track 1, and cannot hide Track 1 demotion
   (`p3c-falsifiability-gates.md:35`-`:39`). P3-D requires Track 2 to remain
   independent or explicitly untouched, and rejects Track 2, `tape_vs_tape`, or
   gate-only telemetry as a production consumer (`p3d-telemetry-schema.md:151`-`:152`,
   `:198`-`:207`). SPEC W13 likewise requires a generated Track 1 path plus an
   independent Track 2/oracle harness (`SPEC.md:866`-`:883`).

## Evidence

- Reviewed PASS-3 S-P3 CH5 contract: CH5 rejects parallel substrates, sidecar
  producers, renamed scanners, retained cursors/projections, and Track 1 equals
  Track 2 dishonesty (`PASS-3-SYNTHESIS-PLAN.md:134`-`:145`).
- Reviewed ORCHESTRATOR universal CH5 and convergence rules: CH5 requires
  substrate union and no hidden coupling, while V1 REVISE dispositions must fold
  before advancement (`ORCHESTRATOR.md:81`-`:88`, `:104`-`:123`).
- Reviewed V1 CH5 and V1 consolidation. The V1 blockers were exactly the weak
  Lock 14 witness rule, missing W9/C3 same-wave zero-orphan predicate, and
  non-binding P3-B/C/D/E in DISPATCH/P3-F
  (`restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH5.md:13`-`:163`;
  `restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:57`-`:88`).
- Cross-checked SPEC, DISPATCH, P3-A through P3-F, and the relevant scoping
  audits. The V2 fold preserves one-substrate/no-sidecar/no-public-API language,
  binds P3-B/C/D/E, serializes ledgers, upgrades Lock 14 witness cardinality, and
  makes same-wave zero-orphan mandatory for all `bbnf-simd` paths.

No CH5 fold items remain open for V2.
