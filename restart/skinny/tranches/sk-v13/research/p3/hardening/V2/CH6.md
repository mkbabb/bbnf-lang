# SK-V13 S-P3 V2 CH6 Anti-Paper-Close Challenge

Lens: CH6 anti-paper-close.
Commit under review: `9f8bbfce5`.
Disposition vocabulary: ACCEPT / REVISE / REJECT.

## Verdict

ACCEPT.

V2 folds the V1 CH6 revise set. W5-W8 no longer close on support plumbing:
each decision/policy section now requires a named consumer plus row movement,
row admission, or a measured architectural block. W10.N, W11.N, W13, and W14.N
now name explicit same-wave production consumers in both SPEC and DISPATCH.
The G-Omega block, strict comparator gate, no-fixpoint close rule, no silent
demotion rule, and no future-consumer/paper-close language remain load-bearing.

## Contract Citations

- PASS-3 defines CH6 as the check that waves close on measurement, not a
  future-phase promise, and asks whether same-wave consumers are named
  (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:140`-`:145`).
- PASS-3 also requires P3-A candidates to carry same-wave consumers and
  falsifiability gates, P3-C to write measurable gates, and P3-F to fold the
  manifest, LOC budgets, falsifiability gates, and pre-blocked routes into SPEC
  and DISPATCH (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:58`-`:63`).
- SKINNY-TRIUMVIRATE makes the same-wave consumer rule load-bearing: a primitive,
  kernel, or generated path must include the hot-path caller, bench the named
  rows, and reject as an orphan if the consumer wire-up is omitted
  (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:177`-`:186`).
- ORCHESTRATOR keeps same-wave consumer, strict-vs-strict comparator, role
  separation, and no-deferral/no-future-promise as CHALLENGE non-negotiables
  (`restart/prompts/ORCHESTRATOR.md:197`-`:211`).

## Fold Items

1. **W5-W8 cannot close on support plumbing: folded.**

   V1 required row-movement or architectural-block exit clauses for W5, W6, W7,
   and W8, with W8 losing "unchanged or improved" as admission evidence
   (`restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH6.md:85`-`:90`).
   SPEC now requires W5 regex extraction to be consumed by a named generated
   selection path and either move/admit a JSON or CSS row or record a measured
   architectural block; support-only extraction rejects
   (`restart/skinny/tranches/sk-v13/SPEC.md:586`-`:595`). W6 applies the same
   rule to generated backend selection and rejects bounded e-graph/cost
   telemetry alone (`restart/skinny/tranches/sk-v13/SPEC.md:627`-`:635`). W7
   requires `compile()` / backend consumption and makes cascade retirement
   without a row-consumed result a measured reject
   (`restart/skinny/tranches/sk-v13/SPEC.md:664`-`:672`). W8 requires a touched
   JSON/CSS row to consume the policy surface and move/admit/block; JSON output
   unchanged is guard evidence only (`restart/skinny/tranches/sk-v13/SPEC.md:689`-`:707`).
   DISPATCH mirrors the rule across W5-W8 and explicitly rejects API extraction,
   e-graph/cost telemetry, CSP plumbing, cascade retirement, or policy wiring
   without row movement/block (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:196`-`:201`).

2. **Every row-family subwave has an explicit same-wave consumer: folded.**

   V1 required `Same-wave consumer:` lines for W10.N, W11.N, W13, and W14.N
   (`restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH6.md:92`-`:99`).
   SPEC now names the generated CSS feature row plus production fact-stream caller
   for W10.N (`restart/skinny/tranches/sk-v13/SPEC.md:776`-`:781`), the generated
   JSON direct sink/digest path exercised by `direct_struct.rs` for W11.N
   (`restart/skinny/tranches/sk-v13/SPEC.md:812`-`:817`), the generated typed
   product parser plus independent Track 2/oracle harness for W13
   (`restart/skinny/tranches/sk-v13/SPEC.md:877`-`:883`), and the generated JSON
   parse path or selected parse runtime caller exercised by `bbnf-bench`
   `parse_only` for W14.N (`restart/skinny/tranches/sk-v13/SPEC.md:914`-`:919`).
   DISPATCH mirrors the same minimum consumer table
   (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:203`-`:210`).

3. **No G-Omega bypass: preserved.**

   SPEC forbids W0-or-later source, generated runtime, gate/report, RESULTS, or
   REDRESS edits until G-Omega and S-P3 convergence or user pin
   (`restart/skinny/tranches/sk-v13/SPEC.md:32`-`:43`). The global close
   condition keeps G-Omega before W0 and requires totality V1.1 CRUD under
   G-Omega authority (`restart/skinny/tranches/sk-v13/SPEC.md:51`-`:73`).
   Section 21 restates that G-Omega must close before W0 and that only planning
   research plus read-only RESULTS/REDRESS inspection are allowed until both
   gates close (`restart/skinny/tranches/sk-v13/SPEC.md:990`-`:1004`). DISPATCH
   requires G-Omega user sign-off and totality V1.1 CRUD before Wave 0+
   (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:38`-`:40`).

4. **Strict comparator, no-fixpoint, no-demotion, and no-future-promise language:
   preserved.**

   SPEC keeps strict same-run anchors as the only admission comparators and keeps
   flaw probes/sidecars out of strict admission
   (`restart/skinny/tranches/sk-v13/SPEC.md:84`-`:90`). It rejects producer-only
   telemetry, paper close, rolling demotion, and pre-G-Omega implementation
   telemetry (`restart/skinny/tranches/sk-v13/SPEC.md:240`-`:246`). The global
   non-negotiables reject any producer without a same-wave measured consumer,
   any close on "wired", "integrated", "scaffolded", "future consumer", or
   "paper close" language, and any support-only behavior wave
   (`restart/skinny/tranches/sk-v13/SPEC.md:294`-`:306`). Close still rejects
   ordinary fixpoint, implementation-limited miss, one-CSS-row close, and
   REDRESS-history close (`restart/skinny/tranches/sk-v13/SPEC.md:78`-`:82`,
   `restart/skinny/tranches/sk-v13/SPEC.md:975`-`:986`).

5. **SIMD zero-orphan and no later cleanup dependency: preserved.**

   The V1 consolidation required same-wave orphan accounting and barred W9/C3
   from relying on W12 for later cleanup
   (`restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:75`-`:78`).
   DISPATCH now requires any `bbnf-simd` or SIMD-generated consumer wave,
   including W9/C3, to exit with `orphan_count_after = 0`, strict checkasm,
   scalar-reference status, delete/demote/revert protocol, and production
   consumer row evidence in the same wave; later W12 cleanup is not admissible
   (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:212`-`:217`). SPEC adds
   the same W9 predicate (`restart/skinny/tranches/sk-v13/SPEC.md:745`-`:751`)
   and W12 zero-orphan/checkasm/row-movement exit gates
   (`restart/skinny/tranches/sk-v13/SPEC.md:841`-`:853`).

## Evidence From P3-A Through P3-F

- P3-A reclassifies P3A-0 as governance substrate and makes P3A-1 through P3A-7
  traceable to same-wave consumers and strict thresholds. P3A-2 rejects crate or
  e-graph scaffold without a resolver-selected JSON/CSS row, P3A-3 names JSON
  and CSS row consumers, P3A-4 rejects generated helpers without row movement or
  architectural-block evidence, and P3A-7 requires a production CSS consumer or
  deletion/demotion with zero orphans
  (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:72`-`:79`).
- P3-B states the folded SPEC/DISPATCH W0-W15 names are canonical and that each
  real subwave counts against the active bracket; overflow brackets forward
  without dropping pinned rows (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:10`-`:18`).
- P3-C rejects support-only/future-consumer work, defines strict same-plane
  comparator formulas, and makes decision-engine subwaves reject unless improved
  rows pass `json_admit` or `row_move_toward_sota`
  (`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:13`-`:18`,
  `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:28`-`:39`,
  `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:217`-`:237`).
- P3-D makes producer-only telemetry and SIMD orphans gate-json rejection classes
  (`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:276`-`:284`).
- P3-E binds CH6 by rejecting microbench, proof-only extraction, test-harness
  hardening, generated surfaces, and "wired/integrated" states as exit gates
  unless the exit names rows, strict comparator planes, thresholds, same-wave
  consumer, REDRESS differential, and revert protocol
  (`restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:180`-`:186`).
- P3-F carries the V2 fold scope from P3-A through P3-E and repeats the
  no-scaffold, same-wave consumer, strict checkasm, row movement/measured
  rejection requirements (`restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md:27`-`:35`,
  `restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md:104`-`:126`).

## Residual Risk

No CH6-blocking residual remains. The remaining "later" wording I found is
planning or schema-sequencing language, not close authority: the controlling
SPEC/DISPATCH rules reject future consumers, require same-wave measured
consumers, and make row-family subwaves explicit.

## Local Check

- `git diff --check -- restart/skinny/tranches/sk-v13/research/p3/hardening/V2/CH6.md`
  (PASS).
