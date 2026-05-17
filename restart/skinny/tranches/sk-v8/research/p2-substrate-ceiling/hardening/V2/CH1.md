# S-P2 V2 CH1 Correctness Review

Role: CH1 (Correctness)

Verdict: REVISE

Score: 82/100

## Blocking Findings

1. Comparator delta columns are still misread as sonic-strict evidence.

   `skinny/RESULTS.md:3` defines the post-comparator delta columns as
   `Delta vs SK-V6`, `Delta vs sonic-strict`, `Delta vs simdjson DOM`, then
   `Delta vs yyjson`. The folded text still shifts several values one column
   to the right. For example, the twitter row has `-25.1%` vs sonic-strict and
   `-35.8%` vs simdjson DOM (`skinny/RESULTS.md:5`), but the synthesis calls
   twitter `-35.8% vs sonic strict`
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:130`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:133`)
   and SC-5 repeats `-35.8% vs sonic strict`
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:115`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:116`).
   The same error appears in the string-density table: SC-4 labels citm as
   `LOSS` at `-11.3%` under `Delta vs sonic (parse)`
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:184`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:191`),
   but the authoritative citm row is `+24.6%` vs sonic-strict and `-11.3%` vs
   simdjson DOM (`skinny/RESULTS.md:8`). SC-5 also says citm's `-11.3%` is
   compared to the `25509` sonic anchor, then calls `+24.6%` a SK-V6 delta
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:42`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md:47`),
   which reverses the actual columns. This violates the strict-vs-strict
   comparator gate from `restart/prompts/ORCHESTRATOR.md:83` and
   `restart/prompts/skinny/PASS-2-RESEARCH.md:214`-`restart/prompts/skinny/PASS-2-RESEARCH.md:219`.

2. The SC-4 string-plane argument still overclaims sonic-rs architecture and
   conflicts with the folded SC-2 comparator caution.

   SC-2 explicitly demotes comparator architecture claims without exact
   upstream anchors and says sonic-rs should be treated as having no persistent
   document-wide structural index unless later source proof says otherwise
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:28`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:43`).
   SC-2's sonic-rs section then describes a single-pass skip-scan model with
   per-token SIMD work consumed immediately, never written to an index array
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:148`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:164`).
   SC-4 still says "simdjson Lemire/Langdale 2019; sonic-rs is a Rust port of
   the same shape" and attributes a whole-document Stage 1 structural index to
   that combined comparator class
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:116`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:131`).
   It repeats that "simdjson/sonic compute the quote bitmap once, branchlessly,
   for the whole document"
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:247`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md:255`).
   That is not just imprecise wording: it is the mechanism used to justify the
   substrate-ceiling claim, and it contradicts the folded SC-2 treatment.

## Non-Blocking Notes

- The W3 lead-hypothesis language is mostly folded correctly. SYNTHESIS says
  the tape/structural-projection union is not selected by S-P2 and still needs
  W0/W1 closure, S-P3/W3 planning, owner paths, same-wave consumer, thresholds,
  and challenge acceptance
  (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:159`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:167`).
  SPEC repeats that nomination does not select W3 and that `tape_vs_tape`
  telemetry is not a production same-wave consumer
  (`restart/skinny/tranches/sk-v8/SPEC.md:405`-`restart/skinny/tranches/sk-v8/SPEC.md:410`).

- The Lock 14 StructuralAlphabet repair is substantially folded. SC-6 now uses
  fixed neutral roles, codegen-emitted per-grammar data, and generated grammar
  modules as the only interpretation point
  (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:339`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:360`,
  `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:486`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:506`).

- The single retained Tape constraint is clear enough for this lens. SC-3
  states one retained `Tape`, move-consumed `StructuralIndex`, and no post-build
  query/cache/attach API
  (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:115`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:120`,
  `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:352`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:357`),
  and SPEC rejects a retained projection added beside the existing tape
  (`restart/skinny/tranches/sk-v8/SPEC.md:430`-`restart/skinny/tranches/sk-v8/SPEC.md:436`).

- The packet does not dispatch an implementation wave. SPEC says no SK-V8
  implementation wave dispatches before G-Alpha and even a closed G-Alpha
  authorizes only W0
  (`restart/skinny/tranches/sk-v8/SPEC.md:171`-`restart/skinny/tranches/sk-v8/SPEC.md:172`,
  `restart/skinny/tranches/sk-v8/SPEC.md:536`-`restart/skinny/tranches/sk-v8/SPEC.md:540`).

## Required Fold Actions

1. Recompute every SC-4, SC-5, and SYNTHESIS comparator delta against the
   `skinny/RESULTS.md:3` column order. Use `Delta vs sonic-strict` only for
   same-run strict sonic claims; move simdjson and yyjson deltas into sidecar
   planning-signal language per `skinny/RESULTS.md:219` and
   `restart/skinny/tranches/sk-v8/SPEC.md:44`-`restart/skinny/tranches/sk-v8/SPEC.md:55`.

2. Rebuild the SC-4 string-density table after the delta correction. If the
   threshold still holds, state it as a planning signal with the corrected
   strict/sidecar split. If it depends on simdjson DOM or yyjson sidecars, do
   not label it a strict sonic threshold.

3. Rewrite SC-4's comparator-architecture paragraph to separate simdjson's
   document-wide stage-1 index from sonic-rs's skip-scan/single-pass model, or
   add exact upstream source anchors proving the stronger sonic-rs claim. Until
   then, SC-4 may cite sonic-rs only as a strict performance anchor and a local
   skip-scan comparator, not as a persistent-index two-stage architecture.

4. After those folds, rerun CH1. Do not dispatch S-P3 or any implementation
   wave from this V2 state.
