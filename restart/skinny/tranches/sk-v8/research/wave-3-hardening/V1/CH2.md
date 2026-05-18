# SK-V8 W3 Hardening V1 - CH2

Scope: HEAD `fc91c2173e8451dd06733381346bd800b0711f6e`
(`docs(sk-v8-wave3-plan): reject Tier A implementation on fit gate`), W3
research, W3 plan, SPEC Section 6, and the live Tier A code surfaces. I edited
only this review artifact.

Verdict: ACCEPT.

Confidence: 96%.

## Findings

1. The W3 plan correctly keeps Tier A narrow and then rejects implementation on
   the fit gate. SPEC Section 6 requires the stage-1 structural projection to
   become representation replacement inside one retained `Tape`, with generated
   JSON retained Track 1 parsing plus retained view/`ValueRef` as the
   same-wave consumer, and with scalar structural rediscovery removed
   (`restart/skinny/tranches/sk-v8/SPEC.md:542`-`restart/skinny/tranches/sk-v8/SPEC.md:575`).
   The plan states that same Tier A candidate and then returns
   reject/route before implementation because the current scanner index and
   retained tape are not event-isomorphic
   (`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:10`-`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:14`,
   `restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:31`-`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:67`).

2. The event-model mismatch is real in the live code. The scanner emits only a
   `StructuralIndex { positions, backend }` (`skinny/crates/bbnf-simd/src/lib.rs:71`-`skinny/crates/bbnf-simd/src/lib.rs:87`),
   JSON scan records punctuation plus real quotes
   (`skinny/crates/runtime/src/grammars/json/scan.rs:22`-`skinny/crates/runtime/src/grammars/json/scan.rs:35`,
   `skinny/crates/runtime/src/grammars/json/scan.rs:130`-`skinny/crates/runtime/src/grammars/json/scan.rs:160`),
   and `OneShotSimd` currently consumes only `positions().len() + 8` for
   reserve sizing (`skinny/crates/runtime/src/grammars/json/scan.rs:47`-`skinny/crates/runtime/src/grammars/json/scan.rs:52`).
   The generated attach hook is still a no-op
   (`skinny/crates/runtime/src/grammars/json/generated.rs:14`-`skinny/crates/runtime/src/grammars/json/generated.rs:17`).
   In contrast, retained parsing writes parser-event offsets through
   `TapeBuilder::push_plain_offset`
   (`skinny/crates/runtime/src/tape/assembler.rs:42`-`skinny/crates/runtime/src/tape/assembler.rs:85`,
   `skinny/crates/runtime/src/grammars/json/parser.rs:35`-`skinny/crates/runtime/src/grammars/json/parser.rs:37`):
   container opens/closes and opening quotes via `consume_structural` /
   `consume_quote_at_cursor`, plus number and literal starts in generated code
   (`skinny/crates/runtime/src/grammars/json/generated.rs:263`-`skinny/crates/runtime/src/grammars/json/generated.rs:306`,
   `skinny/crates/runtime/src/grammars/json/generated.rs:208`,
   `skinny/crates/runtime/src/grammars/json/generated.rs:233`).
   That confirms the research example: a raw structural-punctuation vector
   cannot be moved into the current tape without changing cursor semantics.

3. The plan correctly refuses the forbidden hybrid. SPEC says the projection is
   admissible only as replacement inside the singular retained `Tape`, and it
   fails if retained beside the old offset append path or if parser-owned
   cursor/fact slots survive
   (`restart/skinny/tranches/sk-v8/SPEC.md:550`-`restart/skinny/tranches/sk-v8/SPEC.md:563`).
   The current `Tape` is still one retained offset/flag/payload structure
   (`skinny/crates/runtime/src/tape/mod.rs:90`-`skinny/crates/runtime/src/tape/mod.rs:168`),
   while view/value traversal derives node identity from source bytes at tape
   cursors (`skinny/crates/runtime/src/grammars/json/value.rs:28`-`skinny/crates/runtime/src/grammars/json/value.rs:47`)
   and walks sibling structure by those cursors
   (`skinny/crates/runtime/src/grammars/json/view.rs:267`-`skinny/crates/runtime/src/grammars/json/view.rs:381`).
   A compliant Tier A implementation would have to delete the old append path
   and rewrite the generated parser plus view/value cursor contract in the same
   slice. The plan's listed owner surface and over-budget conclusion are
   therefore credible, not paper caution.

4. Same-wave consumer discipline is preserved. The plan does not nominate
   `tape_vs_tape`, `simd_structural_scan`, Track 2, comparator rows, or
   retained-view-only checks as production consumers; it requires a future
   valid wave to make generated JSON retained Track 1 parsing consume retained
   tape positions/classes in measured rows, with retained view/`ValueRef` parity
   in the same slice
   (`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:69`-`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:76`).
   That matches SPEC's consumer rule and prevents telemetry substitution
   (`restart/skinny/tranches/sk-v8/SPEC.md:564`-`restart/skinny/tranches/sk-v8/SPEC.md:575`).

5. The plan keeps pre-blocked routes closed. It explicitly does not reopen
   REDRESS 50-55 cursor/aux/projection side tables, REDRESS 60-72 direct and
   materialization families, REDRESS 82-84/88-89 reserve routes, `UnionTape`,
   public substrate API, sidecar substrate, parser-owned cursor/facts, parallel
   substrate, or Tier B string-boundary / quote-backslash-parity /
   CostFacts-template claims
   (`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:78`-`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:92`).
   This is consistent with S-P2 V7 convergence boundaries: S-P2 converged as
   research and did not authorize W3 implementation, sidecar substrate,
   public API, parser-owned cursor/facts, Track 1/Track 2 coupling, or
   `tape_vs_tape` as W3 consumer
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:12`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md:64`).

6. The redress route is the right one. The W3 research says the admissible
   shape remains one retained substrate where scanner output becomes the
   production tape and generated retained parsing consumes it, but the current
   SK-V8 slice must first split the event grammar, scalar-span facts, and
   `ValueRef` contract into a precursor
   (`restart/skinny/tranches/sk-v8/research/skv8-W3-tape-structural-research.md:100`-`restart/skinny/tranches/sk-v8/research/skv8-W3-tape-structural-research.md:114`).
   The plan routes exactly that: add REDRESS for the scanner/tape event-model
   mismatch, update HANDOFF to mark W3 rejected/routed and unblock W4, and feed
   SK-V9/Pass Omega with a split precursor
   (`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:94`-`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md:105`).

## Verification

- `git rev-parse HEAD` returned
  `fc91c2173e8451dd06733381346bd800b0711f6e`.
- `git show --stat --oneline --decorate --no-renames fc91c217` showed only
  `restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md` changed at HEAD.
- `git status --short` was clean before this CH2 artifact was created.
- `cargo test -p bbnf-bench offset_stream_tracks_verified_source_events -- --nocapture`
  passed from `skinny/`: 1 matching library test passed, 0 failed.
- `cargo test -p bbnf-bench counts_json_lazy_tape_materialization_shape -- --nocapture`
  passed from `skinny/`: 1 matching library test passed, 0 failed.
- `git diff --exit-code HEAD -- skinny/RESULTS.md` exited 0.
- `git diff --check` exited 0 before this artifact was written.
- `git diff --no-index --check /dev/null restart/skinny/tranches/sk-v8/research/wave-3-hardening/V1/CH2.md`
  exited 0 after this artifact was written.

## Required Folds

1. No W3 source implementation is authorized by this CH2 ACCEPT. Fold the
   result as acceptance of the plan's reject/route decision, not as approval to
   patch SIMD, tape, generated parser, view/value, codegen templates, bench
   gates, or RESULTS.

2. The redress fold must record the scanner/tape event-model mismatch and the
   failed W3 fit gate in `skinny/REDRESS.md`, then update `HANDOFF.md` to mark
   W3 rejected/routed and unblock W4 only under the existing downstream rules.

3. The future SK-V9/Pass Omega precursor must define the retained event/class
   grammar, including numbers, literals, and string quote ownership; prove the
   retained `ValueRef` cursor contract over that grammar; and only then measure
   structural-heavy parse rows in a later wave.

4. Preserve the Tier A exclusions in consolidation: no old append-path hybrid,
   no sidecar/parser-owned cursor or facts, no `UnionTape`, no public or
   parallel substrate, no `tape_vs_tape`/Track 2/comparator row as production
   consumer, and no Tier B string-boundary, quote/backslash/parity, density, or
   CostFacts-template claims inside W3.
