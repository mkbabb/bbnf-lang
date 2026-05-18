# CH3 W0 V5 Hardening Challenge - Lock 14 Freeze

## Verdict

ACCEPT, confidence 96%.

Target `0c49fabd6d6facd136e1e69b8482aa4f239561ae` folds the V4 CH1/CH4
blockers without reopening a CH3 regression route. I tried to reject on Lock 14
grammar-neutrality, frozen roots, new directive/BIR/substrate/asm/build drift,
and fixture-scoped fingerprinting as a possible generic JSON policy leak. I did
not find a material blocker.

This is a CH3 verdict only. W0 still needs the full V5 challenge consolidation
and ORCHESTRATOR convergence handling before any downstream wave can dispatch
(`restart/prompts/ORCHESTRATOR.md:74`, `restart/prompts/ORCHESTRATOR.md:84`,
`restart/prompts/ORCHESTRATOR.md:104`, `restart/prompts/ORCHESTRATOR.md:118`).

## Scope

Reviewed current folded W0 at `0c49fabd` after the V4 rejection/fold. The lens
was deliberately narrow:

- Lock 14 grammar neutrality and non-JSON proof expectations
  (`restart/skinny/tranches/sk-v8/SPEC.md:261`,
  `restart/skinny/tranches/sk-v8/SPEC.md:266`,
  `restart/skinny/tranches/sk-v8/SPEC.md:279`,
  `restart/skinny/tranches/sk-v8/SPEC.md:284`).
- W0 telemetry-only authority and W0 owner/freeze boundaries
  (`restart/skinny/tranches/sk-v8/SPEC.md:288`,
  `restart/skinny/tranches/sk-v8/SPEC.md:290`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:333`,
  `restart/skinny/tranches/sk-v8/SPEC.md:336`,
  `restart/skinny/tranches/sk-v8/SPEC.md:339`).
- Frozen grammar/runtime/tape/codegen/IR/passes/grammar/parser/direct/typed/SIMD
  source surfaces, and no new directive/BIR/substrate/asm/build route
  (`restart/skinny/tranches/sk-v8/SPEC.md:191`,
  `restart/skinny/tranches/sk-v8/SPEC.md:193`,
  `restart/skinny/tranches/sk-v8/SPEC.md:206`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:80`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:85`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:173`).
- V4 rejection requirements for deferred-row semantic consumption and
  validated-fixture fingerprinting
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V4/HARDENING-W0-V4-CONSOLIDATED.md:20`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V4/HARDENING-W0-V4-CONSOLIDATED.md:29`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V4/HARDENING-W0-V4-CONSOLIDATED.md:48`).

## Evidence

- Target diff scope is only two W0 bench/report files: `git show --name-status
  --oneline --no-renames 0c49fabd^..0c49fabd` reports changes only in
  `skinny/crates/bbnf-bench/src/bin/gate.rs` and
  `skinny/crates/bbnf-bench/src/report.rs`.
- Frozen parent diff is empty: `git diff --name-only 0c49fabd^ 0c49fabd --`
  over `skinny/grammars`, `skinny/test_data`, `skinny/crates/test-fixtures`,
  runtime, IR, passes, codegen, grammar, parser, SIMD/build/ext, direct, typed,
  parity, scan, materialization, and `xtask/src/real_typed_schema.rs` produced
  no paths.
- Frozen current worktree diff is also empty on the same path set. `git status
  --short` was clean before writing this artifact.
- `cargo test -p bbnf-bench --profile ax-iter` passed 52 library tests, 8
  `gate` binary tests, and doc tests. The passing set includes
  `lock14_baseline::tests::frozen_roots_cover_directive_and_asm_surfaces`,
  `report::tests::w0_rejects_deferred_validation_semantic_drift`, and
  `tests::w0_criterion_fingerprint_excludes_derendered_probe_estimates`.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
  xtask gate-json --advisory --check-results` exited 0 against the committed W0
  Criterion evidence.
- Focused copied-root fingerprint probe exited 0 after adding
  `json_unvalidated_future/track1_generated/new/estimates.json` to a temp copy
  of `/tmp/skv8-w0-target/criterion`; the rendered run id stayed
  `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`.
- `cargo xtask check-json`, `cargo xtask check-real-typed`, and `cargo xtask
  check-conformance` passed; conformance reported 21 valid fixtures accepted and
  7 invalid fixtures rejected.
- `git diff --check 0c49fabd^ 0c49fabd` exited 0.

Source support:

- `gate-json` calls the Lock 14 baseline validator before fixture/report work
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:42`), then derives a fixture-name set
  from `test_fixtures::load_available_bench_fixtures()`
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:45`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:46`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:50`).
- The V5 fingerprint is fixture-scoped, not any `json_*` path. `RunFacts::probe`
  passes fixture names into `criterion_fingerprint`
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:384`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:390`), and
  `is_w0_criterion_input` accepts `json_<corpus>` only when the stripped corpus is
  in that set (`skinny/crates/bbnf-bench/src/bin/gate.rs:735`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:747`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:749`).
- The focused unit test covers both de-rendered probe exclusion and the V4
  `json_unvalidated_future` failure case
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:1765`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1777`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1784`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1788`).
- Deferred-row telemetry is now semantically consumed before W0 accepts a
  non-strict row: strictness must remain `deferred`, validation path
  `view-boundary`, `parse_utf8=view-boundary`, and `escape_complete=yes`
  (`skinny/crates/bbnf-bench/src/report.rs:920`,
  `skinny/crates/bbnf-bench/src/report.rs:923`,
  `skinny/crates/bbnf-bench/src/report.rs:936`,
  `skinny/crates/bbnf-bench/src/report.rs:942`). The new negative test mutates
  `parse_utf8` and `escape_complete` and expects rejection
  (`skinny/crates/bbnf-bench/src/report.rs:1639`,
  `skinny/crates/bbnf-bench/src/report.rs:1657`,
  `skinny/crates/bbnf-bench/src/report.rs:1661`).
- The Lock 14 validator marks W0 report/gate files as `bench_gate_schema` /
  `telemetry_only`, not generic runtime/codegen policy
  (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:267`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:273`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:279`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:285`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:291`).
- The same validator freezes grammar, test data, runtime, IR, passes, codegen,
  grammar/parser, SIMD/build/ext, direct, typed, parity, scan, materialization,
  and real-typed schema roots (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:403`) and rejects
  `BackendShape`/`UnionTape` drift
  (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:462`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:485`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:488`).

## Findings

1. P0 blocker: none found. The V5 fold does not touch frozen grammar/runtime/
   tape/codegen/IR/passes/grammar/parser/direct/typed/SIMD roots in the parent
   diff or current worktree, and the live gate checks those roots before report
   generation.

2. P1 blocker: none found in fixture-scoped fingerprinting. The fingerprint still
   contains JSON fixture names, but only inside `bbnf-bench` W0 gate/report
   telemetry. It is bounded by the validated fixture set and W0 bench names, and
   the copied-root `json_unvalidated_future` probe shows no generic JSON policy
   leak or unrelated Criterion churn.

3. P1 blocker: none found in deferred validation semantics. The V4 CH1 failure
   is folded: W0 now rejects non-strict row drift from `parse_utf8=view-boundary`
   or `escape_complete=yes`, with an executable negative test.

4. P2 residual: `grammar_id=json`, `domain=json_bench`, `json_<corpus>` Criterion
   paths, and fixture names remain JSON-specific telemetry. This is packet-
   consistent only because it is confined to W0 `bbnf-bench` report/gate
   surfaces. The same pattern would be a blocker if moved into runtime, codegen,
   IR, parser, SIMD, public API, build, or substrate code.

## Required Disposition If Rejected

Not applicable for this CH3 verdict.

If a later V5 consolidator finds a CH3 blocker, the minimal disposition should
be to revert the W0 report/gate/schema/RESULTS slice as one unit per SPEC
Section 3, add or update W0 REDRESS naming the exact frozen root or Lock 14 leak,
and rerun:

```sh
git diff --name-only 0c49fabd^ 0c49fabd -- <frozen-root-paths>
cargo test -p bbnf-bench --profile ax-iter
CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results
```

## Residual Risks

- I did not rerun the full Criterion benchmark capture. This review uses the
  committed `/tmp/skv8-w0-target` evidence root plus gate replay and focused
  negative probes.
- The Lock 14 proof is a W0 proof, not permission for W1-W6. Any later generic
  CostFacts, codegen, runtime, SIMD, parser-template, substrate, or public API
  edit still needs the SPEC Section 2.1 non-JSON proof.
- Fixture-scoped fingerprinting is acceptable as capture identity for the W0
  bench gate. If later waves add row families, they should avoid broadening this
  into a generic policy keyed on JSON grammar or corpus names.
