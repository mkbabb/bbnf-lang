# CH3 W0 V4 Hardening Challenge - Lock 14 Freeze

## Verdict

ACCEPT, confidence 96%.

W0 at `077aadad8aacf95e3250ec157f30ba6ab873bf6b` does not materially violate
the CH3 regression/freeze lens. The V4 fold stays inside W0 telemetry/report/gate
owner paths, leaves grammar/runtime/tape/codegen/IR/passes/grammar/parser/direct/
typed/SIMD source roots unchanged, and has executable gates for the V3 blockers
that could otherwise reopen a source-surface route.

This is a CH3 verdict only. It does not by itself close W0 or authorize W1-W6;
ORCHESTRATOR still requires the challenge/consolidation flow and convergence
rules (`restart/prompts/ORCHESTRATOR.md:74`, `restart/prompts/ORCHESTRATOR.md:83`,
`restart/prompts/ORCHESTRATOR.md:104`, `restart/prompts/ORCHESTRATOR.md:118`).

## Scope

Reviewed the current folded target `077aadad` after the V3 rejection/fold, the
live packet docs, the V3 consolidated blockers, and the W0 source gates. The
adversarial target was Lock 14 grammar-neutrality plus frozen roots: no new
directive, BIR, substrate, asm, build, grammar/runtime/tape/codegen/IR/passes/
grammar/parser/direct/typed/SIMD source-surface change.

Packet constraints used:

- W0 authority only; W1-W6 remain conditional (`restart/skinny/tranches/sk-v8/SPEC.md:31`,
  `restart/skinny/tranches/sk-v8/SPEC.md:35`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:92`).
- Lock 14 generic-crate gate and non-JSON proof rule (`restart/skinny/tranches/sk-v8/SPEC.md:261`,
  `restart/skinny/tranches/sk-v8/SPEC.md:266`,
  `restart/skinny/tranches/sk-v8/SPEC.md:279`).
- Non-negotiables: no directives, BIR variant, `UnionTape`, substrate surface,
  public substrate API, parser-owned structural cursor/facts, JSON policy in
  generic crates, orphan primitive/kernel/generated path (`restart/skinny/tranches/sk-v8/SPEC.md:191`,
  `restart/skinny/tranches/sk-v8/SPEC.md:193`,
  `restart/skinny/tranches/sk-v8/SPEC.md:206`).
- W0 owner paths and pre-blocked behavior/source routes (`restart/skinny/tranches/sk-v8/SPEC.md:288`,
  `restart/skinny/tranches/sk-v8/SPEC.md:290`,
  `restart/skinny/tranches/sk-v8/SPEC.md:333`,
  `restart/skinny/tranches/sk-v8/SPEC.md:339`).
- Handoff rejects W0 if parser/scanner/SIMD/asm/codegen/product-plane behavior
  changes (`restart/skinny/tranches/sk-v8/HANDOFF.md:139`).

## Evidence

- Parent diff is within W0 packet/report/gate files only: `git show --name-only
  --format='%H%n%s' --no-renames 077aadad` names `restart/skinny/tranches/sk-v8/{DISPATCH-PROMPT,HANDOFF,SPEC,SYNTHESIS}.md`,
  `skinny/RESULTS.md`, and `skinny/crates/bbnf-bench/src/{bin/gate.rs,gate.rs,report.rs}`.
- Frozen-root parent diff is empty: `git diff --name-only 077aadad^ 077aadad --
  skinny/grammars skinny/test_data skinny/crates/test-fixtures
  skinny/crates/runtime/src skinny/crates/ir/src skinny/crates/passes/src
  skinny/crates/codegen/src skinny/crates/grammar/src skinny/crates/bbnf/src
  skinny/crates/bbnf-simd/src skinny/crates/bbnf-simd/build.rs
  skinny/crates/bbnf-simd/ext skinny/crates/parse-that-regex/src
  skinny/crates/bbnf-bench/src/direct_struct.rs
  skinny/crates/bbnf-bench/src/real_typed_struct.rs
  skinny/crates/bbnf-bench/src/generated_real_typed.rs
  skinny/crates/bbnf-bench/src/track2 skinny/crates/bbnf-bench/src/parity.rs
  skinny/crates/bbnf-bench/src/scan.rs
  skinny/crates/bbnf-bench/src/materialization.rs
  skinny/xtask/src/real_typed_schema.rs` produced no paths.
- Frozen-root current worktree diff is empty: same pathset with `git diff
  --exit-code -- ...` exited 0.
- `cargo test -p bbnf-bench --profile ax-iter` passed: 51 lib tests, 8 gate-bin
  tests, and doc tests. This includes `lock14_baseline` tests for directive/asm
  frozen roots, BackendShape drift, dirty frozen status, and W0 negative tests
  for unknown comparator strict-admission shapes, malformed sidecar evidence,
  SIMD metadata mismatch, and volatile-probe fingerprint exclusion.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
  xtask gate-json --advisory --check-results` passed against the committed W0
  evidence root.
- `cargo xtask check-json`, `cargo xtask check-real-typed`, and `cargo xtask
  check-conformance` passed; conformance accepted 21 valid fixtures and rejected
  7 invalid fixtures.
- Worktree was clean before writing this artifact.

Source-gate support:

- `gate-json` calls the Lock 14 baseline before report generation
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:41`) and validates schema plus W0
  before writing or comparing results (`skinny/crates/bbnf-bench/src/bin/gate.rs:314`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:324`).
- Lock 14 baseline marks generic/runtime/tape/IR/passes/codegen/SIMD and direct/
  typed/parity/scan/materialization surfaces read-only, while W0 gate/report code
  is telemetry-only (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:219`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:267`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`).
- The same baseline runs `git status`, current diff, and parent diff over frozen
  roots (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:403`), and rejects
  `BackendShape`/`UnionTape` drift (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:462`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:485`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:488`).
- W0 comparator validation rejects unsupported comparator ids before acceptance,
  requires known native/source semantics, and rejects sidecar same-run without a
  structured manifest (`skinny/crates/bbnf-bench/src/report.rs:973`,
  `skinny/crates/bbnf-bench/src/report.rs:1017`,
  `skinny/crates/bbnf-bench/src/report.rs:1051`,
  `skinny/crates/bbnf-bench/src/report.rs:1125`,
  `skinny/crates/bbnf-bench/src/report.rs:1151`).
- Strict admission requires measured-row UTF-8/escape evidence, same output
  plane, `same-run-native`, and `sidecar_freshness=n/a`; this closes the V3
  sidecar-same-run strict-admission gap (`skinny/crates/bbnf-bench/src/gate.rs:135`,
  `skinny/crates/bbnf-bench/src/gate.rs:151`,
  `skinny/crates/bbnf-bench/src/gate.rs:157`,
  `skinny/crates/bbnf-bench/src/gate.rs:163`,
  `skinny/crates/bbnf-bench/src/gate.rs:172`).
- SIMD metadata is read fallibly and validated against fixture hash, bytes,
  capture identity, benchmark semantics, capture policy, and scalar parity hash
  before report acceptance (`skinny/crates/bbnf-bench/src/bin/gate.rs:1353`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1364`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1375`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1387`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1396`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1419`).
- `run_id` now fingerprints W0 Criterion inputs rather than arbitrary path text,
  and the test proves de-rendered probe estimates do not churn it
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:385`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:725`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1749`).
- `RESULTS.md` exposes the current 38-row W0 state and gate-consumed manifest
  rather than behavior admission: main rows span `skinny/RESULTS.md:5` through
  `skinny/RESULTS.md:42`, the manifest starts at `skinny/RESULTS.md:44`, and the
  close note keeps the overall outcome `N-direct / NoGo` at
  `skinny/RESULTS.md:138`.

## Findings

1. P0 blocker: none found. The V4 fold does not touch frozen grammar/runtime/tape/
   codegen/IR/passes/grammar/parser/direct/typed/SIMD roots in the parent diff or
   current worktree, and the live gate checks the same frozen roots executably.

2. P1 blocker: none found. The V3 CH3-eligible blockers are folded: unknown
   comparator strict-admission shapes, sidecar same-run paper evidence, SIMD
   metadata drift, and run-id churn all have validator paths plus focused tests.

3. P2 residual only: W0 telemetry remains JSON-row-specific inside
   `bbnf-bench`, but this is an allowed skinny W0 bench/report surface, not a
   generic crate policy leak. The generic and generated behavior surfaces are
   frozen by diff and by `lock14_baseline`.

## Required Disposition If Rejected

Not applicable for this CH3 verdict. If a later consolidator rejects W0 on a new
CH3 blocker, the disposition should be: revert the report/gate/schema/RESULTS
slice as one unit per SPEC Section 3, record REDRESS for the exact violated
frozen root or grammar-neutrality gate, and rerun the parent-diff freeze command
plus `cargo test -p bbnf-bench --profile ax-iter` and `cargo xtask gate-json
--advisory --check-results`.

## Residual Risks

- I did not rerun the full benchmark capture; acceptance is against the committed
  W0 evidence root and focused tests/gates.
- The Lock 14 freeze proves no V4 W0 source-surface movement relative to
  `077aadad^` and current worktree cleanliness; it does not prove future W1-W6
  plans are safe.
- JSON-specific strings in `bbnf-bench` remain tolerable only while they stay
  bench/report telemetry. Any later generic crate, codegen, runtime, parser,
  SIMD, or substrate edit must carry the SPEC Section 2.1 non-JSON proof.
