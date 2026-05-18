# SK-V8 W0 Research E: No-Behavior-Change Proof

## Scope

This is a W0 research artifact for proving that SK-V8 W0 lands telemetry and
gate/report validation only. It does not propose parser, scanner, SIMD, asm,
codegen, generated-output, or product-plane behavior edits.

W0 authority is narrow:

- SPEC Section 3 defines W0 owner paths as `skinny/crates/bbnf-bench/`,
  `skinny/xtask/src/`, `skinny/RESULTS.md`, W0 research artifacts, and
  `skinny/REDRESS.md` only if W0 rejects (`SPEC.md:329-338`).
- SPEC Section 3 requires the W0 plan to name the `SK-V8-open` capture method
  and no-behavior-change proof, and the exit gate requires no parser, scanner,
  SIMD, asm, codegen behavior, product-plane behavior, or generated parser
  output change (`SPEC.md:345-372`).
- DISPATCH-PROMPT says W0 is telemetry-only, may touch only SPEC Section 3 owner
  paths, and must run focused bbnf-bench/xtask tests plus the W0-updated
  `gate-json` path before admit (`DISPATCH-PROMPT.md:56-88`).
- SPEC Section 10 blocks new directive, BIR, substrate surface, `BackendShape`,
  `UnionTape`, public substrate API, sidecar/parallel substrate, Track 1/Track 2
  coupling, benchmark-private parsers, and automatic implementation dispatch
  (`SPEC.md:769-786`).

## Code-Surface Findings

`skinny/crates/bbnf-bench/benches/json_parity.rs` is the full benchmark driver.
For each fixture it runs parity checks before timing, then benchmarks Track 1
generated parse, Track 2 handcoded parse, sonic-rs strict and lossy, simd-json,
serde_json, direct-to-struct, real typed rows when available, and probe rows.
It writes metadata under `CARGO_TARGET_DIR/criterion/.../metadata.toml`
(`json_parity.rs:10-28`, `:43-63`, `:65-85`, `:87-179`, `:181-260`,
`:261-361`, `:490-509`). W0 can add telemetry capture here only if the parse
bodies, fixture set, workload names, and product calls stay unchanged.

`skinny/crates/bbnf-bench/src/bin/gate.rs` reads Criterion estimates and
metadata from `CARGO_TARGET_DIR/criterion`, validates schema and parity, renders
`skinny/RESULTS.md`, and exits according to the worst gate outcome unless
`--advisory` is supplied (`gate.rs:19-83`, `:195-242`). Existing notes already
state Track 1 is `runtime::generated_json::parse` and Track 2 is independent and
does not call Track 1 (`gate.rs:209-219`). W0 gate work should extend this
consumer path; it must not move the parse/direct/typed behavior boundary.

`skinny/xtask/src/main.rs` exposes `bench-json`, `gate-json`, `check-json`, and
`check-real-typed` (`main.rs:7-25`). `bench-json` runs `cargo bench -p
bbnf-bench`; a full run calls `gate-json` afterward, forwarding only
`--advisory` to the gate (`main.rs:210-238`). `gate-json` runs the `bbnf-bench`
`gate` binary (`main.rs:241-256`). `check-json` regenerates in memory and checks
the checked-in JSON runtime output without writing files (`main.rs:127-133`);
`check-real-typed` performs the same stale-output check for generated real typed
bench output (`main.rs:143-152`).

`skinny/crates/runtime/src/grammars/json/generated.rs` is checked-in generated
runtime output and is explicitly marked generated (`generated.rs:1`). In W0 it
is a freeze target, not an owner file.

## Owner-Path Boundaries Before Admit

Allowed W0 redress surfaces:

- `skinny/crates/bbnf-bench/src/bin/gate.rs`, `src/gate.rs`, `src/report.rs`,
  `src/metadata.rs`, and narrow benchmark metadata emission in
  `benches/json_parity.rs`, only for telemetry/report/gate validation.
- `skinny/xtask/src/main.rs`, only for W0 command plumbing around
  `bench-json`/`gate-json`/negative gate fixtures.
- `skinny/RESULTS.md`, only as the W0 baseline/report output.
- `skinny/REDRESS.md`, only if W0 rejects.

Freeze surfaces:

- Runtime JSON parser/generated/view/value/scan/sink/host/visitor files under
  `skinny/crates/runtime/src/grammars/json/`.
- Runtime tape internals under `skinny/crates/runtime/src/tape/`.
- SIMD and asm-adjacent code under `skinny/crates/bbnf-simd/`.
- Codegen and JSON templates under `skinny/crates/codegen/`.
- Grammar input `skinny/grammars/json.bbnf`.
- Product-plane bench helpers: `direct_struct.rs`, `real_typed_struct.rs`,
  `generated_real_typed.rs`, `track2/`, `parity.rs`, `scan.rs`, and
  `materialization.rs`.

## Required Diff Checks

Run these from the repository root after W0 redress and before admit.

```sh
git status --short
git diff --name-only -- skinny restart/skinny/tranches/sk-v8/research
```

The changed-file set must be explainable by the W0 owner paths. Any source path
outside W0 telemetry/gate/report plumbing is a W0 blocker.

Freeze runtime, grammar, SIMD, asm-adjacent, and codegen behavior:

```sh
git diff --exit-code -- \
  skinny/grammars/json.bbnf \
  skinny/crates/runtime/src/grammars/json \
  skinny/crates/runtime/src/tape \
  skinny/crates/bbnf-simd \
  skinny/crates/codegen \
  skinny/crates/parse-that-regex
```

Freeze product-plane helpers inside the otherwise allowed `bbnf-bench` owner
path:

```sh
git diff --exit-code -- \
  skinny/crates/bbnf-bench/src/direct_struct.rs \
  skinny/crates/bbnf-bench/src/real_typed_struct.rs \
  skinny/crates/bbnf-bench/src/generated_real_typed.rs \
  skinny/crates/bbnf-bench/src/track2 \
  skinny/crates/bbnf-bench/src/parity.rs \
  skinny/crates/bbnf-bench/src/scan.rs \
  skinny/crates/bbnf-bench/src/materialization.rs
```

Check for unexpected changed files with a whitelist. The exact whitelist should
be copied into the W0 plan and tightened to the implemented files.

```sh
changed="$(git diff --name-only -- skinny)"
unexpected="$(printf '%s\n' "$changed" | rg -v '^(skinny/(crates/bbnf-bench/(benches/json_parity\.rs|src/(bin/gate\.rs|gate\.rs|metadata\.rs|report\.rs|probes\.rs)|Cargo\.toml)|xtask/src/main\.rs|RESULTS\.md|REDRESS\.md))$' || true)"
test -z "$unexpected" || { printf 'unexpected W0 paths:\n%s\n' "$unexpected"; exit 1; }
```

## Generated-Output Checks

W0 should prove generated output is current and unchanged rather than regenerate
or edit it.

```sh
(cd skinny && cargo xtask check-json)
(cd skinny && cargo xtask check-real-typed)
git diff --exit-code -- \
  skinny/crates/runtime/src/grammars/json \
  skinny/crates/bbnf-bench/src/generated_real_typed.rs
```

`cargo xtask regen-json` and `cargo xtask regen-real-typed` are not W0 admit
commands. If either check command says generated output is stale, W0 should fail
or route the stale-output issue; it should not silently regenerate parser output
inside a telemetry-only wave.

## Bench And Gate Commands

These are W0 redress/admit commands, not research-agent commands. Use an
isolated target directory so Criterion metadata and RSS subprocess probes are
not mixed with stale local runs.

```sh
(cd skinny && CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --profile ax-iter)
(cd skinny && CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS="-C target-cpu=native" cargo xtask check-conformance)
(cd skinny && CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS="-C target-cpu=native" cargo xtask bench-json --advisory)
```

`bench-json --advisory` runs the full Criterion suite and then invokes
`gate-json --advisory` on success. A separate explicit gate refresh is useful
after inspecting target artifacts or when only gate/report code changed:

```sh
(cd skinny && CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --advisory)
```

Do not use `cargo xtask gate-json --with-cost-facts` in W0. That is W1's
consumer path.

The W0 plan also needs one negative malformed-sidecar-manifest command once the
gate fixture exists. The expected shape is a focused bbnf-bench or xtask test,
for example:

```sh
(cd skinny && CARGO_TARGET_DIR=/tmp/skv8-w0-target cargo test -p bbnf-bench malformed_sidecar_manifest --profile ax-iter)
```

The exact test name must come from the W0 plan/redress. W0 cannot admit without
an executable negative check proving `gate-json` rejects the malformed manifest.

## Admit Checklist

Before W0 admit, the redress owner should record:

1. `SK-V8-open` capture command, target dir, host triple, feature mask,
   `RUSTFLAGS`, run id, and git commit.
2. `cargo xtask check-json` and `cargo xtask check-real-typed` passed.
3. Runtime, grammar, SIMD, codegen, and product-helper diff freezes passed.
4. `cargo test -p bbnf-bench --profile ax-iter` passed.
5. `cargo xtask check-conformance` passed.
6. Full `cargo xtask bench-json --advisory` completed and produced the W0 report.
7. W0-updated `gate-json` consumed every emitted W0 telemetry field.
8. The malformed sidecar manifest negative test failed closed.
9. Every current main row has required telemetry and every throughput cell is
   within +/-1.0% of `SK-V8-open`.
10. `parse_only` rows remain substrate-guard non-admission and no sidecar,
    permissive, lossy, stale, or telemetry-only evidence is used as strict
    admission.

## Failure Routing

- Forbidden diff in parser/scanner/runtime/tape/SIMD/asm/codegen/generated or
  product-helper files: reject W0. Revert the behavior-producing slice and add
  W0 REDRESS naming the exact path and attempted route.
- `check-json` or `check-real-typed` fails: reject W0 unless the failure is
  proven pre-existing and routed before redress. Do not regenerate generated
  parser or typed output inside W0.
- `cargo test`, `check-conformance`, or parity fails: reject W0 and route the
  failed test, fixture, and command output to REDRESS. Do not reinterpret the
  failure as a telemetry gap.
- `bench-json` or `gate-json` fails schema, missing-field, strictness,
  sidecar-freshness, malformed-manifest, or unsupported-outcome checks: reject
  W0 and route to gate/report REDRESS.
- Any throughput cell moves more than +/-1.0% versus `SK-V8-open`: reject W0
  unless the W0 plan already names a valid rerun rule and the rerun preserves the
  no-behavior diff proof. A second unexplained movement is REDRESS evidence, not
  admit evidence.
- Any attempt to use `parse_only`, `tape_vs_tape`, sidecar, permissive, lossy,
  stale, or telemetry-only rows as strict admission: reject W0 under SPEC Section
  10 and DISPATCH non-negotiables.

## Research Verdict

ACCEPT for W0 planning use. The no-behavior proof should be treated as a hard
admit gate: W0 may update telemetry/report/gate validation, but it cannot close
if any parser, scanner, SIMD, asm, codegen, generated-output, or product-plane
behavior surface has changed.
