# SK-V15 W0-D Research: JSON 51 Guard Rows / Cold Rerun Feasibility

Date: 2026-05-28.
Repo: `/Users/mkbabb/Programming/bbnf-lang`.
Scope: research-only. No source, generated code, RESULTS, REDRESS, gate, or
bench artifact edits are authorized from this report.

## Finding

The JSON 51/51 guard is the complete SK-V15 opening JSON set: 17 JSON
corpora times three workloads (`parse_only`, `direct_to_struct`,
`real_typed_struct`). SK-V15 requires those 51 rows to remain admitted,
strict, same-plane, measured, and native Apple M5 Max / aarch64:
`restart/skinny/tranches/sk-v15/SPEC.md:51-53`,
`restart/skinny/tranches/sk-v15/SPEC.md:124-130`, and
`restart/skinny/tranches/sk-v15/SPEC.md:135-137`.

The checked-in `skinny/RESULTS.md` already renders the 51 visible JSON rows as
`A / GO`, `strict`, `measured-row`, `escape_complete=yes` at
`skinny/RESULTS.md:5-55`. The same rows appear in the telemetry manifest at
`skinny/RESULTS.md:61-111`; a local count over manifest row ids confirms 51
`json/...` rows. Notes bind the close state as 17/17 parse-only, 17/17 direct,
and 17/17 real-typed admitted at `skinny/RESULTS.md:139-141`.

The current CSS rows are not part of the JSON guard; they are the 24 SK-V14-W8R
broadcast rows at `skinny/RESULTS.md:112-135`. SK-V15 W0 must preserve them as
diagnostic broadcast evidence unless W1 demotes them:
`restart/skinny/tranches/sk-v15/SPEC.md:250-261` and
`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:119-128`.

## What The Guard Must Check

Minimum guard predicates for the 51 JSON rows:

- Row id set is exactly every `json/{corpus}/{workload}/main` for the 17
  corpora and three workloads named in the gate code at
  `skinny/crates/bbnf-bench/src/report.rs:13-33`.
- Each visible row remains `Outcome=A`, `Verdict=GO`, `Strictness=strict`,
  `parse_utf8=measured-row`, `escape_complete=yes`, and the same output plane
  as the opening row. The current rendered rows show these cells at
  `skinny/RESULTS.md:5-55`.
- Track 1 and Track 2 must stay distinct and Track 2 must remain independent.
  The current manifest records per-row Track 1, Track 2, comparator plane,
  per-iter equality, consumer, and independence fields at
  `skinny/RESULTS.md:59-111`.
- Host/build metadata must be native Apple M5 Max / aarch64 with
  `RUSTFLAGS="-C target-cpu=native"` and `target_cpu=native`. SK-V15 makes M5
  Max / aarch64 the only admission host at
  `restart/skinny/tranches/sk-v15/SPEC.md:135-137`; the current manifest rows
  carry `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max` and native flags,
  for example `skinny/RESULTS.md:61-69`.
- A rerun must not downgrade any verdict and must keep rerun numeric cells
  within `+/-1.0%` of the SK-V15-open baseline:
  `restart/skinny/tranches/sk-v15/SPEC.md:256-263`.

## Cold Baseline Capture

Do not use Criterion or `bench-json` as SK-V15-open cold evidence. `bench-json`
invokes `cargo bench` and, on full runs, updates results through gate-json:
`skinny/xtask/src/main.rs:252-283`. The Criterion JSON benchmark config has a
3-second warm-up at `skinny/crates/bbnf-bench/benches/json_parity.rs:637-645`,
and metadata hardcodes `warmup_samples=3`, `warmup_time_s=3.0`, and
`cold_cache_mode="warm"` at `skinny/crates/bbnf-bench/src/metadata.rs:300-345`.
That violates SK-V15's "No warm benches; cold per-parse evidence only" rule at
`restart/skinny/tranches/sk-v15/SPEC.md:135-137`.

Use `profile_direct` for cold rerun evidence. It is explicitly the cold
profiling binary (`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:1-5`),
defaults `warmup_iters` to zero
(`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:75-83`), runs no warm-up
unless the fourth argument is nonzero
(`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:98-105`), and emits a
machine-readable `PROBE_RESULT` with `warmup_iters`, Mbps, cycles,
instructions, and checksum
(`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:134-151`).

Feasible cold capture command shape on Apple M5/aarch64:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv15-w0-profile-target \
RUSTFLAGS="-C target-cpu=native" \
cargo build --release -p bbnf-bench --bin profile_direct
```

Then run the binary with `warmup_iters=0` for each row family, using the
current manifest sample count for that row:

```sh
# parse_only rows: compare Track 1, Track 2, sonic strict, serde_json
/tmp/skv15-w0-profile-target/release/profile_direct <iters> <corpus> parse_only_track1 0
/tmp/skv15-w0-profile-target/release/profile_direct <iters> <corpus> parse_only_track2 0
/tmp/skv15-w0-profile-target/release/profile_direct <iters> <corpus> parse_only_sonic 0
/tmp/skv15-w0-profile-target/release/profile_direct <iters> <corpus> parse_only_serde 0

# direct_to_struct rows
/tmp/skv15-w0-profile-target/release/profile_direct <iters> <corpus> direct_strict_track1 0
/tmp/skv15-w0-profile-target/release/profile_direct <iters> <corpus> direct_strict_track2 0
/tmp/skv15-w0-profile-target/release/profile_direct <iters> <corpus> direct_strict_sonic 0
/tmp/skv15-w0-profile-target/release/profile_direct <iters> <corpus> direct_strict_serde 0

# real_typed_struct rows
/tmp/skv15-w0-profile-target/release/profile_direct <iters> <corpus> real_typed_track1 0
/tmp/skv15-w0-profile-target/release/profile_direct <iters> <corpus> real_typed_track2 0
/tmp/skv15-w0-profile-target/release/profile_direct <iters> <corpus> real_typed_sonic 0
/tmp/skv15-w0-profile-target/release/profile_direct <iters> <corpus> real_typed_serde 0
```

The mode surface is present in `run_once`:
`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:154-240`. The per-row
sample counts are in the manifest sample-count column at
`skinny/RESULTS.md:61-111`; notable non-400 rows include
`apache_builds/parse_only` with 4000 iterations at `skinny/RESULTS.md:70` and
`y_string_unicode` direct/typed with 1000 iterations at
`skinny/RESULTS.md:110-111`.

## Feasible Commands

Read-only and feasible on Apple M5/aarch64:

```sh
sysctl -n machdep.cpu.brand_string hw.optional.arm.FEAT_CSSC \
  hw.optional.arm.FEAT_DotProd hw.optional.arm.FEAT_PMULL hw.optional.neon

cd /Users/mkbabb/Programming/bbnf-lang/skinny
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --check-results
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --check-results --skv14-existing-results-capture
CARGO_TARGET_DIR=/tmp/skv15-w0-profile-target RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin profile_direct
```

The host feature probe has already been captured for this tranche:
`restart/skinny/tranches/sk-v15/research/p2/evidence/host-aarch64-sysctl.txt:1-18`.
That evidence proves host feature presence only and does not admit primitives
without scalar reference, checkasm/parity, consumer, and row measurement:
`restart/skinny/tranches/sk-v15/research/p2/evidence/host-aarch64-sysctl.txt:20-23`.

Gate-json is feasible as a read-only validation only when `--update-results`
or `--write-results` is omitted. The xtask wrapper forwards supported
gate-json flags at `skinny/xtask/src/main.rs:285-306` and rejects unknown flags
at `skinny/xtask/src/main.rs:308-347`. The gate binary writes `RESULTS.md`
only under `update_results` at `skinny/crates/bbnf-bench/src/bin/gate.rs:407-428`
and `skinny/crates/bbnf-bench/src/bin/gate.rs:788-792`.

Not admissible for SK-V15-open cold evidence:

- `cargo run -p xtask -- bench-json`, because it runs Criterion warm benches
  and can update results on a full run:
  `skinny/xtask/src/main.rs:252-283`.
- `cargo bench -p bbnf-bench --bench json_parity`, because Criterion warm-up
  is configured at `skinny/crates/bbnf-bench/benches/json_parity.rs:637-645`.
- Any command with `--update-results`, `--write-results`, `regen-json`,
  `regen-css`, or `regen-real-typed` during this research slice, because those
  write generated code or ledgers.

## +/-1.0% Comparator

The `+/-1.0%` check should compare a cold SK-V15-open rerun against the current
SK-V15 opening row cells by row id and metric, not against the older
`SK_V8_OPEN_BASELINE` constants. The current opening values are the checked-in
`skinny/RESULTS.md` Track 1, Track 2, sonic-rs strict, and serde_json cells for
the same row at `skinny/RESULTS.md:5-55` plus the manifest metadata at
`skinny/RESULTS.md:61-111`.

Use only same-run, same-plane native Rust comparators for pass/fail. SK-V15
classifies same-run strict anchors as admission evidence and stale sidecars or
different output planes as planning signals only:
`restart/skinny/tranches/sk-v15/SPEC.md:86-93`. `skinny/RESULTS.md` repeats
that native Rust comparators are same-run strict anchors and absent C++
sidecars are never strict anchors at `skinny/RESULTS.md:150-152`.

Current code is not enough to enforce this. The report code still contains an
older SK-V8 opening baseline list at
`skinny/crates/bbnf-bench/src/report.rs:5294-5586`, and its W0 test explicitly
accepts a fresh throughput row with Track 1 multiplied by 1.37 and Track 2
multiplied by 0.72:
`skinny/crates/bbnf-bench/src/report.rs:9804-9810`. The xtask `--check-results`
path validates the existing SK-V14 W0 manifest and rolling delta at
`skinny/xtask/src/main.rs:400-415`, with row-count/manifest checks at
`skinny/xtask/src/main.rs:486-523`; it does not implement the SK-V15
`+/-1.0%` numeric freeze or SK-V15-required telemetry fields. W0 redress must
therefore add or validate a SK-V15-specific numeric comparator before claiming
the guard is automated.

## Dirty Tree Risks

The current dirty tree is a blocker for authoritative SK-V15-open timing until
the owner either cleans it, isolates a clean worktree at `16d26a84`, or routes
the dirty changes through the proper W0 plan/redress. A local status check
shows unrelated modifications in root runtime generated/projection files,
prior research JSON reports, skinny CSS generated files, and benchmark sources.

Relevant risks:

- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` is dirty and directly
  participates in `real_typed_struct` Track 1 paths. Any baseline captured now
  would not be a clean `16d26a84` HEAD baseline.
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs` and seven
  `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs` files are dirty.
  They mostly affect CSS broadcast/diagnostic rows, but a full `bbnf-bench`
  build compiles the crate with those local changes.
- Root `xtask/src/main.rs` and `xtask/src/regen_simple_runtime.rs` are dirty;
  the SK-V15 report/gate investigation should not infer authority from root
  xtask behavior without separating it from skinny `xtask`.
- Dirty root runtime files under `crates/core/src/runtime/{bbnf,bnf,css_pretty,csv,ebnf,google_sheets,math}`
  overlap later SK-V15 Pattern H/generated-discipline concerns. They should not
  be reverted by a W0 JSON guard worker, but they make whole-repo generated
  checks noisy.

The safest W0 baseline posture is: do not mutate this tree, do not run
`--update-results`, do not run warm benches, and do not claim SK-V15-open from
current dirty sources. Capture only after the implementing worker has a clean
HEAD-equivalent source tree or has explicitly routed the dirty delta through
the W0 plan/redress gate.
