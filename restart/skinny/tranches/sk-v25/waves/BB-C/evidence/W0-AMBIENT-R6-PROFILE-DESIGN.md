# BB-C.W0 ambient R6 paired-profile design

**State:** born RED; frozen before either R6 profile binary is built or any
recorded candidate/control measurement is taken.

**Scope:** the generic Pratt-emitter dependency cone only: the seven BBNF,
seven Google Sheets, and two CSS L4 functions whose generated bodies acquire
one `record_compound_bounds_start` and one
`record_compound_bounds_end` call. This protocol grants no W0 item other than
item 3, no W1, lock, tranche, prototype, or performance credit by itself.

## Machine and toolchain

- architecture: native Apple `arm64` / Rust target `aarch64-apple-darwin`;
  every build and execution is forced through `/usr/bin/arch -arm64`; x86 is
  forbidden;
- machine: Apple M5 Max, `Mac17,7`, 128 GiB, 18 logical CPUs;
- OS: macOS 26.4.1 (25E253);
- compiler: `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, LLVM 22.1.2;
- Cargo: `cargo 1.96.0-nightly (eb94155a9 2026-04-09)`;
- allocator and operation: the existing harness bytes, including mimalloc,
  cold-per-parse `sample_size(1)`, 100 Divan samples, setup-time exclusion,
  and the existing per-iteration timeout;
- build profile: the existing `ay-final` publish-grade profile (fat LTO, one
  codegen unit), `--locked`, one Cargo job, and separate isolated target
  directories for control and candidate;
- execution directory: `crates/core`, so the existing relative fixture paths
  resolve exactly.

The run receipt must additionally freeze power/thermal state, relevant
environment, source ledgers, patch identity, binary Mach-O architecture and
SHA-256 identities, and every raw Divan output.

## Exact operands

The candidate is the sealed R6 fixed-point fleet. The control is that exact
fleet with one frozen inverse patch applied: it removes only the newly emitted
7/7/2 start/end call pairs and no pre-existing hook, emitter byte, runtime
byte, fixture, harness, Cargo input, or unrelated dirty byte. The control is
built first in its isolated target directory. The inverse patch is then
reversed, every governed source is checked byte-for-byte against the candidate
ledger, and the candidate is built in its own isolated target directory.

The single Cargo build shape names the three existing benches:

```text
/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=<isolated> \
  cargo build --target aarch64-apple-darwin --profile ay-final --locked \
  -p bbnf --bench bbnf_monolithic --bench google_sheets_monolithic \
  --bench css_l4
```

The receipt must resolve the three exact executable paths from Cargo's own
build output; filenames are never guessed and no stale target artifact may be
selected.

## Pairing and samples

1. Run one unrecorded complete warm-up invocation of each control and candidate
   harness. A warm-up failure is RED and stops the campaign.
2. Run 20 recorded candidate/control pairs per harness. Odd pairs execute
   control then candidate; even pairs execute candidate then control. Each
   invocation is a fresh child process and runs the complete unchanged Divan
   harness because these harnesses do not call Divan's CLI-argument parser.
3. Retain every stdout/stderr byte and exit code. Any nonzero exit, timeout,
   missing row, changed sample count, or parse failure is RED; there is no
   retry or dropped outlier.
4. Hard rows are all six BBNF monolithic rows, Google Sheets
   `parse_simple`/`parse_nested`/`parse_stress`, and CSS L4
   `normalize`/`bootstrap`/`tailwind`. The two Sheets formatting rows run as
   unavoidable unchanged-harness collateral and are reported separately, not
   silently discarded.

For each hard row and pair, the statistic is
`ln(candidate median / control median)`. A family-level paired statistic is
the equal-weight mean of its hard-row log ratios within that pair. Medians are
used because they are emitted directly by the unchanged Divan harness; means,
fastest values, and slowest values remain in raw evidence but are not selected
after measurement.

## Frozen inference and verdict

Using deterministic seed 42, bootstrap the 20 matched-pair log ratios with
replacement for 100,000 iterations. Report the exponentiated mean and the
two-sided percentile 95% interval for every hard row and family aggregate.
No practical-regression tolerance is invented:

- upper ratio at or below 1.0: measured non-regression established;
- lower ratio above 1.0: measured regression;
- interval spanning 1.0: inconclusive and therefore RED for R6 closure.

All classifications are reported. No favorable row may hide a regressing or
inconclusive hard row, and no profile result establishes semantic correctness.
