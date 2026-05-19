ACCEPT

# SK-V11 S-P1 Hardening V3 CH4 Cost/Reproducibility

Disposition: ACCEPT.

## Scope

Read scope for this CH4 pass:

- `restart/prompts/skinny/PASS-1-PROFILE.md` Section 3 CH4.
- `restart/prompts/ORCHESTRATOR.md` Section 3Z.
- Folded S-P1 packet P1-A through P1-F after commit `2e988a6a`.
- W0 baseline at
  `restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md`.
- `/tmp/skv11-p1/pmu/capture_status.tsv`, present during review.
- V1 and V2 hardening consolidations, including V1/V2 CH4.

CH4 asks whether the profile is reproducible. The S-P1 specialization requires
verbatim rerunnable method blocks with run id, host triple, and build flags.
ORCHESTRATOR Section 3Z additionally requires prior REVISE dispositions to be
folded before the next cycle advances.

## Findings

- The V1 CH4 blockers are folded. V1 required shared capture provenance,
  P1-A samply caveat disclosure, P1-C scope correction, P1-E run id placement,
  and explicit binary/source provenance. The V1 consolidation records those
  folds, and V2 CH4 accepted them.
- Commit `2e988a6a` does not reopen CH4. Its fold is a V2 Lock 14 vocabulary
  cleanup in P1-B and P1-E plus V2 hardening archival; it does not change
  capture artifacts, source binaries, run ids, build flags, row outcomes, or
  gate floors.
- W0 is reproducible enough for CH4. It names commit `3ce75df4`, Criterion root
  `/tmp/skv11-open-criterion-3ce75df`, target root
  `/tmp/skv11-open-target-3ce75df`, run id
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`, the exact W0 bench command,
  and the exact `gate-json --with-cost-facts --check-results` verification
  command.
- The folded P1 packet carries a common run id:
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`. P1-A through P1-F carry host
  and toolchain provenance: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5
  Max`, `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, LLVM 22.1.2.
- Build flags and binary provenance are explicit. The packet records release
  builds with debug symbols, `RUSTFLAGS="-C target-cpu=native"`, target
  directory `/tmp/skv11-profile-target-9c8da194`, and binary paths
  `/tmp/skv11-profile-target-9c8da194/release/xctrace_probe` and
  `/tmp/skv11-profile-target-9c8da194/release/profile_direct`.
- Source provenance is explicit enough for a third party to rerun the packet.
  The profiling binaries are tied to source SHA `3ce75df4`, while
  documentation/results freeze SHA `9c8da194` is identified separately. The V2
  and V3 folds are documentation-only.
- The exact shared build command is present across the packet:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv11-profile-target-9c8da194 \
RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin xctrace_probe --bin profile_direct
```

- Capture status corroborates the packet's coverage claims. The present
  `/tmp/skv11-p1/pmu/capture_status.tsv` records PMU parse 34/34 `rc=0`, PMU
  direct/typed 48/48 `rc=0`, samply parse 34/34 `rc=0`, samply direct 34/34
  `rc=0`, and samply typed 14/14 `rc=0`.
- xctrace return-code caveats are disclosed rather than papered over. The
  status ledger records 81/82 CPU Counter traces and 81/82 Time Profiler traces
  as `rc=54`, with one `rc=0` in each family. The packet frames `rc=54` as the
  expected time-limit exit when trace bundles and exported symbol summaries are
  retained, not as clean program exit.
- Artifact caveats are honest. P1-A does not claim a lost per-row samply shell
  transcript; it gives the retained artifact parameterization and treats samply
  as artifact-only flame-profile evidence where needed. P1-A, P1-B, and P1-E
  preserve the `symbolicated=false` samply caveat and use xctrace summaries as
  the self-time percentage authority.
- P1-D frames PMU correctly. Cycles-per-byte and CPI are taken from
  `proc_pid_rusage` `PROBE_RESULT` rows in
  `/tmp/skv11-p1/pmu/parse_pmu_rows.tsv` and
  `/tmp/skv11-p1/pmu/product_pmu_rows.tsv`; branch-miss, L1, and LLC columns
  are not synthesized when the CPU Counter exports do not provide stable
  columns.
- Method framing is aligned with S-P1. P1-C is now a W0 Criterion masking-probe
  extraction, not a new samply Mode III call-stack capture. PMU throughput,
  structural scan, masking probes, parse-only evidence, and W0-clamped rows are
  all framed as planning evidence or diagnostics, not row-admission evidence.

## Required Fold

None.

Carry these caveats forward as method constraints, not V3 blockers:

- Samply self-time is artifact-only where the exact per-row transcript was not
  retained.
- xctrace `rc=54` means retained time-limit trace behavior, not clean exit.
- PMU/cycles facts are planning cost facts; Criterion W0 plus `gate-json`
  remains the row-admission authority.
- P1-C remains a W0 Criterion diagnostic extraction, not a fresh call-stack
  profile.
