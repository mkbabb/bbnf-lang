ACCEPT

# SK-V11 S-P1 Hardening V4 CH4 Cost/Reproducibility Confirmation

Disposition: ACCEPT.

## Scope

This confirmation read only CH4-relevant material:

- `restart/prompts/skinny/PASS-1-PROFILE.md` Section 3 CH4.
- `restart/prompts/ORCHESTRATOR.md` Section 3Z.
- Folded S-P1 packet P1-A through P1-F at HEAD `cc8656b8`.
- W0 baseline at
  `restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md`.
- `/tmp/skv11-p1/pmu/capture_status.tsv`, present during review.
- V1, V2, and V3 hardening consolidations.

PASS-1 CH4 asks whether every method block is reproducible, with verbatim
commands, run id, host triple, and build flags. ORCHESTRATOR 3Z requires prior
REVISE findings to be folded before the next cycle advances.

## Findings

- The current HEAD does not reopen CH4. HEAD `cc8656b8` archives the V3
  accepted challenge; P1-A through P1-F last changed at `2e988a6a`, the V2
  Lock 14 vocabulary fold. The packet under confirmation is therefore the same
  folded packet accepted by V3 CH4.
- The V1 CH4 gaps remain folded. V1 required shared capture provenance, P1-A
  samply caveat disclosure, P1-C scope correction, P1-E run id placement, and
  explicit binary/source provenance. The V1 consolidation records those folds,
  V2 CH4 accepted them, and V3 CH4 accepted them again.
- W0 provenance is sufficient. W0 names capture commit `3ce75df4`, Criterion
  root `/tmp/skv11-open-criterion-3ce75df`, target root
  `/tmp/skv11-open-target-3ce75df`, run id
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`, the exact bench command, and
  the exact `gate-json --with-cost-facts --check-results` verification command.
- P1-A through P1-F carry the common run id
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`.
- Host/toolchain provenance is present: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`;
  `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, LLVM 22.1.2.
- Build flags and binary provenance are present: release profile with debug
  symbols, `RUSTFLAGS="-C target-cpu=native"`, target directory
  `/tmp/skv11-profile-target-9c8da194`, and binaries
  `/tmp/skv11-profile-target-9c8da194/release/xctrace_probe` and
  `/tmp/skv11-profile-target-9c8da194/release/profile_direct`.
- Source provenance is explicit enough for rerun. The profiling binaries are
  tied to source SHA `3ce75df4`; documentation/results freeze SHA `9c8da194`
  is identified separately; V2 and V3 hardening changes are documentation-only.
- The shared build command is exact:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv11-profile-target-9c8da194 \
RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin xctrace_probe --bin profile_direct
```

- The capture ledger corroborates artifact coverage. The present
  `/tmp/skv11-p1/pmu/capture_status.tsv` records PMU parse 34/34 `rc=0`, PMU
  direct/typed 48/48 `rc=0`, samply parse 34/34 `rc=0`, samply direct 34/34
  `rc=0`, and samply typed 14/14 `rc=0`.
- xctrace caveats are disclosed. The same ledger records 81/82 CPU Counter
  traces and 81/82 Time Profiler traces as `rc=54`, plus one `rc=0` in each
  family. The packet frames `rc=54` as retained time-limit trace behavior, not
  clean process exit.
- Samply caveats are disclosed. P1-A does not claim a lost per-row samply shell
  transcript; it gives the retained artifact parameterization. P1-A, P1-B, and
  P1-E preserve the `symbolicated=false` caveat and use xctrace summaries as
  the self-time percentage authority.
- PMU framing is sufficient. P1-D uses `proc_pid_rusage` `PROBE_RESULT` rows
  in `/tmp/skv11-p1/pmu/parse_pmu_rows.tsv` and
  `/tmp/skv11-p1/pmu/product_pmu_rows.tsv` for cycles/B and CPI, and does not
  synthesize branch-miss, L1, or LLC columns when CPU Counter exports lack
  stable columns.
- Method framing is sufficient. P1-C is explicitly a W0 Criterion diagnostic
  masking-probe extraction, not a fresh samply Mode III capture. PMU,
  structural-scan, masking-probe, parse-only, and W0-clamped signals are framed
  as planning evidence or diagnostics; W0 Criterion plus `gate-json` remains
  the row-admission authority.

## Required Fold

None. CH4 can advance with the existing caveats carried forward as method
constraints:

- Samply self-time is artifact-only where exact per-row transcripts were not
  retained.
- xctrace `rc=54` means retained time-limit trace behavior, not clean exit.
- PMU/cycles facts are planning cost facts, not row-admission evidence.
- P1-C remains a W0 Criterion diagnostic extraction, not a fresh call-stack
  profile.
