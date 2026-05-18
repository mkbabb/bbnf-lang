# SK-V9 P1-D: PMU And Cycles-Per-Byte

Pass: S-P1 Profile. Cycle: V2 post-W0 rerun.
Date: 2026-05-18.
Scope: PMU counters and cycles-per-byte for corpus/workload rows.
Output: this file.
Baseline: SK-V9-open at commit `90609aee`, run
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `RUSTFLAGS=-C target-cpu=native`, release/bench profiles with
debug symbols.
Profile tool: attempted `perf`, `xctrace`, and `powermetrics`.
Corpus coverage: 0/17 for real PMU counters; BLOCKED.

## §1 - Method

Commands:

```bash
which perf
xctrace list templates
powermetrics -n 1 -i 100 --samplers cpu_power
sudo -n powermetrics -n 1 -i 100 --samplers cpu_power
```

Captured output:

```text
$ which perf
perf not found
$ xctrace list templates
xcode-select: error: tool 'xctrace' requires Xcode, but active developer directory '/Library/Developer/CommandLineTools' is a command line tools instance
$ powermetrics -n 1 -i 100 --samplers cpu_power
powermetrics must be invoked as the superuser
$ sudo -n powermetrics -n 1 -i 100 --samplers cpu_power
sudo: a password is required
```

The probe transcript is saved at `/tmp/skv9-p1-rerun/p1d-pmu-probe.txt`.

## §2 - Findings

No real PMU counter source is available in this execution context. S-P1 forbids
estimated cycles-per-byte, so P1-D does not convert Criterion `ns_per_byte`,
wall-clock loop times, or inferred clock frequency into c/B.

## §3 - Delta vs SK-V8

Absent. There is no same-run cycle counter for SK-V9-open.

## §4 - Anomalies + Masking Signals

This is a pass-blocking infrastructure defect, not a behavior finding. The
blocked PMU lane must not be normalized into:

- Track 1 or Track 2 proof.
- strict comparator admission.
- a substrate or parser-owned fact slot.
- S-P2 primitive ancestry.

## §5 - Sources

- `/tmp/skv9-p1-rerun/p1d-pmu-probe.txt`
- `restart/prompts/skinny/PASS-1-PROFILE.md` CH1 PMU/cycles requirement
- `restart/skinny/tranches/sk-v9/research/skv9-W0-close.md`
