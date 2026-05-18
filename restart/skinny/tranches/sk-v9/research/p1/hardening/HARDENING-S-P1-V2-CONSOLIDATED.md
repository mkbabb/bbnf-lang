# SK-V9 S-P1 V2 Hardening Consolidated

Date: 2026-05-18.
Pass: S-P1 Profile.
Cycle: V2 post-W0 rerun.
Input cohort: `p1a-samply-mode-1.md` through `p1f-results-delta.md`.
Challenge cohort: `hardening/V2/CH1.md` through `CH6.md`.
Disposition: BLOCKED.
ACCEPT rate: 4/6 = 66.7%.
Convergence: not converged.

## Verdict

S-P1 V2 is an evidence-bearing rerun, not a paper ledger. W0 is closed, the
active baseline is `sk-v9-open:criterion-fnv64-cd1673844eeea12f`, and the rerun
produced 106 fresh samply profiles with symbol sidecars and full corpus coverage
for P1-A, P1-B, and P1-C.

The pass still cannot close because P1-D has no real PMU/cycles-per-byte source
on this host. The contract requires real PMU counters. This run found:

```text
perf not found
xctrace requires full Xcode; active developer dir is CommandLineTools
powermetrics must be invoked as the superuser
sudo -n powermetrics: password is required
```

## Lens Dispositions

| Lens | Disposition | Finding |
|---|---|---|
| CH1 Correctness | REVISE | Fresh samply coverage is complete, but real PMU/cycles are absent. |
| CH2 Generality / Lock 14 | ACCEPT | No JSON-only primitive, directive, BIR, substrate, or policy leak. |
| CH3 Regression / REDRESS | ACCEPT | Blocked typed/direct/structural routes remain blocked. |
| CH4 Cost / Reproducibility | REVISE | Samply replay is reproducible; PMU replay is unavailable. |
| CH5 Hidden Coupling | ACCEPT | Diagnostic surfaces remain non-producers; Track 2 is not normalized. |
| CH6 Anti-paper-close | ACCEPT | Bad panic profiles were rerun; PMU is honestly blocked, not estimated. |

## Folded Requirements

1. Do not dispatch W1+ behavior waves.
2. Do not dispatch S-P2 primitive design from this profile, because S-P1 did not
   converge.
3. Preserve the fresh samply evidence as a partial S-P1 V2 profile.
4. Treat P1-D as the only hard blocker: obtain a real PMU/cycles source or
   explicitly revise the S-P1 contract before another convergence attempt.
5. Keep W0 row fences unchanged: no Apache/CITM/Canada measured typed rows, no
   direct row admission, no structural-scan producer, no strict upgrade.

## Evidence

Passed:

```text
106 samply profile/sidecar pairs under /tmp/skv9-p1-rerun/profiles
profile sample counts: min 4447, max 45504
P1-F extracted 38 SK-V9-open manifest rows
```

Blocked:

```text
/tmp/skv9-p1-rerun/p1d-pmu-probe.txt
```

## Cycle Result

V2 is blocked, not converged. The next valid move is a P1-D environment repair
or explicit contract revision. Behavior waves remain unauthorized.
