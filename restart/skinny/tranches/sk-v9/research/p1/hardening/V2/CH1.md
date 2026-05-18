# SK-V9 S-P1 V2 CH1 Correctness

Disposition: REVISE.

P1-A, P1-B, P1-C, P1-E, and P1-F are materially stronger than V1: 106 fresh
symbol-resolving samply profiles exist under `/tmp/skv9-p1-rerun/profiles`,
each has a `.syms.json` sidecar, and sample counts are 4,447-45,504. Corpus
coverage is complete for parse, direct, probes, and structural scan.

The pass still fails CH1 because P1-D has 0/17 real PMU/cycles rows. The prompt
requires real PMU counters, not estimated c/B. `perf` is absent, `xctrace`
requires full Xcode, and `powermetrics` requires superuser access unavailable in
this run.
