# SK-V9 S-P1 V2 CH4 Cost / Reproducibility

Disposition: REVISE.

The samply and extraction methods are replayable: build commands, target root,
profile flags, artifact paths, run id, host triple, and tool version are named.
The capture root is `/tmp/skv9-p1-rerun`.

The pass still cannot reproduce the mandatory PMU/cycles lane in this host
context. Because S-P1 convergence requires P1-D and not only samply, CH4 remains
REVISE until a replayable PMU source is available or the pass contract is
explicitly amended.
