# S-P1 V1 CH1 Correctness

Disposition: ACCEPT.

All profiling artefacts cover the required corpora and cite executable profile
or probe sources. P1-A covers 17/17 parse-only rows, P1-B covers 17/17 direct
and 17/17 typed rows, and P1-C covers 17/17 for four masking modes after the
`5ed43f8e1` profiling-tool repair. Hot-leaf claims cite inline symbol stacks
from `/tmp/skv16-p1/*top20-inline.tsv` and `/tmp/skv16-p1-mode3/*top20-inline.tsv`.
P1-D states the branch/cache counter limitation explicitly instead of
fabricating unavailable PMU columns.
