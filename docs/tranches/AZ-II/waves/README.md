# AZ-II Waves Index

AZ-II's original wave plan remains at the root of this directory:

| Spec | Status | Disposition |
|---|---|---|
| [`W0.md`](W0.md) | superseded | Bootstrap-cutover research, classifier extension, audit baseline; folded into cutover.A/B. |
| [`W1.md`](W1.md) | superseded | Stage A/B atomic byte-equal cutover; folded into cutover.B. |
| [`W2.md`](W2.md) | superseded | Tape deletion, parity recode, FINAL; scope-revealed into cutover.C onward. |

All cutover-specific specs live under [`cutover/`](cutover/):

| Spec | Status | Disposition |
|---|---|---|
| [`cutover/README.md`](cutover/README.md) | interim manifest | Cutover index and original embedded A/B/C archaeology. |
| [`cutover/A.md`](cutover/A.md) through [`cutover/O.md`](cutover/O.md) | historical | Substage agency records for the A-O execution stream. |
| [`cutover/O0.md`](cutover/O0.md) through [`cutover/O7.md`](cutover/O7.md) plus [`cutover/O3a.md`](cutover/O3a.md) | active terminal series | O0/O1/O2 landed; O3a routed closed; O3 active. |
| [`cutover/O3a-J1.md`](cutover/O3a-J1.md) / [`O3a-C1.md`](cutover/O3a-C1.md) / [`O3a-S1.md`](cutover/O3a-S1.md) / [`O3a-P1.md`](cutover/O3a-P1.md) / [`O3a-A1.md`](cutover/O3a-A1.md) | complete_with_misses child specs | Failure-cohort triads for O3a; source/archive/proof misses are routed to O3/O4/O5/O6/O7. |

Do not dispatch from W0/W1/W2. They are historical source for the
original plan. Active AZ-II dispatch reads the cutover subdirectory and
the current status in `../PROGRESS.md`.
