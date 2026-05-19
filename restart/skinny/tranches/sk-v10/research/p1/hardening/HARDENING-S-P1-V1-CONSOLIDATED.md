# SK-V10 S-P1 V1 Hardening Consolidation

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-19.
Scope: consolidate CH1-CH6 dispositions for the SK-V10 profile packet.
Output: this file.

## Disposition Summary

| Lens | Initial disposition | Folded disposition |
|---|---|---|
| CH1 correctness | REVISE | ACCEPT |
| CH2 generality / Lock 14 | ACCEPT | ACCEPT |
| CH3 regression / REDRESS | ACCEPT | ACCEPT |
| CH4 cost / reproducibility | REVISE | ACCEPT |
| CH5 hidden coupling | ACCEPT | ACCEPT |
| CH6 anti-paper-close | REVISE | ACCEPT |

Final: ACCEPT, 6/6 after fold. No open critical defect. No unresolved REVISE.

## Folded Corrections

- P1-C now states the Mbps formula and cites `new/benchmark.json` byte counts.
- P1-C now cites existing bench source paths.
- P1-A/P1-B/P1-E source anchors now use verified function-start lines.
- P1-A records the `update_center` / `update-center.json` fixture mapping.
- P1-A/P1-B/P1-C/P1-F now include executable extraction or aggregation tools.
- P1-B records `xctrace` `rc=54` as the accepted time-limit completion case.

## Validation

Executed after folding:

```sh
python3 -m py_compile restart/skinny/tranches/sk-v10/research/p1/tools/*.py
python3 restart/skinny/tranches/sk-v10/research/p1/tools/extract_mode3_criterion.py /tmp/skv10-p1/mode3-criterion
python3 restart/skinny/tranches/sk-v10/research/p1/tools/extract_results_main_table.py skinny/RESULTS.md /tmp/skv10-p1/results-main.csv
python3 restart/skinny/tranches/sk-v10/research/p1/tools/summarize_xctrace_time_profile.py --trace-dir /tmp/skv10-p1/repro-test/traces --output-dir /tmp/skv10-p1/repro-test/exports --process-binary xctrace_probe
rg -n "skinny/benches|<main-table|metadata\\.toml|generated\\.rs:51|generated\\.rs:42|generated\\.rs:315|lib\\.rs:310|mod\\.rs:127" restart/skinny/tranches/sk-v10/research/p1
```

The final `rg` returned no matches.

## S-P2 Authorization

S-P1 V1 is accepted for S-P2 input. S-P2 must carry forward the routed
instrumentation fact that direct/typed PMU is absent in V1; a future direct PMU
probe is allowed only as profiling-tool redress, not behavior evidence.
