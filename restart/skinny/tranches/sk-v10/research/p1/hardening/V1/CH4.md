# SK-V10 S-P1 V1 CH4: Cost And Reproducibility

Disposition: ACCEPT AFTER REVISE.
Date: 2026-05-19.
Scope: commands, run roots, export aggregation, fixture identity, and retained
instrumentation gaps.
Output: this file.

## Findings

CH4 returned REVISE on five reproducibility defects:

- P1-C omitted `cd /Users/mkbabb/Programming/bbnf-lang/skinny` before
  `cargo bench -p bbnf-bench`.
- P1-A/P1-B cited generated `summary.json` files without the exact
  export-to-summary command.
- P1-F used a placeholder `python3 <main-table extractor>` command.
- P1-A did not record the hyphenated `update-center.json` fixture caveat.
- P1-B did not explain accepted `xctrace` exit code 54 for time-limit captures.

## Fold

- P1-C now gives the correct skinny-workspace run root and the exact
  `extract_mode3_criterion.py` command.
- P1-A/P1-B now cite
  `tools/summarize_xctrace_time_profile.py` with the trace root, output root,
  and process binary.
- P1-F now cites `tools/extract_results_main_table.py` and its CSV output path.
- P1-A now records the `update_center` logical row versus
  `update-center.json` fixture mapping.
- P1-B now records that `xctrace` `rc=54` means the requested `--time-limit`
  capture completed, and accepts it only when the trace bundle, per-row log,
  and process samples exist.

Validation run after fold:

```sh
python3 -m py_compile restart/skinny/tranches/sk-v10/research/p1/tools/*.py
python3 restart/skinny/tranches/sk-v10/research/p1/tools/extract_mode3_criterion.py /tmp/skv10-p1/mode3-criterion
python3 restart/skinny/tranches/sk-v10/research/p1/tools/extract_results_main_table.py skinny/RESULTS.md /tmp/skv10-p1/results-main.csv
python3 restart/skinny/tranches/sk-v10/research/p1/tools/summarize_xctrace_time_profile.py --trace-dir /tmp/skv10-p1/repro-test/traces --output-dir /tmp/skv10-p1/repro-test/exports --process-binary xctrace_probe
```

## Disposition

ACCEPT. Reproduction commands are now explicit enough for S-P2 audit.
