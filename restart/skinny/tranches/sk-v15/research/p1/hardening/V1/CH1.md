# CH1 Correctness - SK-V15 S-P1 V1

Disposition: ACCEPT after fold.

Initial result: REJECT. CH1 objected that profile artifacts were not citable, `skinny/RESULTS.md` still carried `hot-leaf=not-collected`, and P1-E did not expose symbol / percent / file:line tuples inline enough for S-P2 consumption.

Folded evidence:

- `restart/skinny/tranches/sk-v15/research/p1/evidence/artifact-manifest.tsv` records 119 interactive samply profile rows, all with `exists=true`, `sidecar_exists=true`, and `log_exists=true`.
- `/tmp/skv15-p1/profiles-interactive` contains 119 `.json.gz` profiles and 119 `.json.syms.json` sidecars: P1-A 17, P1-B 68, P1-C 34.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/p1e-normalized-attribution.tsv` contains 119 rows with `raw_function`, `raw_pct`, `raw_file_line`, `primitive_boundary`, and S-P2 antecedent status.
- `skinny/RESULTS.md` remains an admission ledger. S-P1 resolves `hot-leaf=not-collected` for research consumption through external evidence; it does not mutate RESULTS in a read-only profile pass.

Residual caveat: raw top-20 sidecar rows can lack line numbers. The normalized ledger uses first line-resolved rows or blocks sidecar-symbolization drift, so no parser proof relies on a line-missing row.
