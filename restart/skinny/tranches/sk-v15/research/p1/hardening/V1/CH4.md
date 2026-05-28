# CH4 Cost - SK-V15 S-P1 V1

Disposition: ACCEPT.

The packet is reproducible enough for S-P1:

- Clean profiling worktree: `/Users/mkbabb/Programming/bbnf-lang-skv15-profile-279a60646`, HEAD `279a606466c60172932629dd9788cd80d6bc82b0`, empty `git status --short` when checked.
- Build command and profile flags are recorded in P1-A/P1-B/P1-C/P1-D.
- `/tmp/skv15-p1` contains profile, log, PMU, criterion, and summary trees.
- Repo evidence TSVs match the `/tmp/skv15-p1/summary` generated summaries.
- `skinny/RESULTS.md` is byte-identical to SK-V14 close `8e7378025` and rolling source `bae430dcf`.

Non-blocking polish: build command output and clean-worktree status are not copied as standalone evidence files, but command logs and artifact manifests are sufficient for this profile pass.
