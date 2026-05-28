# SK-V15 S-P1 Hardening V1 - Consolidated

Disposition: ACCEPT 6/6 after fold.

| Lens | Initial result | Fold result | Notes |
|---|---|---|---|
| CH1 Correctness | REJECT | ACCEPT | Added artifact manifest and normalized symbol/%/file:line ledger; clarified RESULTS is not mutated by S-P1. |
| CH2 Generality | REVISE | ACCEPT | Added CH2-normalized attribution; blocked generated/schema/harness/sidecar rows as parser primitive proof. |
| CH3 Regression | REVISE | ACCEPT | Added explicit no-reopen routing for REDRESS-50-55/60-72/80/82-84/88/89. |
| CH4 Cost | ACCEPT | ACCEPT | Clean worktree, command logs, profile files, sidecars, and PMU logs are reproducible. |
| CH5 Hidden Coupling | ACCEPT | ACCEPT | Track surfaces and sidecar/tooling caveats remain separated. |
| CH6 Anti-Paper-Close | ACCEPT | ACCEPT | 119 interactive profiles and sidecars exist; no `--save-only` remains. |

S-P1 V1 has one ACCEPT cycle. Per `restart/prompts/ORCHESTRATOR.md` §3Z / `PASS-1-PROFILE.md`, S-P1 still requires a second consecutive ACCEPT cycle before LOCK. V2 may be a challenge-only confirmation over the folded packet unless new evidence changes.

Forward inputs for S-P2:

- Parse-only Track 1 beats best strict comparator by c/B on 17/17 rows.
- direct_strict Track 1 misses c/B against best strict comparator on `mesh` and `unicode_escapes`.
- real_typed Track 1 misses c/B on `unicode_escapes`.
- Mode-III profiles expose harness hashing, checksum, and UTF-8 masking costs; those rows are diagnostic, not parser wins.
- `p1e-normalized-attribution.tsv` is the binding primitive antecedent ledger for S-P2.
