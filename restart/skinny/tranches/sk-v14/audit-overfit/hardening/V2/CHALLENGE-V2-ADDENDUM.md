# S-P0 CHALLENGE V2 Dispatch Addendum

Addendum to `restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/CHALLENGE-CONTEXT.md` (committed at `7d0fbe071`). Same seven lenses, write-only protocol, per-§ disposition format. This addendum binds the cycle-specific differences.

## §0 — Cycle identity

CHALLENGE V2 over the V2 axis-redispatch outputs (commit `1735882a5`, 5 files modified +113/-55):

- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-lock14-scan.md` (A3 V2; H3 HIGH→LOW; H6 freestanding HIGH; L8 new; 30 unchanged)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-generator-truth.md` (A4 V2; 3 folds — scope-extension framing + json_provider line-cite refresh + Three→Four)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-decision-engine.md` (A5 V2; verdict-line FAIL-at-HEAD pattern across §0:11 + §3:102-107 + §4 row 4 + §5 closing bonus)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md` (A6 V2; LegacyPath both-readings-preserved disambiguation at §0:12 + §2 ledger row Status updated to "NEW (scope-extension over V13 Pattern G; not a reversal)")
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (V2; 5 folds — census 54/20 + §1.3 co-derivation note + verdict-line alignment + §1.2 Three→Four + §2.4 CH7-companion lint scope extension)

A1 and A2 STAND (no V2 folds).

## §1 — V2 disposition focus

Per-lens fold-verification overlay:

- **CH1 CORRECTNESS:** verify F-V2-A4-2 line-cite refresh (`sed -n '60p;64p;68p;72p;48p;80,100p' json_provider.rs`); F-V2-SYNTHESIS-1 census (54+20=74); F-V2-SYNTHESIS-4 "Three"→"Four"; SYNTHESIS §1.2 NEW-2.
- **CH2 GENERALITY:** verify F-V2-A3-1 (H3 reclassification preserves Lock-14 generality scope); F-V2-SYNTHESIS-5 lint glob extension covers both runtime + codegen sides.
- **CH3 REGRESSION:** verify F-V2-A4-1 scope-extension framing preserves V13 §7.1 row 1 HONEST verdict; F-V2-A6-1 LegacyPath both-readings paragraph; F-V2-SYNTHESIS-2 co-derivation note.
- **CH4 COST:** verify V2 folds introduce zero LOC/risk/cap drift; SK-V14 SYNTHESIS C-1..C-5 unchanged.
- **CH5 HIDDEN COUPLING:** verify V2 folds don't introduce parallel substrate, Track 1≡Track 2, or renamed-scanner Lock 1 violation.
- **CH6 ANTI-PAPER-CLOSE:** verify F-V2-A5-1 + F-V2-SYNTHESIS-3 verdict-line aligned across A5 + SYNTHESIS (CH6 V1 80% → V2 expected 100%); §5 closing also folded for coherence.
- **CH7 OVERFIT-PRUNE:** verify F-V2-SYNTHESIS-5 CH7-companion lint extension; re-execute `cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'` (expect 9 grammars).

## §2 — V2 output path

Write to `restart/skinny/tranches/sk-v14/audit-overfit/hardening/V2/CH{N}.md` (V2, not V1).

## §3 — Discipline unchanged

- **HARD CAP: 30 min** per lens.
- **DO NOT** `git add`. **DO NOT** `git commit`. Aggregator commits all 8 V2 hardening files atomically (CH1..CH7 + HARDENING-S-P0-V2-CONSOLIDATED.md).
- **EXECUTABLE-VERIFICATION MANDATE**: any shipped shell command, grep count, or file-size citation must be re-run and quoted.

## §4 — Convergence forecast

V1 aggregate 93.5% (145/155). V2 should hit ~98-100% if folds land cleanly. §3Z requires V2 ≥95% + V3 ≥95% (two consecutive). After V2 closes, V3 confirming pass over unchanged V2 artefacts closes §3Z LOCK → G-S-P0-CONVERGED → S-P1 dispatches per the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP.

## §5 — Aggregator unchanged

V2 aggregator dispatches separately after CH1..CH7 V2 lens files exist. Reads all seven V2 CH files; authors `HARDENING-S-P0-V2-CONSOLIDATED.md`; commits all 8 V2 hardening files atomically with `docs(sk-v14-audit-overfit-hardening-V2): challenge V2 + consolidated`.
