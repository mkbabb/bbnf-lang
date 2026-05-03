# BB.W2c — Byte-Equal Regression Artefact + Hand-Written Deletion

**Thesis** Hereupon the cohort template emission's byte-equality precondition (W2a M2) is documented as a regression artefact at `docs/tranches/BB/audit/W2c-byte-equal-evidence.md`; the 25 hand-written cohort files delete only after the byte-equality gate passes. **Closer-gate** `git diff --stat HEAD~1..HEAD` shows file deletions only, no content drift; the byte-equality precondition is the deletion gate.

## §1 Deliverable

W2c is the verification + cleanup wave following W2a's template emission. The byte-equality precondition at W2a M2 establishes that template emission produces byte-identical output to the hand-written cohort files; W2c records the evidence and performs the atomic deletion.

The discipline:
1. **W2a M2**: shadow-emit cohort files via template; diff against hand-written; require zero diff.
2. **W2c M1**: record diff evidence at `docs/tranches/BB/audit/W2c-byte-equal-evidence.md`.
3. **W2c M2**: atomically delete the 25 hand-written cohort files (5 grammars × 5 templated file types: document, view, kind, value, mod).
4. **W2c M3**: verify the templated emission becomes the sole source via `git log --diff-filter=D` showing the deletions, plus the regression test suite passing.

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W2c verification | Verify W2a + W2b closer-gates passed | `cargo nextest run -p bbnf --profile ax-iter` 100% pass | Pre-deletion baseline stable. |
| M1 | Byte-equal evidence artefact | Land `docs/tranches/BB/audit/W2c-byte-equal-evidence.md` recording the diff outputs from W2a M2 (one diff per cohort grammar × 5 templated file types) | `test -f docs/tranches/BB/audit/W2c-byte-equal-evidence.md && grep -c 'DIFF: 0' docs/tranches/BB/audit/W2c-byte-equal-evidence.md` returns 25 | Evidence lands; 25 zero-diff confirmations recorded. |
| M2 | Hand-written file deletion | Delete the 25 hand-written cohort files atomically; the templated emission at W2a becomes the sole source | `git diff --stat HEAD~1..HEAD` shows 25 deletions; no content additions | Deletion lands; cohort runtime mass shrinks per BB-G5. |
| M3 | Post-deletion regression | Run `cargo nextest run -p bbnf --profile ax-iter` 100% pass; verify the templated emission produces equivalent runtime behaviour | All tests pass | Templated emission is sole source; behaviour preserved. |

## §3 Closer gate

```sh
test -f docs/tranches/BB/audit/W2c-byte-equal-evidence.md                              # evidence lands
grep -c 'DIFF: 0' docs/tranches/BB/audit/W2c-byte-equal-evidence.md                    # 25 zero-diff confirmations
git log --diff-filter=D --name-only --pretty=format: HEAD~1..HEAD \
    | grep -E 'crates/core/src/runtime/(bnf|csv|ebnf|css_pretty|math)/' | wc -l       # 25 deletions
cargo nextest run -p bbnf --profile ax-iter                                             # 100% pass
find crates/core/src/runtime/{bnf,csv,ebnf,css_pretty,math} -name '*.rs' | wc -l       # 10 (only arena.rs + builder.rs survive per cohort)
```

## §4 Invariants

§I1. **Migration evidence (`docs/PHASE-4-DIRECTIVE-2026-05-03.md:127`)** — byte-equal diff vs hand-written cohort modules at first commit; the discipline is the precondition for safe deletion.

§I2. **Lock 6** — committed source artefacts; the deletion + templated emission preserves the committed-source discipline.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| The deletion happens before byte-equality verification | Medium | The W2c M1 → M2 sequence is mandatory; M2 cannot run without M1 evidence. |
| External downstream consumers reference deleted module paths | Low | The templated emission lands at the same module paths via `pub use` re-exports; the public surface is preserved by mechanism. |

## §6 Cross-references

- **Preceding wave**: BB.W2b.
- **Following wave**: BB.W3a.

## §7 Iter-time check

| Cargo Command | Expected Duration |
|---|---|
| `cargo nextest run -p bbnf --profile ax-iter` | ≤ 90 s |
| `git log --diff-filter=D --name-only` | ≤ 1 s |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W2c-byte-equal-evidence.md` | `docs/tranches/BB/audit/` | Per-cohort × per-file diff outputs (25 zero-diff confirmations) |

## §9 Audit lane forecast

| Lane | Response |
|---|---|
| Lane 1 | L6 honoured; templated emission committed as source |
| Lane 2 | Verification + cleanup wave; same-wave consumer is the regression test |
| Lane 6 | Cohort runtime LOC delta verified per BB-G5 |
