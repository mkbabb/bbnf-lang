# SK-V14 W3-F: Preblocks And Redress

Date: 2026-05-26.
Scope: W3 failure modes and blocked routes.
Output: this file.

## Section 1 - Findings

W3 exists to close P-3, the tiny-fixture recurrence. It does not delete CSS
templates, revert CSS rows, or re-admit CSS rows. Those are W4 and W8.

## Section 2 - Recommendations

If W3 fails, revert the corpus directory and loader slice together, then record
the failed floor, provenance, or loader check in `skinny/REDRESS.md`.

Keep W4 blocked until W3 admits because PRUNE-2 deletes the old CSS template
path and needs a production-corpus path ready for the later W8 readmit.

## Section 3 - Risks

Pre-blocked routes:

- Tiny embedded fixtures as production corpus.
- Synthetic padding to meet the 800 KiB floor.
- Claiming CSS L4 SOTA movement from W3 alone.
- Loader short-circuits keyed on exact old fixtures.
- Touching `crates/core/src/runtime/css_l4/`, still W6.0.

## Section 4 - Sources

- `restart/skinny/tranches/sk-v14/SPEC.md` Section 6.
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` P-3 and R5.
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-css-measurement.md`.
