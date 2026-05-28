# SK-V15 Wave W11 Challenge

Status: ACCEPT-W11-CLOSE-WITH-ROUTED-BLOCKS.

## CH1 Evidence

ACCEPT. W11 cites executable commands for lock count, Pattern H census,
generated/provenance headers, `check-json`, default `gate-json`, W7 decision
tests, W8/W9 lowerer tests, W9 all-five report gate, W10 quarantine tests, W10
report gate, and W6 CSS same-workload retime.

## CH2 Orphan Rows

ACCEPT. `skv15-W11-close-dependency-checklist.json` contains every `DEP-*` row
from `SPEC.md` and `DISPATCH-PROMPT.md`, including
`DEP-W11-CLOSE-NO-ORPHANS`.

## CH3 Regression

ACCEPT. W11 does not reopen W2R/W4R-style wave-graph cycles. Any CSS deletion or
provider deletion that was not proven in the same wave remains blocked or
excluded from live admission. SK-V16 receives routed remainder only after W11
records the SK-V15 proof state.

## CH4 Measurement

ACCEPT. JSON remains admitted from the existing 51 strict measured rows. CSS is
not admitted. The W11 CSS retime used `RUSTFLAGS='-C target-cpu=native'` on
aarch64 and re-proved `admitted_rows=0`.

## CH5 Hidden Coupling

ACCEPT. FNV remains bench-only quarantine; the W10 gate requires adversarial
fixtures and non-empty production scan classification. W11 does not convert FNV
metadata into a runtime selector, production arbiter, or correctness proof.

## CH6 Dirty Tree

ACCEPT. W11 records the pre-existing dirty generated CSS and generated
real-typed files as non-owned. Broad checks that consume those files are routed
diagnostics; targeted owner-scope checks pass.

## CH7 Overfit Prune

ACCEPT. W11 refuses the tempting overfit close: CSS L4 stays open, historical
SK-V12/SK-V14 CSS PASS-ADMIT text is treated as historical only, and the current
close packet preserves the typed same-workload rejection.
