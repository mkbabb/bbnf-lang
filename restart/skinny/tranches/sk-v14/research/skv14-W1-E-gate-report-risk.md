# SK-V14 W1E: Gate And Report Risk

Date: 2026-05-24.
Scope: W1 validation risks in `gate-json`, report rendering, and `RESULTS.md`.
Output: this file.

## §1 — Findings

- `skinny/xtask/src/main.rs:373-389` stores only a subset of the 32 manifest fields; it omits comparator evidence, validation path, CostFacts, consumer class, Track 2 status, and diagnostic nonproducer status.
- `skinny/xtask/src/main.rs:490-573` validates presence and a small enum set, but does not reject W0 placeholders like `not_admitted:*`, `none:pre-W1`, or absent comparator evidence for rows that W1 mutates.
- `skinny/crates/bbnf-bench/src/report.rs:3555` has the same shallow row validator.
- `skinny/crates/bbnf-bench/src/report.rs:4303` parses `Comparator evidence` as opaque text, while the structured comparator validator operates on `Vec<SkV8ComparatorEvidence>` at `report.rs:5451`.
- Current `skinny/RESULTS.md` is a valid W0 baseline but all 75 manifest rows have `not_admitted:*` equality placeholders. W1 must preserve that only for unchanged W0 baseline rows.

## §2 — Recommendations

- Extend the xtask `Skv14ManifestRow` parser to retain all 32 manifest cells.
- Parse comparator evidence segments enough to reject hidden stale anchors and parse_only `sonic_rs_anchor` after W1.
- Add W1-active row validation: moved rows cannot retain `not_admitted:pre-W1-*`, `legacy:*`, `none:pre-W1`, blank comparator evidence, or Track 2 entries pointing to sonic comparator functions.
- Keep W0 baseline placeholders acceptable only for rows whose W0 state is unchanged.
- Add mutation tests for blank comparator evidence, W1 rows with W0 equality placeholders, DOM parse_only evidence, and sidecar same-run claims without structured manifest.

## §3 — Risks

- If W1 only edits row text, the current gate can still pass stale W0 placeholders.
- If `RESULTS.md` parsing remains one-way, hand-mutated comparator evidence can bypass structured comparator validation.
- CSS rows carry comparator evidence with strict equality signals but still have W0 equality placeholders; W1 validation must avoid accidentally forcing W8 obligations early.

## §4 — Sources

- `skinny/xtask/src/main.rs:373-573`
- `skinny/crates/bbnf-bench/src/report.rs:3555`
- `skinny/crates/bbnf-bench/src/report.rs:4303`
- `skinny/crates/bbnf-bench/src/report.rs:5451`
- `skinny/RESULTS.md:53-127`
