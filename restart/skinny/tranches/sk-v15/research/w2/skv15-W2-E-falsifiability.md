# SK-V15 W2-E - Falsifiability Contract

Scope: W2 report schema, rejection predicates, and challenge risks from
`SPEC.md`, `DISPATCH-PROMPT.md`, S-P0 A3, and the implementation-overfit audit.

## Required Columns

The W2 gate report must carry:

| Column | Purpose |
|---|---|
| `included_roots` | Exact roots scanned by Lock 14 / Lock 16. |
| `excluded_roots` | Every skipped file, directory, or glob. |
| `reason` | Concrete exclusion reason. |
| `owner` | Wave or owner accountable for the exclusion. |
| `self_scan_status` | Whether the gate scanned its own exclusion report/list. |
| `primitive_status` | Lock 16 disposition for each source-present native primitive. |
| `gate_consumer` | Command/report consumer that fails on the row. |
| `affected_rows` | Manifest, dependency, or report rows impacted. |
| `disposition` | Final action: admitted, blocked, routed, deleted, or diagnostic. |

W2-specific fields should also include source path, finding kind, strict
command, scalar reference, rollback/REDRESS, dependency row, non-JSON receiver,
proof command, and JSON guard command.

## Rejection Predicates

Reject W2 if:

- any required report column is missing
- known omitted roots remain silent
- forbidden-token scan remains JSON-only
- companion gates can pass without `--check-results`
- source-present native primitives lack a manifest row and final disposition
- checkasm evidence lacks `BBNF_SIMD_STRICT=1`
- x86/AVX evidence is treated as admission evidence
- W2 deletes provider/template/CSS generated surfaces before W3/W6
- generic/gate/report edits lack non-JSON receiver proof
- JSON-adjacent roots are touched without preserving JSON guards

## Challenge Risks

CH7 is the most likely rejector: hidden exclusions and self-exempting gates are
the center of W2. CH2 rejects JSON-only closure. CH5 rejects hidden
provider/template coupling. CH6 rejects telemetry that is produced but not
gate-consumed. CH1 rejects primitive status not tied to source facts. CH3
rejects JSON guard regression. CH4 rejects folding W3/W6 deletion work into W2.

## Verification Commands

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo fmt --all --check
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p xtask skv15_w2
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p bbnf-bench skv15_w2
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- gate-json --check-results
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- primitive-checkasm
```
