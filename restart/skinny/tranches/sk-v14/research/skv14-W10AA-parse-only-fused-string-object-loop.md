# SK-V14 W10AA parse_only Fused String/Object-Loop Probe

Date: 2026-05-27.

Disposition: REJECT. No source patch lands, and no `RESULTS.md` or
`ROLLING-SOTA-DELTA.md` row moves.

## Candidate

W10AA tested a narrow generated parse_only source candidate:

- `skinny/crates/parse-that-regex/src/lib.rs`: add a fused trusted-UTF-8
  parse-only string-end helper that combines the tiny prefix scan with the
  continued strict string matcher.
- `skinny/crates/codegen/src/runtime_generator.rs`: route generated JSON
  `parse_only_string_end` through that fused helper.
- `skinny/crates/codegen/src/runtime_generator.rs`: remove the redundant
  `ObjectExpectKeyOrEnd` frame after the non-empty object case has already
  checked for `}`.
- `skinny/crates/runtime/src/grammars/json/generated.rs`: regenerated only via
  `cargo xtask regen-json`.

This was a fresh material differential over W10X/W10Y/W10Z: it did not replay
the inline frame stack, trusted syntax-mask scan, plain-string structural fast
path, or cursor-return helper ABI.

## Correctness

- `cargo xtask regen-json`
- `cargo xtask check-json`
- `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p parse-that-regex trusted_ -- --nocapture`
- `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p runtime generated_parse_only_accepts_and_rejects_json -- --nocapture`
- `cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p codegen emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`

The focused generated route tests passed before measurement. The transient
source patch was then reverted because no open parse_only row admitted.

## Cold Profile

Build from `skinny/`:

```sh
RUSTC_WRAPPER= RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
```

Run from `skinny/` across all 17 JSON parse_only rows:

```sh
target/release/profile_direct 400 <corpus> parse_only_track1 0
target/release/profile_direct 400 <corpus> parse_only_track2 0
target/release/profile_direct 400 <corpus> parse_only_sonic 0
target/release/profile_direct 400 <corpus> parse_only_serde 0
```

Evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W10AA-parse-only-fused-string-object-loop.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W10AA-parse-only-fused-string-object-loop.raw.log`

Open-row margins:

| row | Track 1 Mbps | sonic Mbps | margin vs `sonic + 1.0` |
|---|---:|---:|---:|
| twitter/parse_only | 12841.948 | 15868.374 | -3027.426 |
| github_events/parse_only | 14187.844 | 17081.410 | -2894.566 |
| update_center/parse_only | 10547.553 | 14568.576 | -4022.023 |
| random/parse_only | 8522.218 | 10784.752 | -2263.534 |
| gsoc-2018/parse_only | 22819.144 | 35998.458 | -13180.314 |
| distinct_values/parse_only | 6815.473 | 11841.960 | -5027.487 |

Admission threshold: Track 1 must exceed `sonic + 1.0` Mbps. No open row
cleared that floor. Verdict: REJECT.

## Ledger Impact

- JSON parse_only remains 11 / 17 ADMITTED and 6 OPEN.
- Open parse_only rows remain `twitter`, `github_events`, `update_center`,
  `random`, `gsoc-2018`, and `distinct_values`.
- REDRESS-230 records this generated-route rejection.
- Source remains unchanged except for the retained rejection artefacts.
