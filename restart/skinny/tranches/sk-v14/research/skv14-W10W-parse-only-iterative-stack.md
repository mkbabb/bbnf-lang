# SK-V14 W10W JSON parse_only Iterative Stack

## Scope

W10W replaces the generated JSON `parse_only` recursive container walk with an
explicit iterative container stack. The change is generated from
`skinny/crates/codegen/src/runtime_generator.rs` and regenerated into
`skinny/crates/runtime/src/grammars/json/generated.rs`.

The rejected cap-16 tiny-string experiment is not included in the source
change. It moved no open row under paired Track 1 vs Skipper evidence and was
abrogated before the iterative stack measurement.

## Evidence

Generated artifacts:

- `skv14-W10W-parse-only-iterative-stack.raw.log`
  sha256 `97f1af3ee55e6e6b297539206fe7114652bc39fbd96e3d4702f90431625835a0`
- `skv14-W10W-parse-only-iterative-stack.tsv`
  sha256 `f36b112b7042db61a2ceb927abd58cbe90a14403c89daebfc888063b58b2d5ad`

Cold command shape:

```sh
RUSTC_WRAPPER= RUSTFLAGS='-C target-cpu=native' \
  cargo build --release -p bbnf-bench --bin profile_direct

./target/release/profile_direct <iters> <corpus> <parse_only_mode> 0
```

No warmup iterations were used. The open-row sweep used 400 iterations for all
seven pre-W10W open rows. `apache_builds` was then rerun at 4000 iterations for
three paired repetitions because the 400-iteration row is short.

## Admission

`json/apache_builds/parse_only/main` is admitted by W10W. The conservative
longer-run paired row is the first 4000-iteration repeat:

| corpus | iters | Track 1 | Track 2 | Skipper | serde | threshold | margin |
|---|---:|---:|---:|---:|---:|---:|---:|
| apache_builds | 4000 | 13129.331 | 9065.855 | 12951.668 | 3964.266 | 12952.668 | 176.663 |

The two additional 4000-iteration paired repeats also admit:

| repeat | Track 1 | Skipper | threshold | margin |
|---:|---:|---:|---:|---:|
| 2 | 13285.106 | 13007.626 | 13008.626 | 276.480 |
| 3 | 13305.497 | 12868.672 | 12869.672 | 435.825 |

The row uses generated Track 1 `runtime::generated_json::parse_only`, the
independent Track 2 structural oracle, strict `parse_only/sonic_rs::Skipper`,
and cold per-parse measurement with `warmup_iters=0`.

## Remaining Open

Six parse_only rows remain open after W10W:

- `twitter`
- `github_events`
- `update_center`
- `random`
- `gsoc-2018`
- `distinct_values`

Current parse_only state is 11 / 17 admitted and 6 / 17 open.
