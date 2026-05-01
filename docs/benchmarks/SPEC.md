# Benchmark Directory Spec

`docs/benchmarks/` contains **benchmark numerics only**. Wave evidence (walls, parity proofs, deletion scans, audit notes, dispatch packets) lives in the owning tranche's `docs/tranches/{LETTER}/audit/` directory, not here.

## Directory Layout

```text
docs/benchmarks/
├── SPEC.md                             # this file
├── post-{TAG}.json                     # canonical bench matrices (one per close)
├── cost-weights-sweep.json             # cost-model parameter sweep
├── iai-baselines/                      # iai-callgrind canonical baselines
│   └── {grammar}.json
├── profiles/                           # samply / instruments / perf artefacts
│   ├── {tag}-baseline/
│   ├── pre-{tag}/
│   └── post-{tag}/
└── archive/                            # historical non-matrix evidence (read-only)
    ├── {LETTER}/                       # tranche-cluster prior evidence subdirs
    └── post-{TAG}-W*.{txt,md}          # wave-level evidence with TAG prefix
```

What does NOT belong here:

- `*.txt` walls, parse logs, diagnostic dumps, `cargo expand` output;
- `*.md` progress notes, predicate tables, refs/extract probes;
- ad-hoc spot benches (`*-spot.txt`, A/B comparisons);
- per-wave evidence files (`{LETTER}-W{n}-*` walls or proofs).

These all move to `docs/tranches/{LETTER}/audit/` going forward, or to `docs/benchmarks/archive/` if they were already in benchmarks/ at the time of this spec landing.

## File Naming

### Canonical tranche close

```text
post-{TRANCHE}.json
```

Examples: `post-AU.json`, `post-AZ-III.json`, `post-AZ-IV.json`.

One per closed tranche; lands at tranche close (W3 or equivalent), archived as a permanent record. Subsequent tranches reference its rows via `floors.{prior-tag}` for row-by-row delta tracking.

### Wave-level matrices

```text
post-{TRANCHE}-{WAVE}-{kind}.json
```

Where `kind ∈ { mid, close, prototype, spot }`. Examples: `post-AW-IV-W3.json` (close at W3), `post-AY-W3-value.json` (W3 value-path matrix), `post-AZ-III-W2-prototype.json` (hypothetical prototype run).

Wave matrices land when a wave produces a measurable artefact distinct from the tranche close. They are NOT required for every wave — only for waves whose hard gate names a benchmark artefact.

### Non-matrix files

Forbidden at root or in tranche-close path. Routes to `docs/tranches/{LETTER}/audit/` (live) or `docs/benchmarks/archive/` (historical).

## Schema

```json
{
  "tag": "post-{TRANCHE}[-{WAVE}-{kind}]",
  "tranche": "{TRANCHE-LETTER}",
  "wave": "{W<N>|null}",
  "kind": "tranche-close | wave-mid | wave-close | prototype | spot",
  "date": "YYYY-MM-DD",
  "commit": "{8-40-hex}",
  "arch": "{target-triple}",
  "profile": "bench | bench-iter | release | {custom-profile-name}",
  "profile_definition": "{.cargo/config.toml lines + relevant flags}",
  "description": "{concise human prose: tranche thesis, scope, posture}",
  "bench_matrix_note": "{methodology: cold|warm, allocator, sample_size, max_time, skip_ext_time, divan/criterion/iai, harness commands}",
  "harness_carves": [
    {
      "carve": "{harness-file-path}",
      "fixture": "{name}",
      "reason": "{why carved out}",
      "evidence": "{link to wave artefact or audit doc}"
    }
  ],
  "fixtures": {
    "{family}": {
      "{fixture-name}": { "path": "{rel-path}", "bytes": <int> }
    }
  },
  "benches": {
    "{harness-name}": {
      "command": "{exact invocation}",
      "binary": "{relative target path}",
      "unit": "ns_per_iter | ns | us | ms | mb_per_s",
      "{fixture-name}": {
        "ns_per_iter": <int|null>,
        "samples": <int>,
        "iters": <int>,
        "fastest_ns": <int>,
        "slowest_ns": <int>,
        "mean_ns": <int>,
        "fixture_bytes": <int>,
        "mb_per_s": <float>,
        "status": "MEASURED | WATCHDOG_HALT | NAMED-BLOCKER",
        "limit_ns": <int|null>,
        "limit_source": "{file:line of timeout guard}",
        "note": "{free-text on regressions, profile divergence}"
      }
    }
  },
  "competitors": {
    "{harness-name}_{competitor}": {
      "tool": "sonic-rs | simd-json | lightningcss | other",
      "version": "{semver-or-commit}",
      "command": "{harness invocation}",
      "{fixture-name}": {
        "ns_per_iter": <int>,
        "ratio_vs_bbnf": <float>,
        "note": "{same-harness; methodology}"
      }
    }
  },
  "floors": {
    "{prior-tag}": {
      "source": "docs/benchmarks/{prior-tag}.json",
      "compared_at": "{date}",
      "rows_at_or_above": <int>,
      "rows_below": <int>,
      "deltas": {
        "{harness-name}.{fixture-name}": {
          "prior": <int>,
          "current": <int>,
          "delta_pct": <float>,
          "status": "AT_OR_ABOVE | BELOW | INCOMPATIBLE_PROFILE"
        }
      }
    }
  }
}
```

### Required fields per kind

| Field | tranche-close | wave-mid | wave-close | prototype | spot |
|---|:---:|:---:|:---:|:---:|:---:|
| tag, tranche, date, commit, arch | yes | yes | yes | yes | yes |
| wave | no | yes | yes | yes | optional |
| kind | yes | yes | yes | yes | yes |
| profile, profile_definition | yes | yes | yes | yes | yes |
| description, bench_matrix_note | yes | yes | yes | optional | optional |
| harness_carves | optional | optional | optional | no | no |
| fixtures | yes | yes | yes | yes | optional |
| benches | yes | yes | yes | yes | yes |
| competitors | recommended | optional | recommended | no | optional |
| floors | required at tranche-close | optional | recommended | no | no |

`tranche-close` is the only kind that REQUIRES a `floors` block — it must compare row-by-row against at least one prior `post-{X}.json` matrix. Defensible-floor close requires every row at-or-above the immediate predecessor; full close requires every row at-or-above `post-AU.json` and same-harness sonic-rs / lightningcss parity.

## Status Vocabulary

A bench row's `status` field uses one of:

| Status | Meaning | Required fields |
|---|---|---|
| `MEASURED` | row produced numeric ns_per_iter and samples | `ns_per_iter`, `samples`, `mean_ns`, `mb_per_s` |
| `WATCHDOG_HALT` | per-iter wall-clock guard halted measurement | `ns_per_iter_observed` (or `*_min`), `limit_ns`, `limit_source`, `note` |
| `NAMED-BLOCKER` | row was carved out of the harness with a routed cause | `note` (cites the routing destination), `evidence` (audit doc path) |

`status` is omitted when the row is `MEASURED` and all required fields are present (the canonical default). `WATCHDOG_HALT` and `NAMED-BLOCKER` rows MUST carry the `note` field with a routing destination.

A close that lands with any `NAMED-BLOCKER` row in a non-routable carry context (per the owning tranche's §Non-Routable Carries section) is invalid — see `docs/precepts/instructions/tranche/SPEC.md` §Hard Gates.

## Cross-Profile Comparison Rule

Two matrices using different `profile` values cannot compare row-by-row without an explicit cross-profile note in `bench_matrix_note` and a profile-conversion ratio. Examples:

- `post-AZ-III.json` is `bench-iter` (no-LTO); `post-AU.json` is `bench` (fat-LTO). Cross-profile compare requires either a fresh fat-LTO refresh or a documented bench-iter-to-bench scaling factor with profile-conversion evidence.
- `post-AZ-IV.json` (per AZ-IV.md §Hard Gates 11) lands with both fat-LTO `bench` rows and `bench-iter` rows (separate `benches` blocks under harness names suffixed `_bench` and `_bench_iter`) so post-AU floor and post-AZ-III delta both compare apples-to-apples.

## Tranche-Close Workflow (lands the canonical post-{X}.json)

1. The closing wave (typically W3 or W4) runs the bench harnesses under `[profile.bench]` (fat-LTO) at HEAD.
2. The bench command and binary are recorded per-harness; `crates/core/benches/common/timeout.rs` watchdog limits are cited per row.
3. `harness_carves` document any commented-out divan registrations with their routing destination.
4. `competitors` rows run the same fixture through the named tool (`sonic-rs::Value::from_str`, `lightningcss::stylesheet::StyleSheet::parse`, etc.) under the same harness binary.
5. `floors` block compares row-by-row against the immediate prior tranche-close (and against `post-AU.json` for the AU floor invariant).
6. The matrix is committed in the same commit as the wave-close `FINAL.md` ledger entry; the commit body cites the matrix path.

## Wave-Mid / Wave-Close Workflow

Wave-mid matrices answer "is this wave on track?". Wave-close matrices answer "did this wave land its bench-touching gates?". Both follow the same schema with `wave` and `kind` fields populated. Neither replaces the canonical `post-{TRANCHE}.json` at tranche close — that is W3-owned regardless of whether wave matrices were produced.

## Archive Policy

Files in `docs/benchmarks/archive/` are read-only historical evidence. They MAY be cited from closed tranche docs (FINAL.md, audit/) but MUST NOT be cited from active or planned tranche docs as authoritative numerics. Active tranches cite either:

- a current `docs/benchmarks/post-{X}.json` matrix at root, or
- a wave-evidence file under `docs/tranches/{LETTER}/audit/`.

If an archived file is needed for an active tranche claim, the active tranche must reproduce the measurement under its own profile and land the new matrix at root.

## Precept Reference

This spec is repo-local. The cross-repo `docs/precepts/instructions/tranche/SPEC.md` and `WAVE_SPEC.md` define how tranche/wave docs cite benchmark evidence; this file defines the bench-matrix file format itself. Wave specs that touch benchmarks (`AZ-IV/waves/W3.md` and equivalents) cite `docs/benchmarks/SPEC.md` in their §Verification Artefacts section.
