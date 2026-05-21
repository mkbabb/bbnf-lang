# SK-V12 W1b-2b A3 - Criterion Estimate Consumption

Date: 2026-05-20.
Scope: how W1b-2b should consume the post-W1b-2a Criterion artifacts for
`nonjson_css_l4`.

## Inputs

Read the three Criterion lanes under:

```text
skinny/target/criterion/nonjson_css_l4/
```

The relevant `new/` lanes are:

| Lane | Meaning | Required files |
|---|---|---|
| `track1_generated_css_l4_decl_values` | Generated Track 1 CSS L4 declaration-value parser/fact producer. | `new/benchmark.json`, `new/estimates.json`, `new/sample.json` |
| `track2_cssparser_oracle` | Independent cssparser oracle, same output plane as Track 1. | `new/benchmark.json`, `new/estimates.json`, `new/sample.json` |
| `lightningcss_same_plane_fact_stream` | lightningcss comparator using the W1b-2a same-plane source-sidecar extractor. | `new/benchmark.json`, `new/estimates.json`, `new/sample.json` |

Do not consume `base/` for the gate decision. `base/` is Criterion history.
Use `new/` as the current run. `change/` is advisory only and should not be a
source of Mbps.

## Current Criterion Estimates

Each lane declares `throughput: {"Bytes":187}` in `benchmark.json`. The mean
estimate is nanoseconds per iteration from `estimates.json`:
`mean.point_estimate`.

| Lane | Bytes | Samples | Mean ns/iter | Mean Mbps |
|---|---:|---:|---:|---:|
| Track 1 generated | 187 | 30 | 3484.383794 | 429.344208 |
| cssparser oracle | 187 | 30 | 6880.481226 | 217.426652 |
| lightningcss same-plane | 187 | 30 | 8855.758871 | 168.929622 |

Mean Mbps is derived, not stored:

```text
Mbps = bytes * 8_000 / mean_ns
```

The `8_000` factor is `8 bits/byte * 1000 ns/us * 1000 us/ms * 1000 ms/s /
1_000_000 bits/Mbit`, reduced to the Criterion unit shape where `mean_ns` is
nanoseconds per iteration. For 187 bytes and 3484.383794 ns, Track 1 is
`187 * 8000 / 3484.383794 = 429.344208 Mbps`.

The confidence interval can be converted the same way, with bounds inverted
because lower time means higher throughput:

```text
Mbps lower = bytes * 8_000 / mean_ns_upper_bound
Mbps upper = bytes * 8_000 / mean_ns_lower_bound
```

Current 95 percent CI in Mbps:

| Lane | Mbps lower | Mbps upper |
|---|---:|---:|
| Track 1 generated | 427.419885 | 430.891216 |
| cssparser oracle | 216.808596 | 217.966097 |
| lightningcss same-plane | 168.575023 | 169.283982 |

## Sample Counts

Use `sample.json.iters.length` as the Criterion sample count. The W1b-2b gate
should require at least 30 samples for each consumed lane. The current run has
30 samples for Track 1, cssparser, and lightningcss.

Do not confuse Criterion sample count with the W1b-1 quick-measurement
`sample_count` in `skv12-W1b-1-css-l4-oracle.json`. That report's `sample_count`
is 2000 loop iterations from `write_report_with_quick_measurement()`, not
Criterion's configured `sample_size(30)`.

## Reading The Three Lanes

Track 1 is the candidate. It must provide:

- the current `new/estimates.json` mean estimate;
- matching 187-byte throughput metadata;
- at least 30 Criterion samples;
- retained equality artifacts showing the emitted fact stream is byte-identical
  to the oracles.

cssparser is the independent oracle. It is not the SOTA comparator for the
W1b-2b admission claim, but it remains the strict same-plane correctness
anchor. If Track 1 and lightningcss agree with each other but cssparser does
not agree, the result is invalid rather than fast.

lightningcss is the SOTA comparator. Consume its Criterion mean exactly like
Track 1: same 187-byte throughput payload, same `mean.point_estimate` field,
same Mbps formula, same sample-count rule. The W1b-2b admission threshold from
W1b-2 A3 is:

```text
threshold_mbps = lightningcss_mbps + 1
admission_margin_mbps = track1_mbps - threshold_mbps
```

With the current Criterion means:

```text
threshold_mbps = 168.929622 + 1 = 169.929622
admission_margin_mbps = 429.344208 - 169.929622 = 259.414586
```

That is an admission candidate only if the three-way equality proof and all
freshness checks also pass.

## Fail-Closed Behavior

Missing files fail closed. If any consumed lane lacks `new/benchmark.json`,
`new/estimates.json`, or `new/sample.json`, no fallback to `base/`, report JSON,
or hand-entered Mbps is allowed.

Malformed files fail closed. Unknown schema shape, missing `throughput.Bytes`,
non-positive bytes, missing `mean.point_estimate`, non-finite or non-positive
mean, or sample count below 30 must reject the gate.

Stale files fail closed. The consumed report identity must bind the current
run id into artifact paths, as the current helper does for `track1_artifact` and
`benchmark_artifact_path`. A report whose run id does not appear in its
Criterion artifact references is stale evidence. A report whose generated
source, runtime path, fixture checksum, byte count, oracle source, or output
plane does not match the CSS L4 declaration-values row is stale or coupled
evidence.

Equality failures fail closed. The W1b-2b report can only admit Track 1 against
lightningcss when Track 1, cssparser, and lightningcss retained fact artifacts
are byte-identical on the declared output plane. Missing retained fact
artifacts, failed `strict_output_equality`, or a missing lightningcss AST
cross-check are correctness failures, not performance baselines.

The only non-admission success state is a measured baseline: equality,
freshness, sample count, and Criterion consumption all pass, but
`track1_mbps <= lightningcss_mbps + 1`. In that case the result should be
reported as measured evidence, not as a SOTA admission.
