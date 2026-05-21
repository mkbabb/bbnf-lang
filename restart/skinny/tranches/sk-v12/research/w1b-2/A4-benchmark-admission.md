# SK-V12 W1b-2 A4 - Benchmark Admission Design

Date: 2026-05-20.
Phase: W1b-2 research.
Scope: Criterion measurement and admission math.

## Recommendation

Extend the existing `nonjson_css_l4` benchmark in place. Keep the W1b-1 Track 1
and cssparser oracle rows, and add a third comparator row:

```text
lightningcss_same_plane_fact_stream
```

The benchmark group should set `sample_size(30)` explicitly and retain the
existing `Throughput::Bytes(input.len() as u64)`.

## Benchmark Shape

```rust
let mut group = c.benchmark_group("nonjson_css_l4");
group.sample_size(30);
group.throughput(Throughput::Bytes(input.len() as u64));

group.bench_function("track1_generated_css_l4_decl_values", ...);
group.bench_function("track2_cssparser_oracle", ...);
group.bench_function("lightningcss_same_plane_fact_stream", |b| {
    b.iter(|| {
        black_box(
            nonjson_css_l4::lightningcss_facts(black_box(&input))
                .expect("lightningcss CSS fact stream"),
        )
    })
});
```

Pre-benchmark work must perform three-way equality and write the W1b-2 report:

```rust
nonjson_css_l4::assert_strict_equality(&input)?;
nonjson_css_l4::assert_lightningcss_strict_equality(&input)?;
let report = nonjson_css_l4::write_sota_report_with_quick_measurement()?;
report.validate_gate()?;
```

## Admission Math

Compute:

```rust
let threshold_mbps = lightningcss_measure.mbps + 1.0;
let admission_margin_mbps = track1_measure.mbps - threshold_mbps;
let css_admit = track1_measure.mbps > threshold_mbps;
```

Equality at exactly `lightningcss_mbps + 1` is a miss.

Expected native command:

```sh
RUSTFLAGS="-C target-cpu=native" \
cargo bench -p bbnf-bench --bench nonjson_css_l4 -- --sample-size 30
```

## REDRESS Evidence

REDRESS 124 should record:

- W1b-2 status: admit candidate, measured baseline, or blocked/fail.
- Fixture SHA-256 and input byte count.
- Run id and host/build fields.
- Criterion command and artifact root.
- Track 1 Mbps, cssparser oracle Mbps, lightningcss Mbps.
- Threshold `lightningcss_mbps + 1` and margin.
- Strict equality status and all three fact artifact paths.
- lightningcss version/build hash and comparator command.
- Report path and gate command result.
