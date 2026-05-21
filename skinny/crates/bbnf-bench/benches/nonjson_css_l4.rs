use bbnf_bench::nonjson_css_l4;
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use std::time::Duration;

fn bench_nonjson_css_l4(c: &mut Criterion) {
    let input = nonjson_css_l4::read_fixture().expect("CSS L4 fixture is readable");
    nonjson_css_l4::assert_strict_equality(&input).expect("CSS Track 1 equals cssparser oracle");
    nonjson_css_l4::assert_lightningcss_strict_equality(&input)
        .expect("CSS Track 1 equals lightningcss fact stream");
    let report =
        nonjson_css_l4::write_report_with_quick_measurement().expect("CSS L4 report is emitted");
    report.validate_gate().expect("CSS L4 report gate passes");

    let mut group = c.benchmark_group("nonjson_css_l4");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("track1_generated_css_l4_decl_values", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::track1_facts(black_box(&input)).expect("Track 1 CSS fact stream"),
            )
        })
    });
    group.bench_function("track2_cssparser_oracle", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::oracle_facts(black_box(&input)).expect("cssparser CSS fact stream"),
            )
        })
    });
    group.bench_function("lightningcss_same_plane_fact_stream", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::lightningcss_facts(black_box(&input))
                    .expect("lightningcss CSS fact stream"),
            )
        })
    });
    group.finish();
}

criterion_group! {
    name = benches;
    config = Criterion::default()
        .warm_up_time(Duration::from_secs(3))
        .measurement_time(Duration::from_secs(5))
        .sample_size(30)
        .confidence_level(0.95)
        .significance_level(0.05)
        .noise_threshold(0.02);
    targets = bench_nonjson_css_l4
}
criterion_main!(benches);
