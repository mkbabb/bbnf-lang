use bbnf_bench::nonjson_css_l4;
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};

fn bench_nonjson_css_l4(c: &mut Criterion) {
    let input = nonjson_css_l4::read_fixture().expect("CSS L4 fixture is readable");
    nonjson_css_l4::assert_strict_equality(&input).expect("CSS Track 1 equals cssparser oracle");
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
    group.finish();
}

criterion_group!(benches, bench_nonjson_css_l4);
criterion_main!(benches);
