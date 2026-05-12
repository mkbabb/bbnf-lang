use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use std::time::Duration;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn bench_structural_scan(c: &mut Criterion) {
    let fixtures =
        test_fixtures::load_available_bench_fixtures().expect("failed to load JSON fixture suite");

    for fixture in &fixtures {
        let scalar = bbnf_bench::scan::structural_offsets_scalar(&fixture.bytes);
        let simd = bbnf_bench::scan::structural_offsets_simd(&fixture.bytes);
        assert_eq!(
            bbnf_bench::scan::hash_offsets(&scalar),
            bbnf_bench::scan::hash_offsets(&simd),
            "SIMD/scalar parity hash mismatch on {}",
            fixture.name
        );
    }

    let mut group = c.benchmark_group("simd/structural_scan");
    for fixture in &fixtures {
        group.throughput(Throughput::Bytes(fixture.bytes.len() as u64));
        group.bench_function(format!("{}/simd", fixture.name), |b| {
            b.iter(|| bbnf_bench::scan::structural_offsets_simd(black_box(&fixture.bytes)));
        });
        group.bench_function(format!("{}/scalar", fixture.name), |b| {
            b.iter(|| bbnf_bench::scan::structural_offsets_scalar(black_box(&fixture.bytes)));
        });
    }
    group.finish();
}

criterion_group! {
    name = simd_scan;
    config = Criterion::default()
        .warm_up_time(Duration::from_secs(3))
        .measurement_time(Duration::from_secs(5))
        .sample_size(100)
        .confidence_level(0.95)
        .significance_level(0.05)
        .noise_threshold(0.02);
    targets = bench_structural_scan
}
criterion_main!(simd_scan);
