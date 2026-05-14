use bbnf_bench::metadata::{BenchFacts, HostFacts, RowMetadata};
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use std::time::Duration;
use std::{env, path::PathBuf};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn bench_structural_scan(c: &mut Criterion) {
    let host = HostFacts::probe();
    let fixtures =
        test_fixtures::load_available_bench_fixtures().expect("failed to load JSON fixture suite");
    let measurement_time_s = 5.0;
    let sample_size = 100;

    for fixture in &fixtures {
        let scalar = bbnf_bench::scan::structural_offsets_scalar(&fixture.bytes);
        let simd = bbnf_bench::scan::structural_offsets_simd(&fixture.bytes);
        let scalar_hash = bbnf_bench::scan::hash_offsets(&scalar);
        let simd_hash = bbnf_bench::scan::hash_offsets(&simd);
        assert_eq!(
            scalar_hash, simd_hash,
            "SIMD/scalar parity hash mismatch on {}",
            fixture.name
        );
        write_simd_row(&host, fixture, scalar_hash, measurement_time_s, sample_size);
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

fn write_simd_row(
    host: &HostFacts,
    fixture: &test_fixtures::JsonFixture,
    scalar_hash: String,
    measurement_time_s: f64,
    sample_size: u32,
) {
    let row = RowMetadata::from_bench(
        host,
        BenchFacts::simd_scan(
            &fixture.name,
            fixture.sha256.clone(),
            fixture.bytes.len() as u64,
            scalar_hash,
            measurement_time_s,
            sample_size,
        ),
    );
    row.write_toml(&metadata_path(&fixture.name))
        .expect("failed to write SIMD scan metadata");
}

fn metadata_path(corpus: &str) -> PathBuf {
    env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("../..")
                .join("target")
        })
        .join("criterion")
        .join("simd_structural_scan")
        .join(format!("{corpus}_simd"))
        .join("metadata.toml")
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
