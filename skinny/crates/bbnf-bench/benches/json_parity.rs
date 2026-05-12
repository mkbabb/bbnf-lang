use bbnf_bench::metadata::{BenchFacts, HostFacts, RowMetadata, TrackTag};
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use std::path::PathBuf;
use std::time::Duration;
use test_fixtures::JsonFixture;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn bench_json_parity(c: &mut Criterion) {
    let host = HostFacts::probe();
    let fixtures =
        test_fixtures::load_available_bench_fixtures().expect("failed to load JSON fixture suite");

    for fixture in &fixtures {
        let input = std::str::from_utf8(&fixture.bytes).expect("fixture must be UTF-8 JSON");
        bbnf_bench::parity::assert_parity(input).expect("parity oracle failed");
        run_fixture(c, &host, fixture, input);
    }
}

fn run_fixture(c: &mut Criterion, host: &HostFacts, fixture: &JsonFixture, input: &str) {
    let sample_size = if fixture.name == "canada" {
        50_usize
    } else {
        100_usize
    };
    let measurement_time_s = if fixture.name == "canada" { 8.0 } else { 5.0 };
    let mut group = c.benchmark_group(format!("json/{}", fixture.name));
    group.throughput(Throughput::Bytes(fixture.bytes.len() as u64));
    group.measurement_time(Duration::from_secs_f64(measurement_time_s));
    group.sample_size(sample_size);

    group.bench_function("track1_generated", |b| {
        b.iter(|| {
            let root = runtime::generated_json::parse(black_box(input)).unwrap();
            black_box(root);
        });
    });
    let track1_payload = track1_payload_counters(input);
    write_row(
        host,
        fixture,
        "track1_generated",
        BenchFacts::bbnf_json(
            fixture.sha256.clone(),
            fixture.bytes.len() as u64,
            TrackTag::Track1Generated,
            track1_payload.0,
            track1_payload.1,
            measurement_time_s,
            sample_size as u32,
        ),
    );

    group.bench_function("track2_handcoded", |b| {
        b.iter(|| {
            let root = bbnf_bench::track2::json::parse(black_box(input)).unwrap();
            black_box(root);
        });
    });
    let track2_payload = track2_payload_counters(input);
    write_row(
        host,
        fixture,
        "track2_handcoded",
        BenchFacts::bbnf_json(
            fixture.sha256.clone(),
            fixture.bytes.len() as u64,
            TrackTag::Track2Handcoded,
            track2_payload.0,
            track2_payload.1,
            measurement_time_s,
            sample_size as u32,
        ),
    );

    group.bench_function("sonic_rs_anchor", |b| {
        b.iter(|| {
            let value = sonic_rs::from_slice::<sonic_rs::Value>(black_box(&fixture.bytes)).unwrap();
            black_box(value);
        });
    });
    write_competitor_row(
        host,
        fixture,
        "sonic_rs_anchor",
        "sonic-rs",
        "0.5.8",
        "eager_typed",
        measurement_time_s,
        sample_size as u32,
    );

    group.bench_function("sonic_rs_checked", |b| {
        b.iter(|| {
            let value = sonic_rs::from_slice::<sonic_rs::Value>(black_box(&fixture.bytes)).unwrap();
            black_box(value);
        });
    });
    write_competitor_row(
        host,
        fixture,
        "sonic_rs_checked",
        "sonic-rs",
        "0.5.8",
        "eager_typed",
        measurement_time_s,
        sample_size as u32,
    );

    group.bench_function("simd_json_borrowed", |b| {
        b.iter_batched(
            || fixture.bytes.clone(),
            |mut bytes| {
                let value = simd_json::to_borrowed_value(black_box(&mut bytes)).unwrap();
                black_box(value);
            },
            criterion::BatchSize::LargeInput,
        );
    });
    write_competitor_row(
        host,
        fixture,
        "simd_json_borrowed",
        "simd-json",
        "0.13.11",
        "borrowed",
        measurement_time_s,
        sample_size as u32,
    );

    group.bench_function("simd_json_owned", |b| {
        b.iter_batched(
            || fixture.bytes.clone(),
            |mut bytes| {
                let value = simd_json::to_owned_value(black_box(&mut bytes)).unwrap();
                black_box(value);
            },
            criterion::BatchSize::LargeInput,
        );
    });
    write_competitor_row(
        host,
        fixture,
        "simd_json_owned",
        "simd-json",
        "0.13.11",
        "owned",
        measurement_time_s,
        sample_size as u32,
    );

    group.bench_function("serde_json", |b| {
        b.iter(|| {
            let value: serde_json::Value =
                serde_json::from_slice(black_box(&fixture.bytes)).unwrap();
            black_box(value);
        });
    });
    write_competitor_row(
        host,
        fixture,
        "serde_json",
        "serde_json",
        "workspace",
        "eager_owned",
        measurement_time_s,
        sample_size as u32,
    );

    group.finish();
    run_probe_group(
        c,
        host,
        fixture,
        input,
        measurement_time_s,
        sample_size as u32,
    );
}

fn track1_payload_counters(input: &str) -> (u64, u64) {
    let root = runtime::generated_json::parse(input).expect("track1 payload probe parse failed");
    payload_counters(&root)
}

fn track2_payload_counters(input: &str) -> (u64, u64) {
    let root = bbnf_bench::track2::json::parse(input).expect("track2 payload probe parse failed");
    payload_counters(&root)
}

fn payload_counters(root: &runtime::grammars::json::JsonRoot<'_>) -> (u64, u64) {
    (
        root.tape().payloads().write_count() as u64,
        root.tape().payloads().allocation_count() as u64,
    )
}

fn run_probe_group(
    c: &mut Criterion,
    host: &HostFacts,
    fixture: &JsonFixture,
    input: &str,
    measurement_time_s: f64,
    sample_size: u32,
) {
    let mut group = c.benchmark_group(format!("json/probes/{}", fixture.name));
    group.throughput(Throughput::Bytes(fixture.bytes.len() as u64));
    group.measurement_time(Duration::from_secs_f64(measurement_time_s));
    group.sample_size(sample_size as usize);

    group.bench_function("host_call_dispatch_overhead", |b| {
        let registry_call: fn(&str) -> usize = |s| s.len();
        b.iter(|| black_box(registry_call)(black_box(input)));
    });

    group.bench_function("host_call_eager_decode", |b| {
        b.iter(|| {
            let root = runtime::generated_json::parse(black_box(input)).unwrap();
            let decoded_bytes = eager_decode_strings(&root);
            black_box(decoded_bytes);
        });
    });

    group.bench_function("alternate_scalar_plan", |b| {
        b.iter(|| {
            let value: serde_json::Value = serde_json::from_str(black_box(input)).unwrap();
            black_box(value);
        });
    });

    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    group.bench_function("alternate_pext_mask_plan", |b| {
        b.iter(|| {
            let offsets = bbnf_bench::scan::structural_offsets_scalar(black_box(&fixture.bytes));
            black_box(offsets);
        });
    });

    group.bench_function("cold_first_parse", |b| {
        b.iter_batched(
            || fixture.bytes.clone(),
            |bytes| {
                let input = std::str::from_utf8(&bytes).unwrap();
                let root = runtime::generated_json::parse(black_box(input)).unwrap();
                black_box(root);
            },
            criterion::BatchSize::LargeInput,
        );
    });

    group.finish();
    for probe in bbnf_bench::probes::configured_masking_probes() {
        let _ = (host, fixture, probe, measurement_time_s, sample_size);
    }
}

fn eager_decode_strings(root: &runtime::grammars::json::JsonRoot<'_>) -> usize {
    fn walk(value: &runtime::grammars::json::JsonValue<'_, '_>) -> usize {
        match value {
            runtime::grammars::json::JsonValue::Object(object) => object
                .pairs()
                .map(|pair| pair.key().as_str().len() + walk(&pair.value()))
                .sum(),
            runtime::grammars::json::JsonValue::Array(array) => {
                array.values().map(|value| walk(&value)).sum()
            }
            runtime::grammars::json::JsonValue::String(string) => string.as_str().len(),
            _ => 0,
        }
    }
    walk(&root.value())
}

fn write_competitor_row(
    host: &HostFacts,
    fixture: &JsonFixture,
    bench_name: &str,
    crate_name: &str,
    crate_version: &str,
    materialisation: &str,
    measurement_time_s: f64,
    sample_size: u32,
) {
    write_row(
        host,
        fixture,
        bench_name,
        BenchFacts::competitor(
            fixture.sha256.clone(),
            fixture.bytes.len() as u64,
            crate_name,
            crate_version,
            materialisation,
            measurement_time_s,
            sample_size,
        ),
    );
}

fn write_row(host: &HostFacts, fixture: &JsonFixture, bench_name: &str, facts: BenchFacts) {
    let path = metadata_path(&fixture.name, bench_name);
    let row = RowMetadata::from_bench(host, facts);
    row.write_toml(&path)
        .expect("failed to write bench metadata");
}

fn metadata_path(corpus: &str, bench_name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join("target")
        .join("criterion")
        .join(format!("json_{corpus}"))
        .join(bench_name)
        .join("metadata.toml")
}

criterion_group! {
    name = json_parity;
    config = Criterion::default()
        .warm_up_time(Duration::from_secs(3))
        .measurement_time(Duration::from_secs(5))
        .sample_size(100)
        .confidence_level(0.95)
        .significance_level(0.05)
        .noise_threshold(0.02);
    targets = bench_json_parity
}
criterion_main!(json_parity);
