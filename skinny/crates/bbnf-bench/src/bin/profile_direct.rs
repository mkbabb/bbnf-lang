//! Cold profiling binary for direct, typed, and parse-only workload gates.
//!
//! Invocation:
//!     cargo build --release -p bbnf-bench --bin profile_direct
//!     samply record ./target/release/profile_direct 10000 twitter parse_only_track1

use std::env;
use std::path::PathBuf;
use std::time::Instant;

#[repr(C)]
#[derive(Default, Clone, Copy)]
struct RusageInfoV5 {
    ri_uuid: [u8; 16],
    ri_user_time: u64,
    ri_system_time: u64,
    ri_pkg_idle_wkups: u64,
    ri_interrupt_wkups: u64,
    ri_pageins: u64,
    ri_wired_size: u64,
    ri_resident_size: u64,
    ri_phys_footprint: u64,
    ri_proc_start_abstime: u64,
    ri_proc_exit_abstime: u64,
    ri_child_user_time: u64,
    ri_child_system_time: u64,
    ri_child_pkg_idle_wkups: u64,
    ri_child_interrupt_wkups: u64,
    ri_child_pageins: u64,
    ri_child_elapsed_abstime: u64,
    ri_diskio_bytesread: u64,
    ri_diskio_byteswritten: u64,
    ri_cpu_time_qos_default: u64,
    ri_cpu_time_qos_maintenance: u64,
    ri_cpu_time_qos_background: u64,
    ri_cpu_time_qos_utility: u64,
    ri_cpu_time_qos_legacy: u64,
    ri_cpu_time_qos_user_initiated: u64,
    ri_cpu_time_qos_user_interactive: u64,
    ri_billed_system_time: u64,
    ri_serviced_system_time: u64,
    ri_logical_writes: u64,
    ri_lifetime_max_phys_footprint: u64,
    ri_instructions: u64,
    ri_cycles: u64,
    ri_billed_energy: u64,
    ri_serviced_energy: u64,
    ri_interval_max_phys_footprint: u64,
    ri_runnable_time: u64,
    ri_flags: u64,
}

const RUSAGE_INFO_V5: i32 = 5;

extern "C" {
    fn proc_pid_rusage(pid: libc::pid_t, flavor: i32, buffer: *mut u8) -> i32;
}

fn read_rusage_v5() -> RusageInfoV5 {
    let mut ri = RusageInfoV5::default();
    let rc = unsafe {
        proc_pid_rusage(
            libc::getpid(),
            RUSAGE_INFO_V5,
            (&mut ri) as *mut RusageInfoV5 as *mut u8,
        )
    };
    if rc != 0 {
        panic!("proc_pid_rusage failed rc={rc}");
    }
    ri
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let iters: usize = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(10_000);
    let corpus = args
        .get(2)
        .cloned()
        .unwrap_or_else(|| "twitter".to_string());
    let mode = args.get(3).map(String::as_str).unwrap_or("track1");
    let warmup_iters: usize = args.get(4).and_then(|s| s.parse().ok()).unwrap_or(0);
    let path = if corpus.contains('/') || corpus.ends_with(".json") {
        PathBuf::from(&corpus)
    } else if mode.starts_with("real_typed_") || mode.starts_with("direct_strict_") {
        bbnf_bench::real_typed_struct::locate_fixture(&corpus)
    } else {
        locate_fixture(&corpus)
    };

    eprintln!(
        "profile-direct: corpus={corpus} mode={mode} path={path:?} iters={iters} warmup_iters={warmup_iters}"
    );
    let bytes = std::fs::read(&path).expect("failed to read fixture");
    let input = std::str::from_utf8(&bytes).expect("fixture is not UTF-8");
    eprintln!("profile-direct: fixture size = {} bytes", bytes.len());

    for _ in 0..warmup_iters {
        run_once(mode, &corpus, input, &bytes);
    }

    eprintln!("profile-direct: starting timed loop");
    let ri_before = read_rusage_v5();
    let start = Instant::now();
    let mut checksum = 0_u64;
    for _ in 0..iters {
        checksum ^= run_once(
            mode,
            &corpus,
            std::hint::black_box(input),
            std::hint::black_box(&bytes),
        );
    }
    let elapsed = start.elapsed();
    let ri_after = read_rusage_v5();
    let total_bytes = (bytes.len() as u128) * (iters as u128);
    let mbps = (total_bytes as f64 * 8.0) / (elapsed.as_secs_f64() * 1_000_000.0);
    let cycles = ri_after.ri_cycles - ri_before.ri_cycles;
    let instructions = ri_after.ri_instructions - ri_before.ri_instructions;
    let user_time_ns = ri_after.ri_user_time - ri_before.ri_user_time;
    let system_time_ns = ri_after.ri_system_time - ri_before.ri_system_time;
    let ns_per_byte = elapsed.as_nanos() as f64 / total_bytes as f64;
    let cycles_per_byte = cycles as f64 / total_bytes as f64;
    let cpi = if instructions == 0 {
        0.0
    } else {
        cycles as f64 / instructions as f64
    };
    eprintln!(
        "profile-direct: {iters} iters in {:.2}s -> {:.0} Mbps (digest cksum {checksum})",
        elapsed.as_secs_f64(),
        mbps
    );
    println!(
        "PROBE_RESULT corpus={} corpus_bytes={} iters={} mode={} warmup_iters={} elapsed_s={:.6} ns_per_byte={:.6} mbps={:.3} cycles={} instructions={} cycles_per_byte={:.6} cpi={:.6} user_ns={} system_ns={} checksum={}",
        corpus,
        bytes.len(),
        iters,
        mode,
        warmup_iters,
        elapsed.as_secs_f64(),
        ns_per_byte,
        mbps,
        cycles,
        instructions,
        cycles_per_byte,
        cpi,
        user_time_ns,
        system_time_ns,
        checksum
    );
}

fn run_once(mode: &str, corpus: &str, input: &str, bytes: &[u8]) -> u64 {
    let digest = match mode {
        "track1" => bbnf_bench::direct_struct::track1_digest(input),
        "track2" => bbnf_bench::direct_struct::track2_digest(input),
        "sonic" => bbnf_bench::direct_struct::sonic_digest(bytes),
        "serde" => bbnf_bench::direct_struct::serde_digest(bytes),
        "real_typed_track1" | "real_typed_track2" | "real_typed_sonic" | "real_typed_serde" => {
            return real_typed_checksum(corpus, mode, input, bytes);
        }
        "direct_strict_track1"
        | "direct_strict_track2"
        | "direct_strict_sonic"
        | "direct_strict_serde" => {
            return direct_strict_checksum(corpus, mode, input, bytes);
        }
        "parse_only_track1" | "parse_only_track2" | "parse_only_sonic" | "parse_only_serde" => {
            return parse_only_checksum(mode, input, bytes);
        }
        other => panic!(
            "unknown mode {other}; expected track1|track2|sonic|serde|real_typed_track1|real_typed_track2|real_typed_sonic|real_typed_serde|direct_strict_track1|direct_strict_track2|direct_strict_sonic|direct_strict_serde|parse_only_track1|parse_only_track2|parse_only_sonic|parse_only_serde"
        ),
    }
    .expect("direct digest failed");
    std::hint::black_box(
        digest.fingerprint
            ^ digest.objects
            ^ digest.arrays
            ^ digest.strings
            ^ digest.numbers
            ^ digest.string_bytes,
    )
}

fn real_typed_checksum(corpus: &str, mode: &str, input: &str, bytes: &[u8]) -> u64 {
    let fixture = bbnf_bench::real_typed_struct::fixture_for_name(corpus)
        .unwrap_or_else(|| panic!("real typed mode {mode} does not support corpus {corpus}"));
    let output = match mode {
        "real_typed_track1" => bbnf_bench::real_typed_struct::track1_typed(fixture, input),
        "real_typed_track2" => bbnf_bench::real_typed_struct::track2_typed(fixture, input),
        "real_typed_sonic" => bbnf_bench::real_typed_struct::sonic_typed(fixture, bytes),
        "real_typed_serde" => bbnf_bench::real_typed_struct::serde_typed(fixture, bytes),
        _ => unreachable!("real_typed_checksum called for non-real-typed mode"),
    }
    .expect("real typed parse failed");
    std::hint::black_box(bbnf_bench::real_typed_struct::typed_checksum(&output))
}

fn direct_strict_checksum(corpus: &str, mode: &str, input: &str, bytes: &[u8]) -> u64 {
    let checksum = match mode {
        "direct_strict_track1" => bbnf_bench::direct_struct::track1_strict_product(corpus, input),
        "direct_strict_track2" => bbnf_bench::direct_struct::track2_strict_product(corpus, input),
        "direct_strict_sonic" => bbnf_bench::direct_struct::sonic_strict_product(corpus, bytes),
        "direct_strict_serde" => bbnf_bench::direct_struct::serde_strict_product(corpus, bytes),
        _ => unreachable!("direct_strict_checksum called for non-direct-strict mode"),
    }
    .expect("direct strict product failed");
    std::hint::black_box(checksum)
}

fn parse_only_checksum(mode: &str, input: &str, bytes: &[u8]) -> u64 {
    match mode {
        "parse_only_track1" => {
            runtime::generated_json::parse_only(input).expect("generated parse_only failed");
            std::hint::black_box(bytes.len() as u64)
        }
        "parse_only_track2" => {
            let root = bbnf_bench::track2::json::parse(input).expect("track2 parse failed");
            std::hint::black_box(
                fnv_u32(root.tape().offsets())
                    ^ fnv_u32(root.tape().flag_cursors())
                    ^ fnv_u8(root.tape().flag_values())
                    ^ root.tape().payloads().write_count() as u64
                    ^ root.tape().payloads().allocation_count() as u64,
            )
        }
        "parse_only_sonic" => {
            bbnf_bench::sonic_skipper::parse_only(bytes).expect("sonic skipper failed");
            std::hint::black_box(bytes.len() as u64)
        }
        "parse_only_serde" => {
            let value =
                serde_json::from_slice::<serde_json::Value>(bytes).expect("serde_json failed");
            std::hint::black_box(value_type_tag(&value) ^ bytes.len() as u64)
        }
        _ => unreachable!("parse_only_checksum called for non-parse-only mode"),
    }
}

fn value_type_tag(value: &serde_json::Value) -> u64 {
    match value {
        serde_json::Value::Null => 0x00,
        serde_json::Value::Bool(value) => u64::from(*value),
        serde_json::Value::Number(_) => 0x02,
        serde_json::Value::String(value) => 0x53 ^ value.len() as u64,
        serde_json::Value::Array(values) => 0x5b ^ values.len() as u64,
        serde_json::Value::Object(values) => 0x7b ^ values.len() as u64,
    }
}

fn fnv_u32(values: &[u32]) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for value in values {
        for byte in value.to_le_bytes() {
            hash ^= u64::from(byte);
            hash = hash.wrapping_mul(0x100000001b3);
        }
    }
    hash
}

fn fnv_u8(values: &[u8]) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for value in values {
        hash ^= u64::from(*value);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

fn locate_fixture(name: &str) -> PathBuf {
    let manifest = env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| env::current_dir().unwrap());
    for dir in manifest.ancestors() {
        let candidate = dir
            .join("crates/test-fixtures/corpus/json")
            .join(format!("{name}.json"));
        if candidate.exists() {
            return candidate;
        }
        let candidate = dir.join("test_data").join(format!("{name}.json"));
        if candidate.exists() {
            return candidate;
        }
        let candidate = dir
            .join("test_data")
            .join(format!("{}.json", name.replace('_', "-")));
        if candidate.exists() {
            return candidate;
        }
    }
    panic!("could not locate fixture {name}.json under crates/test-fixtures/corpus/json");
}
