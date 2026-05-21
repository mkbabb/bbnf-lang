use std::env;
use std::hint::black_box;
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
    let rc = unsafe { proc_pid_rusage(libc::getpid(), RUSAGE_INFO_V5, (&mut ri) as *mut RusageInfoV5 as *mut u8) };
    if rc != 0 {
        panic!("proc_pid_rusage failed rc={rc}");
    }
    ri
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let iters: usize = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(1000);
    let corpus = args.get(2).cloned().unwrap_or_else(|| "twitter".to_string());
    let mode = args.get(3).map(String::as_str).unwrap_or("host_call_eager_decode");
    if mode == "alternate_pext_mask_plan" || mode == "alternate_dispatch_table_plan" {
        println!("UNSUPPORTED corpus={corpus} mode={mode} reason=aarch64_or_disabled_probe");
        return;
    }
    let path = locate_fixture(&corpus);
    eprintln!("mode3-profiler: corpus={corpus} mode={mode} path={path:?} iters={iters}");
    let bytes = std::fs::read(&path).expect("failed to read fixture");
    let input = std::str::from_utf8(&bytes).expect("fixture is not UTF-8");
    for _ in 0..8 {
        black_box(run_once(mode, input, &bytes));
    }
    eprintln!("mode3-profiler: starting timed loop");
    let ri_before = read_rusage_v5();
    let start = Instant::now();
    let mut checksum = 0usize;
    for _ in 0..iters {
        checksum ^= run_once(mode, black_box(input), black_box(&bytes));
    }
    let elapsed = start.elapsed();
    let ri_after = read_rusage_v5();
    let total_bytes = (bytes.len() as u128) * (iters as u128);
    let mbps = (total_bytes as f64 * 8.0) / (elapsed.as_secs_f64() * 1_000_000.0);
    let cycles = ri_after.ri_cycles - ri_before.ri_cycles;
    let instructions = ri_after.ri_instructions - ri_before.ri_instructions;
    let cycles_per_byte = cycles as f64 / total_bytes as f64;
    let cpi = if instructions == 0 { 0.0 } else { cycles as f64 / instructions as f64 };
    println!(
        "PROBE_RESULT corpus={} corpus_bytes={} iters={} mode={} elapsed_s={:.6} mbps={:.3} cycles={} instructions={} cycles_per_byte={:.6} cpi={:.6} user_ns={} system_ns={} checksum={}",
        corpus,
        bytes.len(),
        iters,
        mode,
        elapsed.as_secs_f64(),
        mbps,
        cycles,
        instructions,
        cycles_per_byte,
        cpi,
        ri_after.ri_user_time - ri_before.ri_user_time,
        ri_after.ri_system_time - ri_before.ri_system_time,
        checksum
    );
}

fn run_once(mode: &str, input: &str, bytes: &[u8]) -> usize {
    match mode {
        "host_call_dispatch_overhead" => input.len(),
        "host_call_eager_decode" => {
            let root = runtime::generated_json::parse(input).expect("generated parse failed");
            eager_decode_strings(&root)
        }
        "alternate_scalar_plan" => {
            let value: serde_json::Value = serde_json::from_str(input).expect("serde scalar parse failed");
            value_fingerprint(&value)
        }
        "cold_first_parse" => {
            let cloned = bytes.to_vec();
            let cloned_input = std::str::from_utf8(&cloned).expect("cloned fixture is UTF-8");
            let root = runtime::generated_json::parse(cloned_input).expect("cold generated parse failed");
            root.tape().offsets().len()
        }
        "structural_scan_scalar" => bbnf_bench::scan::structural_offsets_scalar(bytes).len(),
        "structural_scan_simd" => bbnf_bench::scan::structural_offsets_simd(bytes).len(),
        other => panic!("unknown mode {other}"),
    }
}

fn eager_decode_strings(root: &runtime::grammars::json::JsonRoot<'_>) -> usize {
    fn walk(value: &runtime::grammars::json::JsonValue<'_, '_>) -> usize {
        match value {
            runtime::grammars::json::JsonValue::Object(object) => object
                .pairs()
                .map(|pair| pair.key().as_str().len() + walk(&pair.value()))
                .sum(),
            runtime::grammars::json::JsonValue::Array(array) => array.values().map(|value| walk(&value)).sum(),
            runtime::grammars::json::JsonValue::String(string) => string.as_str().len(),
            _ => 0,
        }
    }
    walk(&root.value())
}

fn value_fingerprint(value: &serde_json::Value) -> usize {
    match value {
        serde_json::Value::Null => 1,
        serde_json::Value::Bool(value) => usize::from(*value) + 2,
        serde_json::Value::Number(number) => number.to_string().len(),
        serde_json::Value::String(string) => string.len(),
        serde_json::Value::Array(array) => array.iter().map(value_fingerprint).sum::<usize>() ^ array.len(),
        serde_json::Value::Object(object) => object.iter().map(|(k, v)| k.len() ^ value_fingerprint(v)).sum::<usize>() ^ object.len(),
    }
}

fn locate_fixture(name: &str) -> PathBuf {
    let skinny = PathBuf::from("/Users/mkbabb/Programming/bbnf-lang/skinny");
    match name {
        "twitter" | "citm_catalog" | "canada" => skinny.join("crates/test-fixtures/corpus/json").join(format!("{name}.json")),
        "update_center" => skinny.join("test_data/update-center.json"),
        _ => skinny.join("test_data").join(format!("{name}.json")),
    }
}
