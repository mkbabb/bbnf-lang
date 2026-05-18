//! PMU capture harness for SK-V9 S-P1 V3 P1-D rerun.
//!
//! Loads one JSON corpus exactly once and parses it inside a tight
//! steady-state loop. Designed to be wrapped by xctrace for per-symbol
//! hot-leaf attribution; in parallel, this binary itself reads
//! `proc_pid_rusage(RUSAGE_INFO_V5)` before/after the loop to extract
//! the real PMU `ri_cycles` and `ri_instructions` counters Apple
//! exposes through kpc — no estimation, no derived clock.
//!
//! Invocation:
//!     ./xctrace_probe <corpus_path> <track> <iters>
//!
//!     track := "track1" | "track2"
//!
//! Cold-per-parse discipline: each loop iteration calls `parse()` from
//! a freshly black-boxed `&str` view; no warmed sub-AST is retained
//! across iterations and the return value is black-boxed so the
//! optimiser cannot lift the parser out.

use std::env;
use std::path::PathBuf;
use std::time::Instant;

/// Subset of `rusage_info_v5` covering the fields we need plus enough
/// padding to satisfy the kernel write size. Apple exports the full
/// struct via `proc_pid_rusage`; we only consume `ri_cycles` and
/// `ri_instructions`, which are at the fixed offsets the public header
/// `<sys/resource.h>` documents.
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
    if args.len() < 4 {
        eprintln!("usage: xctrace_probe <corpus_path> <track:track1|track2> <iters>");
        std::process::exit(2);
    }
    let corpus_path = PathBuf::from(&args[1]);
    let track = args[2].as_str();
    let iters: u64 = args[3].parse().expect("iters must be u64");

    let bytes = std::fs::read(&corpus_path).expect("failed to read corpus");
    let input = std::str::from_utf8(&bytes).expect("corpus must be UTF-8 JSON");

    eprintln!(
        "xctrace_probe: corpus={:?} bytes={} track={} iters={}",
        corpus_path,
        bytes.len(),
        track,
        iters
    );

    // Single sanity parse to surface any parser error before the long loop.
    match track {
        "track1" => {
            runtime::generated_json::parse(input).expect("track1 sanity parse failed");
        }
        "track2" => {
            bbnf_bench::track2::json::parse(input).expect("track2 sanity parse failed");
        }
        other => panic!("unknown track {other}; expected track1|track2"),
    };

    let ri_before = read_rusage_v5();
    let start = Instant::now();
    let mut checksum: u64 = 0;
    match track {
        "track1" => {
            for _ in 0..iters {
                let root = runtime::generated_json::parse(std::hint::black_box(input))
                    .expect("track1 parse failed");
                checksum ^= root.tape().offset_bytes() as u64;
                std::hint::black_box(&root);
            }
        }
        "track2" => {
            for _ in 0..iters {
                let root = bbnf_bench::track2::json::parse(std::hint::black_box(input))
                    .expect("track2 parse failed");
                checksum ^= root.tape().offset_bytes() as u64;
                std::hint::black_box(&root);
            }
        }
        _ => unreachable!(),
    }
    let elapsed = start.elapsed();
    let ri_after = read_rusage_v5();

    let cycles = ri_after.ri_cycles - ri_before.ri_cycles;
    let instructions = ri_after.ri_instructions - ri_before.ri_instructions;
    let user_time_ns = ri_after.ri_user_time - ri_before.ri_user_time;
    let system_time_ns = ri_after.ri_system_time - ri_before.ri_system_time;
    let total_bytes = (bytes.len() as u128) * (iters as u128);
    let ns_per_byte = elapsed.as_nanos() as f64 / total_bytes as f64;
    let mbps = (total_bytes as f64 * 8.0) / (elapsed.as_secs_f64() * 1_000_000.0);
    let cycles_per_byte = cycles as f64 / total_bytes as f64;
    let cpi = cycles as f64 / instructions as f64;

    eprintln!(
        "xctrace_probe: done iters={iters} elapsed={:.3}s ns/B={ns_per_byte:.3} Mbps={mbps:.0} cycles={cycles} instrs={instructions} cycles/B={cycles_per_byte:.3} CPI={cpi:.3} checksum={checksum}",
        elapsed.as_secs_f64(),
    );
    // Single-line stdout record for the wrapper to parse.
    println!(
        "PROBE_RESULT corpus_bytes={} iters={} track={} elapsed_s={:.6} ns_per_byte={:.6} mbps={:.3} cycles={} instructions={} cycles_per_byte={:.6} cpi={:.6} user_ns={} system_ns={} checksum={}",
        bytes.len(),
        iters,
        track,
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
