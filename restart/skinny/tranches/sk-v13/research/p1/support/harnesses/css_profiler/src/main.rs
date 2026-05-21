use std::hint::black_box;
use std::time::Instant;

#[cfg(target_os = "macos")]
fn rusage_user_system_ns() -> (i128, i128) {
    unsafe {
        let mut usage = std::mem::MaybeUninit::<libc::rusage_info_v4>::uninit();
        if libc::proc_pid_rusage(libc::getpid(), libc::RUSAGE_INFO_V4, usage.as_mut_ptr() as *mut _) != 0 {
            return (-1, -1);
        }
        let usage = usage.assume_init();
        (usage.ri_user_time as i128, usage.ri_system_time as i128)
    }
}

#[cfg(not(target_os = "macos"))]
fn rusage_user_system_ns() -> (i128, i128) { (-1, -1) }

fn measure<F>(input: &str, iterations: usize, mut f: F) -> Result<(f64, u128, i128, i128, usize), String>
where
    F: FnMut(&str) -> Result<String, String>,
{
    let (u0, s0) = rusage_user_system_ns();
    let started = Instant::now();
    let mut bytes = 0usize;
    let mut out_len = 0usize;
    for _ in 0..iterations {
        let out = f(black_box(input))?;
        out_len ^= out.len();
        bytes += input.len();
        black_box(&out);
    }
    let elapsed = started.elapsed().as_nanos();
    let (u1, s1) = rusage_user_system_ns();
    let mbps = (bytes as f64) / (elapsed as f64 / 1_000_000_000.0) / 1_000_000.0;
    Ok((mbps, elapsed, u1 - u0, s1 - s0, out_len))
}

fn main() -> Result<(), String> {
    let iterations = std::env::args().nth(1).and_then(|s| s.parse::<usize>().ok()).unwrap_or(50_000);
    let input = bbnf_bench::nonjson_css_l4::read_fixture().map_err(|e| e.to_string())?;
    let (track1, oracle, lightning) = bbnf_bench::nonjson_css_l4::assert_lightningcss_strict_equality(&input)?;
    println!("strict_equality\tpass\ttrack1_len\t{}\toracle_len\t{}\tlightningcss_len\t{}\tbytes\t{}\titerations\t{}", track1.len(), oracle.len(), lightning.len(), input.len(), iterations);
    for (mode, mbps, elapsed, user, sys, out_len) in [
        {
            let (mbps, elapsed, user, sys, out_len) = measure(&input, iterations, |s| bbnf_bench::nonjson_css_l4::track1_facts(s))?;
            ("track1", mbps, elapsed, user, sys, out_len)
        },
        {
            let (mbps, elapsed, user, sys, out_len) = measure(&input, iterations, |s| bbnf_bench::nonjson_css_l4::oracle_facts(s).map_err(|e| e.to_string()))?;
            ("cssparser", mbps, elapsed, user, sys, out_len)
        },
        {
            let (mbps, elapsed, user, sys, out_len) = measure(&input, iterations, |s| bbnf_bench::nonjson_css_l4::lightningcss_facts(s).map_err(|e| e.to_string()))?;
            ("lightningcss", mbps, elapsed, user, sys, out_len)
        },
    ] {
        println!("measurement\t{}\tmbps\t{:.6}\telapsed_ns\t{}\tproc_user_ns\t{}\tproc_system_ns\t{}\tout_xor\t{}", mode, mbps, elapsed, user, sys, out_len);
    }
    Ok(())
}
