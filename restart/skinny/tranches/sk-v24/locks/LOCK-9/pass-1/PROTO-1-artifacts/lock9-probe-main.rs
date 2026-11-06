// LOCK-9 probe: does a cross-crate bbnf-simd prefix_xor kernel inline under
// fat-LTO, or does the dispatch.rs OnceLock fn-pointer facade force an
// indirect branch (blr) in the caller (and inside a loop)?
use std::hint::black_box;

// Baseline: the exact inline bit-twiddle parse-that/quote_parity.rs uses today.
#[no_mangle]
#[inline(never)]
pub extern "C" fn probe_baseline(mask: u64) -> u64 {
    let mut x = mask;
    x ^= x << 1; x ^= x << 2; x ^= x << 4;
    x ^= x << 8; x ^= x << 16; x ^= x << 32;
    x
}

// Arm A: through bbnf-simd's public prefix_xor_64 -> prim::bitmap_prefix_xor_64
//        -> dispatch::primitive_kernels() OnceLock fn-pointer table.
#[no_mangle]
#[inline(never)]
pub extern "C" fn probe_arm_a(mask: u64) -> u64 {
    bbnf_simd::prefix_xor_64(mask, false)
}

// Arm B: through bbnf-simd's direct #[cfg]-selected entry, no OnceLock.
#[no_mangle]
#[inline(never)]
pub extern "C" fn probe_arm_b(mask: u64) -> u64 {
    bbnf_simd::prefix_xor_64_direct(mask, false)
}

// Loop variants: mimic compute_in_string_bitmap's per-stripe call. Does the
// call survive INSIDE the loop body?
#[no_mangle]
#[inline(never)]
pub extern "C" fn probe_loop_arm_a(masks: &[u64]) -> u64 {
    let mut acc = 0u64;
    for &m in masks {
        acc ^= bbnf_simd::prefix_xor_64(m ^ acc, false);
    }
    acc
}

#[no_mangle]
#[inline(never)]
pub extern "C" fn probe_loop_arm_b(masks: &[u64]) -> u64 {
    let mut acc = 0u64;
    for &m in masks {
        acc ^= bbnf_simd::prefix_xor_64_direct(m ^ acc, false);
    }
    acc
}

fn main() {
    let m = black_box(0x00FF_00FF_1234_5678u64);
    let v: Vec<u64> = (0..black_box(8u64)).map(|i| m.wrapping_mul(i + 1)).collect();
    // Parity check: all three must agree for carry_in=false.
    let a = probe_baseline(black_box(m));
    let b = probe_arm_a(black_box(m));
    let c = probe_arm_b(black_box(m));
    println!("baseline={a:#018x} arm_a={b:#018x} arm_b={c:#018x} parity={}", a == b && b == c);
    println!("loop_a={:#018x} loop_b={:#018x}",
        probe_loop_arm_a(black_box(&v)), probe_loop_arm_b(black_box(&v)));

    // Direct per-call micro-timing of the kernel swap (the json bench never
    // exercises prefix_xor, so this is the honest measurement surface).
    // 16384 masks ~ a 1 MiB input's stripe count; 2000 reps each.
    let masks: Vec<u64> = (0..16384u64).map(|i| m.wrapping_mul(i * 2654435761 + 1)).collect();
    let reps = 2000usize;
    let time = |f: &dyn Fn(&[u64]) -> u64| -> f64 {
        // warmup
        for _ in 0..64 { black_box(f(black_box(&masks))); }
        let mut best = f64::MAX;
        for _ in 0..9 {
            let t0 = std::time::Instant::now();
            let mut acc = 0u64;
            for _ in 0..reps { acc ^= f(black_box(&masks)); }
            black_box(acc);
            let ns = t0.elapsed().as_nanos() as f64 / (reps as f64 * masks.len() as f64);
            if ns < best { best = ns; }
        }
        best
    };
    let a = time(&|s| probe_loop_arm_a(s));
    let b = time(&|s| probe_loop_arm_b(s));
    println!("PERCALL_NS arm_a(onceLock)={a:.4} arm_b(direct)={b:.4} ratio_b_over_a={:.3}", b / a);
    println!("SPEEDUP arm_b vs arm_a = {:.2}x", a / b);
}
