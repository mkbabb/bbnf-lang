#![allow(dead_code)]

pub struct Xorshift64(u64);

impl Xorshift64 {
    pub fn new(seed: u64) -> Self {
        Self(seed | 1)
    }

    pub fn next_u64(&mut self) -> u64 {
        let mut state = self.0;
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        self.0 = state;
        state
    }

    pub fn fill(&mut self, buffer: &mut [u8]) {
        let mut cursor = 0;
        while cursor + 8 <= buffer.len() {
            buffer[cursor..cursor + 8].copy_from_slice(&self.next_u64().to_le_bytes());
            cursor += 8;
        }
        if cursor < buffer.len() {
            let bytes = self.next_u64().to_le_bytes();
            let remaining = buffer.len() - cursor;
            buffer[cursor..].copy_from_slice(&bytes[..remaining]);
        }
    }
}

#[inline(never)]
pub fn guarded_call<F, R>(f: F) -> R
where
    F: FnOnce() -> R,
{
    with_stack_canary_xor_fold("guarded_call", f)
}

#[inline(never)]
pub fn stack_canary_then<F, R>(f: F) -> R
where
    F: FnOnce() -> R,
{
    with_stack_canary_xor_fold("stack_canary_then", f)
}

#[inline(never)]
pub fn with_stack_canary_xor_fold<F, R>(label: &'static str, f: F) -> R
where
    F: FnOnce() -> R,
{
    let mut canary = [0u8; 1024];
    Xorshift64::new(0xC0DE_CAFE_C0DE_CAFE).fill(&mut canary);
    let before = canary;
    let pre_fold = xor_fold(&canary);
    std::hint::black_box(&before);
    let result = f();
    std::hint::black_box(&canary);
    let post_fold = xor_fold(&canary);
    if pre_fold != post_fold || canary != before {
        let first_bad_byte = canary
            .iter()
            .zip(before.iter())
            .position(|(after, before)| after != before);
        panic!(
            "checkasm stack canary was clobbered in {label}: pre_fold={pre_fold:#018x} post_fold={post_fold:#018x} first_bad_byte={first_bad_byte:?}"
        );
    }
    result
}

fn xor_fold(bytes: &[u8; 1024]) -> u64 {
    let mut fold = 0u64;
    for chunk in bytes.chunks_exact(8) {
        let lane = u64::from_le_bytes(chunk.try_into().expect("canary chunks are 8 bytes"));
        fold ^= lane;
    }
    fold
}

#[cfg(target_arch = "aarch64")]
#[inline(never)]
pub fn callee_saved_register_then<F, R>(f: F) -> R
where
    F: FnOnce() -> R,
{
    const SENTINELS: [u64; 10] = [
        0x1919_1919_1919_1919,
        0x2020_2020_2020_2020,
        0x2121_2121_2121_2121,
        0x2222_2222_2222_2222,
        0x2323_2323_2323_2323,
        0x2424_2424_2424_2424,
        0x2525_2525_2525_2525,
        0x2626_2626_2626_2626,
        0x2727_2727_2727_2727,
        0x2828_2828_2828_2828,
    ];

    let saved = read_aarch64_callee_saved();
    write_aarch64_callee_saved(SENTINELS);
    let result = f();
    let observed = read_aarch64_callee_saved();
    write_aarch64_callee_saved(saved);
    assert_eq!(
        observed, SENTINELS,
        "checkasm candidate clobbered an AArch64 callee-saved register"
    );
    result
}

#[cfg(not(target_arch = "aarch64"))]
#[inline(never)]
pub fn callee_saved_register_then<F, R>(f: F) -> R
where
    F: FnOnce() -> R,
{
    f()
}

#[cfg(target_arch = "aarch64")]
#[inline(always)]
fn read_aarch64_callee_saved() -> [u64; 10] {
    let x19: u64;
    let x20: u64;
    let x21: u64;
    let x22: u64;
    let x23: u64;
    let x24: u64;
    let x25: u64;
    let x26: u64;
    let x27: u64;
    let x28: u64;
    unsafe {
        core::arch::asm!(
            "mov {x19}, x19",
            "mov {x20}, x20",
            "mov {x21}, x21",
            "mov {x22}, x22",
            "mov {x23}, x23",
            "mov {x24}, x24",
            "mov {x25}, x25",
            "mov {x26}, x26",
            "mov {x27}, x27",
            "mov {x28}, x28",
            x19 = out(reg) x19,
            x20 = out(reg) x20,
            x21 = out(reg) x21,
            x22 = out(reg) x22,
            x23 = out(reg) x23,
            x24 = out(reg) x24,
            x25 = out(reg) x25,
            x26 = out(reg) x26,
            x27 = out(reg) x27,
            x28 = out(reg) x28,
            options(nostack, preserves_flags)
        );
    }
    [x19, x20, x21, x22, x23, x24, x25, x26, x27, x28]
}

#[cfg(target_arch = "aarch64")]
#[inline(always)]
fn write_aarch64_callee_saved(values: [u64; 10]) {
    unsafe {
        core::arch::asm!(
            "mov x19, {x19}",
            "mov x20, {x20}",
            "mov x21, {x21}",
            "mov x22, {x22}",
            "mov x23, {x23}",
            "mov x24, {x24}",
            "mov x25, {x25}",
            "mov x26, {x26}",
            "mov x27, {x27}",
            "mov x28, {x28}",
            x19 = in(reg) values[0],
            x20 = in(reg) values[1],
            x21 = in(reg) values[2],
            x22 = in(reg) values[3],
            x23 = in(reg) values[4],
            x24 = in(reg) values[5],
            x25 = in(reg) values[6],
            x26 = in(reg) values[7],
            x27 = in(reg) values[8],
            x28 = in(reg) values[9],
            options(nostack, preserves_flags)
        );
    }
}
