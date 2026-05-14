mod checkasm_common;

use checkasm_common::{guarded_call, Xorshift64};

fn table_from_members(members: &[u8]) -> [u8; 256] {
    let mut table = [0u8; 256];
    for &member in members {
        table[member as usize] = 0xff;
    }
    table
}

#[test]
fn byte_class_from_table_64_density_sweep() {
    let mut rng = Xorshift64::new(0x5441_424C_455F_3634);
    let densities = [0usize, 1, 2, 4, 8, 16, 32, 64, 128, 255, 256];
    for density in densities {
        let members: Vec<u8> = (0..density).map(|byte| byte as u8).collect();
        let table = table_from_members(&members);
        for _ in 0..64 {
            let mut src = [0u8; 64];
            rng.fill(&mut src);
            assert_eq!(
                guarded_call(|| bbnf_simd::prim::byte_class_from_table_64(&src, &table)),
                bbnf_simd::scalar::byte_class_from_table_64::byte_class_from_table_64_scalar(
                    &src, &table
                )
            );
        }
    }
}

#[test]
fn byte_class_from_table_64_alignment_windows() {
    let table = table_from_members(b"{}[],:\"\\");
    let mut backing = [0u8; 128];
    for (index, byte) in backing.iter_mut().enumerate() {
        *byte = index as u8;
    }
    for align in 0..64 {
        let src: &[u8; 64] = backing[align..align + 64].try_into().unwrap();
        assert_eq!(
            guarded_call(|| bbnf_simd::prim::byte_class_from_table_64(src, &table)),
            bbnf_simd::scalar::byte_class_from_table_64::byte_class_from_table_64_scalar(
                src, &table
            )
        );
    }
}
