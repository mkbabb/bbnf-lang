//! Exhaustive native boundary and over-read differentials.

#![cfg(all(target_arch = "aarch64", target_os = "macos"))]

use simd_scan::{alphabet::StructuralAlphabet, scalar, scan_structural};

const JSON: StructuralAlphabet = StructuralAlphabet::from_parts(
    &[b'{', b'}', b'[', b']', b':', b',', b'"', b'\\'],
    &[],
    &[b'"'],
);

const CSS: StructuralAlphabet = StructuralAlphabet::from_parts(
    &[b'{', b'}', b'(', b')', b';', b':', b','],
    &[(b'/', b'*'), (b'*', b'/')],
    &[b'"', b'\''],
);

const WIDE: StructuralAlphabet = StructuralAlphabet::from_parts(
    &[b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h', b'i'],
    &[(b'/', b'*'), (b'*', b'/')],
    &[b'"', b'\''],
);

const MULTI: StructuralAlphabet = StructuralAlphabet::from_parts(
    &[
        b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm', b'n', b'o',
        b'p', b'q',
    ],
    &[(b'/', b'*'), (b'*', b'/')],
    &[b'"', b'\''],
);

fn patterned(len: usize, salt: usize) -> Vec<u8> {
    (0..len)
        .map(|index| {
            const BYTES: &[u8] = b"{}[]:,/*\\\"\'\x00\x80\xbf\xc0\xff az09";
            BYTES[(index * 17 + salt * 29) % BYTES.len()]
        })
        .collect()
}

fn assert_parity(input: &[u8], alphabet: &StructuralAlphabet) {
    let reference = scalar::scan(input, alphabet);
    let actual = scan_structural(input, alphabet);
    assert_eq!(actual.positions, reference.positions);
    assert_eq!(actual.kinds, reference.kinds);
}

#[test]
fn every_short_tail_and_alignment_matches_reference() {
    for len in 0..=129 {
        for offset in 0..64 {
            let input = patterned(len, offset);
            let mut storage = vec![0xa5; offset + len + 64];
            storage[offset..offset + len].copy_from_slice(&input);
            let subject = &storage[offset..offset + len];
            assert_parity(subject, &JSON);
            assert_parity(subject, &CSS);
            assert_parity(subject, &WIDE);
            assert_parity(subject, &MULTI);
        }
    }
}

#[test]
fn page_ending_inputs_never_read_the_guard_page() {
    unsafe {
        let page = libc::sysconf(libc::_SC_PAGESIZE);
        assert!(page > 0);
        let page = page as usize;
        let allocation = libc::mmap(
            core::ptr::null_mut(),
            page * 2,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_PRIVATE | libc::MAP_ANON,
            -1,
            0,
        );
        assert_ne!(allocation, libc::MAP_FAILED);
        let base = allocation.cast::<u8>();
        assert_eq!(
            libc::mprotect(base.add(page).cast(), page, libc::PROT_NONE),
            0
        );

        for len in 0..=129 {
            let start = base.add(page - len);
            let input = patterned(len, len);
            core::ptr::copy_nonoverlapping(input.as_ptr(), start, len);
            let subject = core::slice::from_raw_parts(start, len);
            assert_parity(subject, &JSON);
            assert_parity(subject, &CSS);
            assert_parity(subject, &WIDE);
            assert_parity(subject, &MULTI);
        }

        assert_eq!(libc::munmap(allocation, page * 2), 0);
    }
}
