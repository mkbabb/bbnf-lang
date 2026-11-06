use std::collections::BTreeMap;

use xtask::architecture::evidence::binding::{
    apple_binary_frame_for_test, apple_canonical_os_digest_for_test,
    apple_direct_dependencies_for_test, apple_physical_argv_for_test,
    apple_selection_inputs_for_test, apple_selector_record_for_test,
    live_capture_input_map_for_test,
};
use xtask::architecture::schema::Sha256Digest;

fn append_frame(bytes: &mut Vec<u8>, tag: &[u8], value: &[u8]) {
    bytes.extend_from_slice(&(tag.len() as u64).to_be_bytes());
    bytes.extend_from_slice(tag);
    bytes.extend_from_slice(&(value.len() as u64).to_be_bytes());
    bytes.extend_from_slice(value);
}

#[test]
fn transcript_framing_is_binary_exact_and_field_domain_separated() {
    let domain = b"literal-domain";
    let fields = [(b"x".as_slice(), b"a\0b".as_slice()), (b"y", b"0")];
    let mut expected = domain.to_vec();
    for (tag, value) in fields {
        append_frame(&mut expected, tag, value);
    }
    assert_eq!(apple_binary_frame_for_test(domain, &fields), expected);
    let expected_digest = |label: &str, index: u64, value: &[u8]| {
        let mut bytes = b"bbnf.apple-arm.canonical-text-field.v2".to_vec();
        append_frame(&mut bytes, b"field-label", label.as_bytes());
        append_frame(&mut bytes, b"field-index", &index.to_be_bytes());
        append_frame(&mut bytes, b"field-value", value);
        Sha256Digest::of(&bytes)
    };
    let value = b"/same/raw/path";
    let digest = apple_canonical_os_digest_for_test("launcher", 0, value);
    assert_eq!(digest, expected_digest("launcher", 0, value));
    for (label, index) in [("selected", 0), ("launcher", 1)] {
        assert_ne!(
            digest,
            apple_canonical_os_digest_for_test(label, index, value)
        );
    }
    let expected = b"-arm64\0/Developer/clang\0--version\0raw\xff"
        .split(|byte| *byte == 0)
        .map(<[u8]>::to_vec)
        .collect::<Vec<_>>();
    let actual = apple_physical_argv_for_test(b"/Developer/clang", &[b"--version", b"raw\xff"]);
    assert_eq!(actual, expected);
}

#[test]
fn selector_snapshot_rejects_empty_multiline_nul_and_split_selection() {
    assert_eq!(
        apple_selector_record_for_test(b"/Developer/\xff\n").expect("arbitrary path bytes"),
        b"/Developer/\xff"
    );
    for invalid in [
        b"".as_slice(),
        b"\n",
        b"/Developer/A\n/Developer/B\n",
        b"/Developer/A\0\n",
    ] {
        assert!(apple_selector_record_for_test(invalid).is_err());
    }
    let selected = b"/Developer/Canonical";
    let coherent = vec![selected.to_vec(); 3];
    apple_selection_inputs_for_test(selected, &coherent).expect("coherent selection");
    let mut split = coherent;
    split[1] = b"/Developer/Other".to_vec();
    assert!(apple_selection_inputs_for_test(selected, &split).is_err());
}

#[test]
fn direct_dependency_listing_preserves_order_and_paths_with_spaces() {
    let root = b"/Applications/Xcode Tool/usr/bin/clang";
    let listing = b"/Applications/Xcode Tool/usr/bin/clang:\n\
\t/usr/lib/libSystem B.dylib (compatibility version 65535.255.255, current version 1351.0.0)\n\
\t@rpath/My Framework.framework/My Framework (compatibility version 0.0.0, current version 2.4.3, weak)\n";
    assert_eq!(
        apple_direct_dependencies_for_test(root, listing).expect("complete ARM64 listing"),
        vec![
            b"/usr/lib/libSystem B.dylib".to_vec(),
            b"@rpath/My Framework.framework/My Framework".to_vec(),
        ]
    );
}

#[test]
fn direct_dependency_listing_rejects_empty_partial_wrong_slice_and_extra_rows() {
    let root = b"/tool";
    let valid_row = b"\t/usr/lib/libA.dylib (compatibility version 1.0.0, current version 1.2.3)\n";
    assert!(apple_direct_dependencies_for_test(root, b"").is_err());
    let partial = b"/tool:\n\t/usr/lib/libA.dylib";
    assert!(apple_direct_dependencies_for_test(root, partial).is_err());
    let wrong_slice = [b"/tool (unexpected slice):\n".as_slice(), valid_row].concat();
    assert!(apple_direct_dependencies_for_test(root, &wrong_slice).is_err());
    let extra = [b"/tool:\n".as_slice(), valid_row, b"extra\n"].concat();
    assert!(apple_direct_dependencies_for_test(root, &extra).is_err());
    let multi = [b"/tool:\n".as_slice(), valid_row, b"/tool:\n", valid_row].concat();
    assert!(apple_direct_dependencies_for_test(root, &multi).is_err());
    let invalid_versions = "01.0.0\n0.01.0\n0.0.01\n.0.0\n0..0\n0.0.\n65536.0.0\n0.256.0\n0.0.256\n18446744073709551616.0.0\n1.2.3.4";
    for invalid in invalid_versions.lines() {
        let listing = format!(
            "/tool:\n\t/usr/lib/libA.dylib (compatibility version {invalid}, current version 1.2.3)\n"
        );
        assert!(apple_direct_dependencies_for_test(root, listing.as_bytes()).is_err());
    }
}
#[test]
fn live_capture_input_map_retains_proxy_and_rejects_payload_substitution() {
    let retained = BTreeMap::from([
        ("ar".into(), "/Developer/ar".into()),
        ("cargo".into(), "/Users/test/.cargo/bin/cargo".into()),
        ("cc".into(), "/Developer/clang".into()),
        ("git".into(), "/usr/bin/git".into()),
        ("ld".into(), "/Developer/ld".into()),
        ("node".into(), "/usr/local/bin/node".into()),
        ("rustc".into(), "/Users/test/.cargo/bin/rustc".into()),
        ("sccache".into(), "/usr/local/bin/sccache".into()),
        ("xtask-exporter".into(), "/workspace/target/xtask".into()),
    ]);
    let mut candidate = retained.clone();
    candidate.remove("xtask-exporter");
    let accepted = live_capture_input_map_for_test(&retained, &candidate).expect("retained map");
    assert_eq!(accepted, candidate);
    let mut substituted = candidate;
    substituted.insert("cargo".to_owned(), "/x".into());
    assert!(live_capture_input_map_for_test(&retained, &substituted).is_err());
}
