use std::sync::atomic::{AtomicUsize, Ordering};

use serde::ser::{SerializeSeq, Serializer};

use subject::{
    JsonMapKeyTestCase, JsonOperation, JsonTestView, Sha256, budgeted_json_view_for_test,
    exact_os_identity_bytes_for_test, serialized_len_capped, sha256, with_json_observer_for_test,
};
use xtask::architecture::test_support as subject;

struct CountedSequence<'a> {
    visited: &'a AtomicUsize,
    length: usize,
}

static SOURCE_SCANS: AtomicUsize = AtomicUsize::new(0);
static PREFLIGHT_BYTE_TOUCHES: AtomicUsize = AtomicUsize::new(0);
static FRAGMENT_WRITES: AtomicUsize = AtomicUsize::new(0);

fn observe_json(operation: JsonOperation) {
    match operation {
        JsonOperation::SourceBytesScanned(bytes) => {
            SOURCE_SCANS.fetch_add(bytes, Ordering::Relaxed);
        }
        JsonOperation::PreflightBytesTouched(bytes) => {
            PREFLIGHT_BYTE_TOUCHES.fetch_add(bytes, Ordering::Relaxed);
        }
        JsonOperation::FragmentWritten(bytes) => {
            FRAGMENT_WRITES.fetch_add(bytes, Ordering::Relaxed);
        }
    }
}

fn reset_json_counters() {
    SOURCE_SCANS.store(0, Ordering::Relaxed);
    PREFLIGHT_BYTE_TOUCHES.store(0, Ordering::Relaxed);
    FRAGMENT_WRITES.store(0, Ordering::Relaxed);
}

#[test]
fn byte_preflight_admits_the_complete_charge_before_touching_payload() {
    let value = [0, 9, 10, 99, 100, 255].repeat(16 * 1024);
    reset_json_counters();
    let canonical = serde_json::to_vec(&value).expect("reference byte-array JSON");
    let (length, digest, work, upper) = with_json_observer_for_test(observe_json, || {
        budgeted_json_view_for_test(JsonTestView::Bytes(&value), usize::MAX)
    })
    .expect("exact budgeted byte JSON");
    assert_eq!(
        (length, digest, upper),
        (canonical.len(), sha256(&canonical), canonical.len())
    );
    assert_eq!(PREFLIGHT_BYTE_TOUCHES.load(Ordering::Relaxed), value.len());
    assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), value.len() * 2);
    assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), canonical.len());

    reset_json_counters();
    let (baseline, _) = crate::take_heap_usage();
    let error = with_json_observer_for_test(observe_json, || {
        budgeted_json_view_for_test(JsonTestView::Bytes(&value), work - 1)
    })
    .expect_err("one below exact byte admission");
    let (active, peak) = crate::take_heap_usage();
    assert_eq!((active - baseline, peak - baseline), (0, 0));
    assert_eq!(PREFLIGHT_BYTE_TOUCHES.load(Ordering::Relaxed), 0);
    assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), 0);
    assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), 0);
    let _ = error;
}

fn assert_supported_map_key(
    case: JsonMapKeyTestCase,
    canonical: &[u8],
    expected_work: Option<usize>,
) {
    reset_json_counters();
    let (length, digest, work, upper) = with_json_observer_for_test(observe_json, || {
        budgeted_json_view_for_test(JsonTestView::MapKey(case), usize::MAX)
    })
    .expect("supported pinned map key");
    assert_eq!(
        (length, digest, upper),
        (canonical.len(), sha256(canonical), canonical.len())
    );
    if let Some(expected_work) = expected_work {
        assert_eq!(work, expected_work);
        reset_json_counters();
        let error = with_json_observer_for_test(observe_json, || {
            budgeted_json_view_for_test(JsonTestView::MapKey(case), expected_work - 1)
        })
        .expect_err("one below exact supported map key");
        assert_eq!(PREFLIGHT_BYTE_TOUCHES.load(Ordering::Relaxed), 0);
        assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), 0);
        assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), 0);
        let _ = error;
    }
}

#[test]
fn finite_float_map_keys_have_exact_compact_width_and_digest() {
    // Container and value consume 64 and 40 work. A four-byte finite key
    // consumes one visit plus twice its compact width including two quotes.
    assert_supported_map_key(
        JsonMapKeyTestCase::F32(1.25),
        br#"{"1.25":true}"#,
        Some(148),
    );
    assert_supported_map_key(
        JsonMapKeyTestCase::F64(-2.5),
        br#"{"-2.5":true}"#,
        Some(148),
    );
}

#[test]
fn escaped_char_unit_variant_and_recursive_supported_keys_match_json() {
    assert_supported_map_key(JsonMapKeyTestCase::Char('\n'), br#"{"\n":true}"#, None);
    assert_supported_map_key(JsonMapKeyTestCase::UnitVariant, br#"{"ready":true}"#, None);
    assert_supported_map_key(JsonMapKeyTestCase::NewtypeStruct, br#"{"7":true}"#, None);
    assert_supported_map_key(JsonMapKeyTestCase::Some, br#"{"7":true}"#, None);
}

fn assert_rejected_map_key(case: JsonMapKeyTestCase) {
    reset_json_counters();
    let (baseline, _) = crate::take_heap_usage();
    let error = with_json_observer_for_test(observe_json, || {
        budgeted_json_view_for_test(JsonTestView::MapKey(case), usize::MAX)
    })
    .expect_err("unsupported pinned map key");
    let (active, peak) = crate::take_heap_usage();
    assert_eq!((active - baseline, peak - baseline), (0, 0));
    assert_eq!(PREFLIGHT_BYTE_TOUCHES.load(Ordering::Relaxed), 0);
    assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), 0);
    assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), 0);
    let _ = error;
}

#[test]
fn nonfinite_map_keys_reject_before_output_or_heap_work() {
    for case in [
        JsonMapKeyTestCase::F32(f32::NAN),
        JsonMapKeyTestCase::F32(f32::INFINITY),
        JsonMapKeyTestCase::F64(f64::NEG_INFINITY),
    ] {
        assert_rejected_map_key(case);
    }
}

#[test]
fn unsupported_map_key_shapes_reject_before_output_or_heap_work() {
    for case in [
        JsonMapKeyTestCase::Unit,
        JsonMapKeyTestCase::UnitStruct,
        JsonMapKeyTestCase::Bytes,
        JsonMapKeyTestCase::None,
        JsonMapKeyTestCase::NewtypeVariant,
        JsonMapKeyTestCase::Seq,
        JsonMapKeyTestCase::Tuple,
        JsonMapKeyTestCase::TupleStruct,
        JsonMapKeyTestCase::TupleVariant,
        JsonMapKeyTestCase::Map,
        JsonMapKeyTestCase::Struct,
        JsonMapKeyTestCase::StructVariant,
    ] {
        assert_rejected_map_key(case);
    }
}

#[test]
fn map_key_collect_str_rejects_before_display_formatting() {
    let calls = AtomicUsize::new(0);
    let allocated = AtomicUsize::new(0);
    let display = MultiFragmentDisplay {
        calls: &calls,
        allocated: &allocated,
    };
    reset_json_counters();
    budgeted_json_view_for_test(JsonTestView::MapCollectStrKey(&display), usize::MAX)
        .expect_err("map-key collect_str is rejected before formatting");
    assert_eq!(calls.load(Ordering::Relaxed), 0);
    assert_eq!(allocated.load(Ordering::Relaxed), 0);
    assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), 0);
}

struct MultiFragmentDisplay<'a> {
    calls: &'a AtomicUsize,
    allocated: &'a AtomicUsize,
}

impl std::fmt::Display for MultiFragmentDisplay<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.calls.fetch_add(1, Ordering::Relaxed);
        let allocation = vec![b'x'; 4096];
        self.allocated
            .fetch_add(allocation.capacity(), Ordering::Relaxed);
        formatter.write_str("alpha")?;
        formatter.write_str(std::str::from_utf8(&allocation).expect("ASCII allocation"))?;
        formatter.write_str("omega")
    }
}

#[test]
fn serde_preflight_admits_raw_strings_before_scanning_or_streaming() {
    for value in [
        "a".repeat(64 * 1024),
        format!("{}\\\"", "a".repeat(64 * 1024)),
        "\\\"\\n\\r\\t".repeat(16 * 1024),
    ] {
        reset_json_counters();
        let canonical = serde_json::to_vec(&value).expect("reference JSON");
        let (length, digest, work, upper) = with_json_observer_for_test(observe_json, || {
            budgeted_json_view_for_test(JsonTestView::String(&value), usize::MAX)
        })
        .expect("exact budgeted JSON");
        assert_eq!(length, canonical.len());
        assert_eq!(upper, length);
        assert_eq!(digest, sha256(&canonical));
        assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), value.len() * 2);
        assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), canonical.len());

        reset_json_counters();
        let (baseline, _) = crate::take_heap_usage();
        let error = with_json_observer_for_test(observe_json, || {
            budgeted_json_view_for_test(JsonTestView::String(&value), work - 1)
        })
        .expect_err("one below exact admission");
        let (active, peak) = crate::take_heap_usage();
        assert_eq!(
            (active - baseline, peak - baseline),
            (0, 0),
            "one-below rejection, including its returned marker, allocated"
        );
        assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), 0);
        assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), 0);
        let _ = error;
    }
}

#[test]
fn serde_preflight_rejects_checked_arithmetic_overflow_before_output() {
    reset_json_counters();
    let error = with_json_observer_for_test(observe_json, || {
        budgeted_json_view_for_test(JsonTestView::OverflowSequence, usize::MAX)
    })
    .expect_err("declared container length overflows its checked admission");
    assert!(error.to_string().contains("RESOURCE_BUDGET_EXCEEDED"));
    assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), 0);
    assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), 0);
}

#[test]
fn every_supported_sealed_json_view_has_exact_serde_json_parity() {
    fn assert_view(view: JsonTestView<'_>, canonical: Vec<u8>) {
        let (length, digest, _, upper) =
            budgeted_json_view_for_test(view, usize::MAX).expect("sealed JSON view");
        assert_eq!(length, canonical.len());
        assert_eq!(digest, sha256(&canonical));
        assert_eq!(upper, canonical.len());
    }

    let string = "quoted\"\\\n\0";
    assert_view(
        JsonTestView::String(string),
        serde_json::to_vec(string).expect("reference string"),
    );
    assert_view(
        JsonTestView::I128(i128::MIN),
        serde_json::to_vec(&i128::MIN).expect("reference scalar"),
    );
    let sequence = [i128::MIN, 0, i128::MAX];
    assert_view(
        JsonTestView::I128Slice(&sequence),
        serde_json::to_vec(&sequence).expect("reference sequence"),
    );
    assert_view(
        JsonTestView::UnknownI128Slice(&sequence),
        serde_json::to_vec(&sequence).expect("reference unknown sequence"),
    );
    let numeric = std::collections::BTreeMap::from([(u128::MAX, false)]);
    assert_view(
        JsonTestView::U128BoolMap(&numeric),
        serde_json::to_vec(&numeric).expect("reference numeric-key map"),
    );
    let boolean = std::collections::BTreeMap::from([(true, i128::MIN)]);
    assert_view(
        JsonTestView::BoolI128Map(&boolean),
        serde_json::to_vec(&boolean).expect("reference bool-key map"),
    );
}

#[test]
fn sequence_and_map_separators_match_serde_json_after_multiple_entries() {
    let sequence = [1, 2, 3, 4];
    let (length, digest, _, upper) =
        budgeted_json_view_for_test(JsonTestView::I128Slice(&sequence), usize::MAX)
            .expect("multi-entry sequence");
    let canonical = serde_json::to_vec(&sequence).expect("reference sequence");
    assert_eq!(
        (length, digest, upper),
        (canonical.len(), sha256(&canonical), canonical.len())
    );

    let map = std::collections::BTreeMap::from([(1u128, true), (2, false), (3, true)]);
    let (length, digest, _, upper) =
        budgeted_json_view_for_test(JsonTestView::U128BoolMap(&map), usize::MAX)
            .expect("multi-entry map");
    let canonical = serde_json::to_vec(&map).expect("reference map");
    assert_eq!(
        (length, digest, upper),
        (canonical.len(), sha256(&canonical), canonical.len())
    );
}

#[test]
fn direct_numeric_and_bool_map_keys_have_exact_serde_json_parity() {
    fn assert_map(view: JsonTestView<'_>, canonical: Vec<u8>, expected_work: usize) {
        reset_json_counters();
        let (length, digest, work, upper) = with_json_observer_for_test(observe_json, || {
            budgeted_json_view_for_test(view, expected_work)
        })
        .expect("sealed map operation");
        assert_eq!((length, digest), (canonical.len(), sha256(&canonical)));
        assert_eq!(work, expected_work);
        assert_eq!(upper, canonical.len());
        assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), 0);
        assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), canonical.len());
    }

    // Container: 2 * 32. Each key/value scalar: one 32-unit callback plus
    // two exact output/hash widths. Numeric/bool keys include JSON quotes.
    let expected = 2 * 32 + (32 + 2 * 41) + (32 + 2 * 5);
    let numeric = std::collections::BTreeMap::from([(u128::MAX, false)]);
    assert_map(
        JsonTestView::U128BoolMap(&numeric),
        serde_json::to_vec(&numeric).expect("numeric-key JSON"),
        expected,
    );
    reset_json_counters();
    budgeted_json_view_for_test(JsonTestView::U128BoolMap(&numeric), expected - 1)
        .expect_err("one below numeric-key map");
    assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), 0);
    assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), 0);

    let expected = 2 * 32 + (32 + 2 * 6) + (32 + 2 * 40);
    let boolean = std::collections::BTreeMap::from([(true, i128::MIN)]);
    assert_map(
        JsonTestView::BoolI128Map(&boolean),
        serde_json::to_vec(&boolean).expect("bool-key JSON"),
        expected,
    );
    reset_json_counters();
    budgeted_json_view_for_test(JsonTestView::BoolI128Map(&boolean), expected - 1)
        .expect_err("one below bool-key map");
    assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), 0);
    assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), 0);
}

#[test]
fn arbitrary_collect_str_is_rejected_before_display_or_heap_work() {
    let calls = AtomicUsize::new(0);
    let allocated = AtomicUsize::new(0);
    let display = MultiFragmentDisplay {
        calls: &calls,
        allocated: &allocated,
    };
    reset_json_counters();
    let (baseline, _) = crate::take_heap_usage();
    let error = with_json_observer_for_test(observe_json, || {
        budgeted_json_view_for_test(JsonTestView::CollectStr(&display), usize::MAX)
    })
    .expect_err("sealed collect_str is rejected before formatting");
    let (active, peak) = crate::take_heap_usage();
    assert_eq!(calls.load(Ordering::Relaxed), 0);
    assert_eq!(allocated.load(Ordering::Relaxed), 0);
    assert_eq!(
        (active - baseline, peak - baseline),
        (0, 0),
        "typed preflight error allocated before rejection"
    );
    let _ = error;
    assert_eq!(PREFLIGHT_BYTE_TOUCHES.load(Ordering::Relaxed), 0);
    assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), 0);
    assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), 0);
}

#[test]
fn scalar_output_and_hash_widths_are_part_of_the_exact_operation_charge() {
    fn assert_scalar_shape(view: JsonTestView<'_>, canonical: Vec<u8>, expected_work: usize) {
        let (length, digest, work, upper) =
            budgeted_json_view_for_test(view, expected_work).expect("exact scalar operation");
        assert_eq!((length, digest), (canonical.len(), sha256(&canonical)));
        assert_eq!(work, expected_work);
        assert_eq!(upper, canonical.len());
    }

    assert_scalar_shape(
        JsonTestView::I128(i128::MIN),
        serde_json::to_vec(&i128::MIN).expect("scalar JSON"),
        32 + 2 * 40,
    );
    reset_json_counters();
    budgeted_json_view_for_test(JsonTestView::I128(i128::MIN), 32 + 2 * 40 - 1)
        .expect_err("one below scalar operation");
    assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), 0);
    assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), 0);

    let declared = vec![i128::MIN; 64];
    let declared_work = 65 * 32 + 64 * (32 + 2 * 40);
    assert_scalar_shape(
        JsonTestView::I128Slice(&declared),
        serde_json::to_vec(&declared).expect("declared sequence JSON"),
        declared_work,
    );
    reset_json_counters();
    budgeted_json_view_for_test(JsonTestView::I128Slice(&declared), declared_work - 1)
        .expect_err("one below declared sequence");
    assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), 0);
    assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), 0);

    let unknown_work = 32 + 64 * (32 + 2 * 40);
    assert_scalar_shape(
        JsonTestView::UnknownI128Slice(&declared),
        serde_json::to_vec(&declared).expect("unknown sequence JSON"),
        unknown_work,
    );
    reset_json_counters();
    budgeted_json_view_for_test(JsonTestView::UnknownI128Slice(&declared), unknown_work - 1)
        .expect_err("one below unknown sequence");
    assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), 0);
    assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), 0);
}

#[test]
fn admitted_json_execution_has_no_replayable_or_lower_kind_token_surface() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src/architecture/hash.rs"),
    )
    .expect("hash source");
    for forbidden in [
        "pub(crate) struct JsonAdmission",
        "pub(crate) fn admit_json<",
        "pub(crate) fn admit_json_hash<",
        "pub(crate) fn measure(&self)",
        "pub(crate) fn stream_hash(&self",
        "pub(crate) struct JsonPlan",
        "pub(crate) fn prepare_json_measure<",
        "pub(crate) fn prepare_json_hash<",
        "pub(crate) fn hash_serialized_budgeted<",
    ] {
        assert!(
            !source.contains(forbidden),
            "credited JSON execution remains replayable: {forbidden}"
        );
    }
}

#[test]
fn every_json_admission_total_uses_checked_arithmetic_before_output() {
    fn assert_overflow(view: JsonTestView<'_>) {
        reset_json_counters();
        let error = with_json_observer_for_test(observe_json, || {
            budgeted_json_view_for_test(view, usize::MAX)
        })
        .expect_err("declared JSON length overflows checked admission");
        assert!(error.to_string().contains("RESOURCE_BUDGET_EXCEEDED"));
        assert_eq!(SOURCE_SCANS.load(Ordering::Relaxed), 0);
        assert_eq!(FRAGMENT_WRITES.load(Ordering::Relaxed), 0);
    }
    assert_overflow(JsonTestView::OverflowSequence);
    assert_overflow(JsonTestView::OverflowMap);
}

#[test]
fn serde_preflight_pass_ledger_covers_worst_case_json_expansion() {
    let (_, _, empty_work, _) =
        budgeted_json_view_for_test(JsonTestView::String(""), usize::MAX).expect("empty string");
    let controls = "\0".repeat(4096);
    let (_, _, control_work, _) =
        budgeted_json_view_for_test(JsonTestView::String(&controls), usize::MAX)
            .expect("control string");
    assert_eq!(
        control_work - empty_work,
        controls.len() * (3 + 2 * 6),
        "one preflight plus two writer scans and two six-byte output/hash passes"
    );
}

impl serde::Serialize for CountedSequence<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut sequence = serializer.serialize_seq(Some(self.length))?;
        for index in 0..self.length {
            self.visited.fetch_add(1, Ordering::Relaxed);
            sequence.serialize_element(&index)?;
        }
        sequence.end()
    }
}

#[test]
fn capped_serialized_measurement_stops_at_the_ceiling() {
    let visited = AtomicUsize::new(0);
    let error = serialized_len_capped(
        &CountedSequence {
            visited: &visited,
            length: 10_000,
        },
        32,
    )
    .expect_err("serialized form exceeds ceiling");
    assert!(error.to_string().contains("RESOURCE_BUDGET_EXCEEDED"));
    assert!(visited.load(Ordering::Relaxed) < 10_000);
    let canonical = subject::canonical_json(&[1, 2, 3]).expect("canonical JSON");
    assert_eq!(canonical, b"[1,2,3]");
    assert_eq!(
        subject::hash_serialized(&[1, 2, 3]).expect("serialized hash"),
        sha256(&canonical)
    );
}

#[cfg(unix)]
#[test]
fn exact_os_identity_v1_has_a_fixed_invalid_utf8_frame_and_sha() {
    use std::os::unix::ffi::OsStringExt;

    let value = std::ffi::OsString::from_vec(vec![b'r', b'a', b'w', 0xff]);
    let frame = exact_os_identity_bytes_for_test(b"d", &value);
    let mut expected = b"bbnf.exact-os-identity.v1\0".to_vec();
    expected.extend_from_slice(&1u64.to_le_bytes());
    expected.push(b'd');
    expected.extend_from_slice(&4u64.to_le_bytes());
    expected.extend_from_slice(b"raw\xff");
    assert_eq!(frame, expected, "the v1 framing contract changed");
    assert_eq!(
        sha256(&frame),
        "b1fb5a636800d55a1b2f8b60cf83934f89e5a8f78e2e7837621fcf4617cd7d3a"
    );
}

fn assert_vector(bytes: &[u8], expected: &str) {
    assert_eq!(sha256(bytes), expected);
    for size in [1, 7, 55, 56, 63, 64, 65, 127, 128, 129, 4096] {
        let mut hash = Sha256::new();
        bytes.chunks(size).for_each(|chunk| hash.update(chunk));
        assert_eq!(hash.finalize(), expected, "chunk size {size}");
    }
}

#[test]
fn sha256_known_vectors_and_chunking() {
    assert_vector(
        b"",
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
    );
    assert_vector(
        b"abc",
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
    );
    assert_vector(
        b"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
        "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1",
    );
    assert_vector(
        &vec![b'a'; 1_000_000],
        "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0",
    );

    for (length, expected) in [
        (
            55,
            "463eb28e72f82e0a96c0a4cc53690c571281131f672aa229e0d45ae59b598b59",
        ),
        (
            56,
            "da2ae4d6b36748f2a318f23e7ab1dfdf45acdc9d049bd80e59de82a60895f562",
        ),
        (
            63,
            "29af2686fd53374a36b0846694cc342177e428d1647515f078784d69cdb9e488",
        ),
        (
            64,
            "fdeab9acf3710362bd2658cdc9a29e8f9c757fcf9811603a8c447cd1d9151108",
        ),
        (
            65,
            "4bfd2c8b6f1eec7a2afeb48b934ee4b2694182027e6d0fc075074f2fabb31781",
        ),
        (
            119,
            "da18797ed7c3a777f0847f429724a2d8cd5138e6ed2895c3fa1a6d39d18f7ec6",
        ),
        (
            120,
            "f52b23db1fbb6ded89ef42a23ce0c8922c45f25c50b568a93bf1c075420bbb7c",
        ),
        (
            127,
            "92ca0fa6651ee2f97b884b7246a562fa71250fedefe5ebf270d31c546bfea976",
        ),
        (
            128,
            "471fb943aa23c511f6f72f8d1652d9c880cfa392ad80503120547703e56a2be5",
        ),
        (
            129,
            "5099c6a56203f9687f7d33f4bfdf576d31dc91f6b695ecea38b2770c87631135",
        ),
    ] {
        let bytes = (0..length).map(|byte| byte as u8).collect::<Vec<_>>();
        assert_vector(&bytes, expected);
    }
}
