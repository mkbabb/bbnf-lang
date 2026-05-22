mod checkasm_common;

use checkasm_common::{guarded_call, Xorshift64};
use std::hint::black_box;
use std::time::Instant;
use test_fixtures::sha256_hex;

const DELIMS: &[u8] = b"{};";
const FIXTURE: &[u8] = include_bytes!(
    "../../../../restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css"
);

#[derive(Clone)]
struct Case {
    bytes: Vec<u8>,
    cursor: usize,
    end: usize,
}

fn find_scalar(bytes: &[u8], mut cursor: usize, end: usize, set: &[u8]) -> usize {
    assert!(cursor <= end && end <= bytes.len());
    assert!(set.len() <= 8);
    while cursor < end && !set.contains(&bytes[cursor]) {
        cursor += 1;
    }
    cursor
}

fn find_candidate(bytes: &[u8], cursor: usize, end: usize, set: &[u8]) -> usize {
    bbnf_simd::find_ascii_set_member64(bytes, cursor, end, set)
}

fn cases() -> Vec<Case> {
    let mut out = Vec::new();
    let css = FIXTURE.repeat(24);
    for cursor in (0..css.len()).step_by(5) {
        out.push(Case {
            bytes: css.clone(),
            cursor,
            end: css.len(),
        });
    }
    for lane in 0..64 {
        let mut bytes = vec![b'a'; 128];
        bytes[lane] = b';';
        out.push(Case {
            bytes,
            cursor: 0,
            end: 64,
        });
    }
    for tail in 0..64 {
        let mut bytes = vec![0x80; 96];
        if tail > 0 {
            bytes[tail - 1] = b'}';
        }
        out.push(Case {
            bytes,
            cursor: 0,
            end: tail,
        });
    }
    for seed in [
        0xCAFE_F00D_BAAD_F00D,
        0x5441_424C_455F_3634,
        0xDEAD_BEEF_1234_5678,
    ] {
        let mut rng = Xorshift64::new(seed);
        for _ in 0..64 {
            let mut bytes = vec![0u8; 192];
            rng.fill(&mut bytes);
            out.push(Case {
                bytes,
                cursor: (rng.next_u64() as usize) % 65,
                end: 192,
            });
        }
    }
    out
}

fn cases_sha256(cases: &[Case]) -> String {
    let mut bytes = Vec::new();
    for case in cases {
        bytes.extend_from_slice(&(case.cursor as u64).to_le_bytes());
        bytes.extend_from_slice(&(case.end as u64).to_le_bytes());
        bytes.extend_from_slice(&(case.bytes.len() as u64).to_le_bytes());
        bytes.extend_from_slice(&case.bytes);
    }
    sha256_hex(&bytes)
}

#[test]
fn ascii_set_member_find_matches_scalar() {
    let duplicate_set = b"{{;;}}";
    for set in [DELIMS, duplicate_set.as_slice()] {
        for case in cases() {
            let before = case.bytes.clone();
            let expected = find_scalar(&case.bytes, case.cursor, case.end, set);
            let observed = guarded_call(|| find_candidate(&case.bytes, case.cursor, case.end, set));
            assert_eq!(
                observed, expected,
                "cursor={} end={} set={set:?}",
                case.cursor, case.end
            );
            assert_eq!(case.bytes, before, "candidate mutated source bytes");
        }
    }
}

fn time_calls<F>(cases: &[Case], rounds: usize, mut f: F) -> f64
where
    F: FnMut(&Case) -> usize,
{
    let start = Instant::now();
    let mut acc = 0usize;
    for _ in 0..rounds {
        for case in cases {
            acc ^= black_box(f(black_box(case)));
        }
    }
    black_box(acc);
    let calls = (cases.len() * rounds) as f64;
    start.elapsed().as_secs_f64() * 1_000_000_000.0 / calls
}

#[test]
fn ascii_set_member_find_microbench_artifact() {
    let cases = cases();
    let rounds = 4096usize;
    let sample_count = cases.len() * rounds;
    for case in &cases {
        assert_eq!(
            find_candidate(&case.bytes, case.cursor, case.end, DELIMS),
            find_scalar(&case.bytes, case.cursor, case.end, DELIMS)
        );
    }
    let scalar_ns = time_calls(&cases, rounds, |case| {
        find_scalar(&case.bytes, case.cursor, case.end, DELIMS)
    });
    let candidate_ns = time_calls(&cases, rounds, |case| {
        find_candidate(&case.bytes, case.cursor, case.end, DELIMS)
    });
    let ratio = scalar_ns / candidate_ns.max(f64::MIN_POSITIVE);
    let decision = if ratio >= 1.01 { "pass" } else { "reject" };
    println!(
        "skv12-w4 delimiter-find scalar_ns={scalar_ns:.6} candidate_ns={candidate_ns:.6} ratio={ratio:.6} decision={decision}"
    );
    if let Ok(path) = std::env::var("SKV12_W4_MICROBENCH_OUT") {
        if let Some(parent) = std::path::Path::new(&path).parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        let json = format!(
            concat!(
                "{{\n",
                "  \"schema_id\": \"sk-v12-w4-delimiter-find-microbench-v1\",\n",
                "  \"wave_id\": \"SK-V12-W4\",\n",
                "  \"selected_candidate\": \"a64_ascii_set_run_skip\",\n",
                "  \"caller_api\": \"find_ascii_set_member64\",\n",
                "  \"delimiter_set_hex\": \"7b7d3b\",\n",
                "  \"fixture_sha256\": \"{}\",\n",
                "  \"synthetic_windows_sha256\": \"{}\",\n",
                "  \"sample_count\": {},\n",
                "  \"scalar_ns_per_iter\": {:.9},\n",
                "  \"candidate_ns_per_iter\": {:.9},\n",
                "  \"candidate_speedup_ratio\": {:.9},\n",
                "  \"threshold_speedup_ratio\": 1.01,\n",
                "  \"parity_status\": \"pass\",\n",
                "  \"decision\": \"{}\"\n",
                "}}\n"
            ),
            sha256_hex(FIXTURE),
            cases_sha256(&cases),
            sample_count,
            scalar_ns,
            candidate_ns,
            ratio,
            decision
        );
        std::fs::write(path, json).unwrap();
    }
}
