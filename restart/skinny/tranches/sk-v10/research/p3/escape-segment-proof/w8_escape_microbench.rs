use parse_that_regex::unescape_string;
use std::borrow::Cow;
use std::env;
use std::fs;
use std::hint::black_box;
use std::path::Path;
use std::time::Instant;
const FIXTURES: &[&str] = &["unicode_escapes", "unicode_mixed", "y_string_unicode"];
const SAMPLES: usize = 25;
const TARGET_BYTES: usize = 32 * 1024 * 1024;
struct Slice {
    name: &'static str,
    text: String,
    ranges: Vec<(usize, usize)>,
    bytes: usize,
    rounds: usize,
}
fn main() {
    run_diff_cases();
    let root = env::args().nth(1).unwrap_or_else(|| ".".into());
    println!("run_id=sk-v10-w8-escape-microproof");
    println!("host_triple={}-apple-darwin", env::consts::ARCH);
    println!("build_flags=-C opt-level=3 -C target-cpu=native");
    println!("feature_gate=target_arch=aarch64");
    println!("sample_count={SAMPLES}");
    println!("threshold_speedup=1.08");
    println!("caller=unescape_string -> unescape_four_unicode_escapes");
    println!("scalar_oracle=unescape_uxxxx_scalar + scalar JSON surrogate policy");
    println!("differential_harness=w8_escape_microbench valid/invalid cases + fixture parity");
    println!();
    let mut prod_total = 0.0;
    let mut scalar_total = 0.0;
    for name in FIXTURES {
        let slice = load_slice(Path::new(&root), name);
        verify_slice(&slice);
        let (prod_ns, scalar_ns) = bench(&slice);
        let speedup = if prod_ns == 0.0 { 0.0 } else { scalar_ns / prod_ns };
        if !slice.ranges.is_empty() {
            prod_total += prod_ns;
            scalar_total += scalar_ns;
        }
        println!(
            "slice={} strings={} raw_bytes={} rounds={} bytes_per_sample={} production_ns={:.0} scalar_ns={:.0} speedup={:.3}",
            slice.name,
            slice.ranges.len(),
            slice.bytes,
            slice.rounds,
            slice.bytes * slice.rounds,
            prod_ns,
            scalar_ns,
            speedup
        );
    }
    println!("aggregate_speedup={:.3}", scalar_total / prod_total.max(1.0));
}
fn load_slice(root: &Path, name: &'static str) -> Slice {
    let path = root.join("skinny/test_data").join(format!("{name}.json"));
    let text = fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    let ranges = collect_unicode_strings(&text);
    let bytes = ranges.iter().map(|(start, end)| end - start).sum::<usize>();
    let rounds = if bytes == 0 { 0 } else { (TARGET_BYTES / bytes).max(1) };
    Slice {
        name,
        text,
        ranges,
        bytes,
        rounds,
    }
}
fn collect_unicode_strings(text: &str) -> Vec<(usize, usize)> {
    let bytes = text.as_bytes();
    let mut ranges = Vec::new();
    let mut cursor = 0;
    while cursor < bytes.len() {
        if bytes[cursor] != b'"' {
            cursor += 1;
            continue;
        }
        let start = cursor + 1;
        cursor = start;
        let mut has_unicode = false;
        while cursor < bytes.len() {
            match bytes[cursor] {
                b'\\' => {
                    has_unicode |= bytes.get(cursor + 1) == Some(&b'u');
                    cursor += 2;
                }
                b'"' => {
                    if has_unicode {
                        ranges.push((start, cursor));
                    }
                    cursor += 1;
                    break;
                }
                _ => cursor += 1,
            }
        }
    }
    ranges
}
fn verify_slice(slice: &Slice) {
    for &(start, end) in &slice.ranges {
        assert_same(&slice.text[start..end]);
    }
}
fn run_diff_cases() {
    for raw in [
        r"\u0041",
        r"\u0041\u0042\u0043",
        r"\u0041\u0042\u0043\u0044",
        r"\uD83D\uDE00",
        r"x\u0041y\u0042z",
        r"\u0041x\u0042\u0043\u0044",
        r"\\u0041",
    ] {
        assert_same(raw);
    }
    for raw in [r"\u12XX", r"\uD800", r"\uDE00", r"\uD800\u0041"] {
        assert!(unescape_string(raw).is_err());
        assert!(unescape_scalar(raw).is_err());
    }
}
fn assert_same(raw: &str) {
    let prod = unescape_string(raw).unwrap().into_owned();
    let scalar = unescape_scalar(raw).unwrap().into_owned();
    assert_eq!(prod, scalar, "raw={raw:?}");
}
fn bench(slice: &Slice) -> (f64, f64) {
    if slice.ranges.is_empty() {
        return (0.0, 0.0);
    }
    let mut prod = Vec::with_capacity(SAMPLES);
    let mut scalar = Vec::with_capacity(SAMPLES);
    for sample in 0..SAMPLES {
        if sample % 2 == 0 {
            prod.push(time(slice, false));
            scalar.push(time(slice, true));
        } else {
            scalar.push(time(slice, true));
            prod.push(time(slice, false));
        }
    }
    (mean(&mut prod), mean(&mut scalar))
}
fn time(slice: &Slice, scalar: bool) -> u128 {
    let start = Instant::now();
    let mut checksum = 0usize;
    for _ in 0..slice.rounds {
        for &(start, end) in &slice.ranges {
            let raw = &slice.text[start..end];
            let value = if scalar {
                unescape_scalar(raw).unwrap()
            } else {
                unescape_string(raw).unwrap()
            };
            checksum ^= value.len();
            checksum ^= value.as_bytes().first().copied().unwrap_or(0) as usize;
        }
    }
    black_box(checksum);
    start.elapsed().as_nanos()
}
fn unescape_scalar(raw: &str) -> Result<Cow<'_, str>, ()> {
    let bytes = raw.as_bytes();
    if !bytes.contains(&b'\\') {
        return if bytes.iter().all(|b| *b > 0x1f) {
            Ok(Cow::Borrowed(raw))
        } else {
            Err(())
        };
    }
    let mut out = String::with_capacity(raw.len());
    let mut cursor = 0;
    let mut segment = 0;
    while cursor < bytes.len() {
        while cursor < bytes.len() && !matches!(bytes[cursor], b'\\' | 0x00..=0x1f) {
            cursor += 1;
        }
        if cursor >= bytes.len() {
            break;
        }
        if bytes[cursor] <= 0x1f {
            return Err(());
        }
        if segment < cursor {
            out.push_str(&raw[segment..cursor]);
        }
        let slash = cursor;
        cursor += 1;
        match *bytes.get(cursor).ok_or(())? {
            b'"' => out.push('"'),
            b'\\' => out.push('\\'),
            b'/' => out.push('/'),
            b'b' => out.push('\u{0008}'),
            b'f' => out.push('\u{000c}'),
            b'n' => out.push('\n'),
            b'r' => out.push('\r'),
            b't' => out.push('\t'),
            b'u' => {
                let (ch, next) = decode_unicode(bytes, slash)?;
                out.push(ch);
                cursor = next;
                segment = cursor;
                continue;
            }
            _ => return Err(()),
        }
        cursor += 1;
        segment = cursor;
    }
    if segment < raw.len() {
        out.push_str(&raw[segment..]);
    }
    Ok(Cow::Owned(out))
}
fn decode_unicode(bytes: &[u8], slash: usize) -> Result<(char, usize), ()> {
    if bytes.get(slash) != Some(&b'\\') || bytes.get(slash + 1) != Some(&b'u') {
        return Err(());
    }
    let first = hex_unit(bytes, slash + 2)?;
    let mut cursor = slash + 6;
    let scalar = if (0xd800..=0xdbff).contains(&first) {
        if bytes.get(cursor) != Some(&b'\\') || bytes.get(cursor + 1) != Some(&b'u') {
            return Err(());
        }
        let second = hex_unit(bytes, cursor + 2)?;
        if !(0xdc00..=0xdfff).contains(&second) {
            return Err(());
        }
        cursor += 6;
        0x10000 + (((first as u32 - 0xd800) << 10) | (second as u32 - 0xdc00))
    } else if (0xdc00..=0xdfff).contains(&first) {
        return Err(());
    } else {
        first as u32
    };
    Ok((char::from_u32(scalar).ok_or(())?, cursor))
}
fn hex_unit(bytes: &[u8], start: usize) -> Result<u16, ()> {
    let mut value = 0u16;
    for offset in 0..4 {
        value = (value << 4) | u16::from(hex(*bytes.get(start + offset).ok_or(())?).ok_or(())?);
    }
    Ok(value)
}
fn hex(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        b'A'..=b'F' => Some(byte - b'A' + 10),
        _ => None,
    }
}
fn mean(samples: &mut [u128]) -> f64 {
    samples.sort_unstable();
    let trim = samples.len() / 10;
    let samples = &samples[trim..samples.len() - trim];
    samples.iter().sum::<u128>() as f64 / samples.len() as f64
}
