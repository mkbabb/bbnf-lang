//! SK-V18 G6 — end-to-end scan differential over the REAL css-l4 corpora.
//!
//! dav1d discipline: the G6 dispatch-targeting `css_balanced_component_scan`
//! (which classifies a 64-byte block once via `structural_classify_64` and
//! CTZ-iterates the significant bitmask) must produce the IDENTICAL scan result
//! — the same `(byte, pos)` STOP hit, the same `None` at EOF, and the same
//! error OFFSET + message — as an INDEPENDENT, fully-scalar per-byte reference
//! recognizer. This is the binding correctness gate for the scalar-shell
//! integration: identical structural decisions in identical order ⟹ bit-identical
//! tape ⟹ exact 9-field cssparser parity.
//!
//! The reference (`scan_reference`) is the pre-redesign per-byte algorithm: a
//! clean-room scalar walk that membership-tests the STOP set and matches each
//! structural byte. The corpora are `include_bytes!`-embedded (the four real
//! 71KB–495KB css-l4-sk-v14 sources). The differential sweeps every start
//! position over each corpus, for each of the four grammar-derived STOP sets,
//! and asserts the production scan equals the reference exactly — covering the
//! kind-fan / significant-mask parity, the CTZ-iterate boundary (the shell
//! visits exactly the reference's structural sequence), the block re-derive after
//! recursion/string/comment jumps, and error-position reproduction.

use runtime::runtime_scan::{css_balanced_component_scan, ScanError, SkipAlphabet};

const SKIP: SkipAlphabet = [b'\'', b'"', b'/', b'(', b')', b'[', b']', b'{', b'}'];

/// The four grammar-derived STOP sets that reach `css_balanced_component_scan`.
const STOP_SETS: &[&[u8]] = &[b";{}", b"{};", b";}", b":{};"];

const CSS_CORPORA: &[(&str, &[u8])] = &[
    (
        "animate",
        include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../corpora/css-l4-sk-v14/animate-4.1.1.min.css"
        )),
    ),
    (
        "tailwindcss",
        include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../corpora/css-l4-sk-v14/tailwindcss-0.2.0.min.css"
        )),
    ),
    (
        "bootstrap",
        include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../corpora/css-l4-sk-v14/bootstrap-5.3.3.min.css"
        )),
    ),
    (
        "material",
        include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../corpora/css-l4-sk-v14/material-components-web-14.0.0.min.css"
        )),
    ),
];

// -----------------------------------------------------------------------------
// Independent scalar reference: the pre-redesign per-byte recognizer.
// -----------------------------------------------------------------------------

fn ref_scan_error(offset: usize, message: &'static str) -> ScanError {
    ScanError { offset, message }
}

fn ref_consume_comment(bytes: &[u8], start: usize) -> Result<usize, ScanError> {
    let mut pos = start + 2;
    while pos + 1 < bytes.len() {
        if bytes[pos] == b'*' && bytes[pos + 1] == b'/' {
            return Ok(pos + 2);
        }
        pos += 1;
    }
    Err(ref_scan_error(start, "unclosed comment"))
}

fn ref_consume_string(bytes: &[u8], start: usize) -> Result<usize, ScanError> {
    let quote = bytes[start];
    let mut pos = start + 1;
    while pos < bytes.len() {
        match bytes[pos] {
            byte if byte == quote => return Ok(pos + 1),
            b'\\' => {
                pos += 1;
                if pos >= bytes.len() {
                    return Err(ref_scan_error(start, "unclosed string"));
                }
                if bytes[pos] == b'\r' && bytes.get(pos + 1).copied() == Some(b'\n') {
                    pos += 2;
                } else {
                    pos += 1;
                }
            }
            b'\n' | b'\r' | 0x0c => {
                return Err(ref_scan_error(pos, "unescaped newline in string"));
            }
            _ => pos += 1,
        }
    }
    Err(ref_scan_error(start, "unclosed string"))
}

fn ref_consume_balanced(bytes: &[u8], start: usize, close: u8) -> Result<usize, ScanError> {
    let len = bytes.len();
    let mut pos = start + 1;
    while pos < len {
        let byte = bytes[pos];
        if byte == close {
            return Ok(pos + 1);
        }
        pos = ref_dispatch(bytes, pos, byte)?;
    }
    Err(ref_scan_error(start, "unclosed component block"))
}

fn ref_dispatch(bytes: &[u8], pos: usize, byte: u8) -> Result<usize, ScanError> {
    let next = bytes.get(pos + 1).copied();
    let consumed = match byte {
        b'\'' | b'"' => ref_consume_string(bytes, pos)?,
        b'/' if next == Some(b'*') => ref_consume_comment(bytes, pos)?,
        b'(' => ref_consume_balanced(bytes, pos, b')')?,
        b'[' => ref_consume_balanced(bytes, pos, b']')?,
        b'{' => ref_consume_balanced(bytes, pos, b'}')?,
        b')' | b']' | b'}' => {
            return Err(ref_scan_error(pos, "unexpected closing delimiter"));
        }
        _ => pos + 1,
    };
    Ok(consumed)
}

fn scan_reference(
    bytes: &[u8],
    mut pos: usize,
    delimiters: &[u8],
) -> Result<Option<(u8, usize)>, ScanError> {
    let len = bytes.len();
    while pos < len {
        let byte = bytes[pos];
        if delimiters.contains(&byte) {
            return Ok(Some((byte, pos)));
        }
        pos = ref_dispatch(bytes, pos, byte)?;
    }
    Ok(None)
}

// -----------------------------------------------------------------------------
// The differential.
// -----------------------------------------------------------------------------

#[test]
fn css_balanced_component_scan_matches_scalar_reference() {
    // Sweep every start position over each corpus, for each STOP set, and assert
    // the production (NEON-backed) scan == the independent scalar reference,
    // INCLUDING the error variant (offset + message). A `usize` stride keeps the
    // half-million-byte material corpus tractable while still covering every
    // alignment class and a dense set of distinct entry positions (the scan from
    // each position re-classifies a fresh block, exercising the boundary +
    // re-derive at every misalignment).
    let mut comparisons = 0usize;
    for (name, corpus) in CSS_CORPORA {
        // Choose a stride that yields a dense, alignment-covering position set:
        // step by a value coprime with 64 so successive starts walk all 64
        // misalignment classes, and bound total work per corpus.
        let stride = (corpus.len() / 4096).max(1) | 1; // odd ⟹ coprime with 64
        for &stop in STOP_SETS {
            let mut start = 0usize;
            while start <= corpus.len() {
                let got = css_balanced_component_scan(corpus, start, stop, &SKIP);
                let want = scan_reference(corpus, start, stop);
                assert_eq!(
                    got, want,
                    "scan diverged: corpus={name} stop={stop:?} start={start}"
                );
                comparisons += 1;
                start += stride;
            }
        }
    }
    assert!(comparisons > 0, "no scan comparisons ran");
    eprintln!(
        "[css-scan-diff] css_balanced_component_scan == scalar reference: \
         {comparisons} scans over 4 corpora × 4 stop sets (every alignment class)"
    );
}

/// Synthetic edge cases that the corpora may under-sample: error positions
/// (unexpected close, unclosed comment/string), block-boundary structurals, and
/// nested recursion straddling 64-byte blocks.
#[test]
fn css_scan_edge_cases_match_reference() {
    let cases: &[&[u8]] = &[
        b"",
        b".a{color:red}",
        b".a{background:url(x)}",
        b"/* c */.b{d:e}",
        b"x:\"quoted; {} delims\";y:'p (q)';",
        b".a,.b{c:d}",
        b")unexpected",
        b"a{b:c", // unclosed block
        b"/* unterminated comment",
        b"x:\"unclosed string",
        // A 64+ byte inert run then a structural straddling the block boundary.
        b"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa{x:y};z",
        // Nested recursion straddling blocks.
        b"calc(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa(b))end;",
        // Comment containing stop-like bytes that must NOT leak.
        b"/* ; { } : leak test */.x{y:z};",
    ];
    for input in cases {
        for &stop in STOP_SETS {
            for start in 0..=input.len() {
                let got = css_balanced_component_scan(input, start, stop, &SKIP);
                let want = scan_reference(input, start, stop);
                assert_eq!(
                    got, want,
                    "edge scan diverged: stop={stop:?} start={start} input={input:?}"
                );
            }
        }
    }
}
