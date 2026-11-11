//! sk-v27 LW2.CON-2 (S11 BENCH), PREREG §10: `perf3-driver css|json FILE K`.
//!
//! Parses FILE K times in this one fresh process with parse-that's span-parser CSS or JSON
//! parser (`css_parser` / `json_parser`). The timed region is the K parses; each result is
//! `black_box`ed and its top-level length folded into the checksum. After the loop, the
//! sha-free FNV-1a of one result's `Debug` text is folded in (outside the timed region): the
//! two arms run the same parser source apart from one inline attribute, so equal work is the
//! digest's to show, not a per-iteration walk's.
use std::hint::black_box;

use parse_that::state::ParserState;

fn fnv(h: u64, bytes: &[u8]) -> u64 {
    bytes.iter().fold(h, |h, b| (h ^ *b as u64).wrapping_mul(0x100_0000_01b3))
}

fn main() {
    let a: Vec<String> = std::env::args().collect();
    let (kind, path, k) = (a[1].as_str(), &a[2], a[3].parse::<usize>().expect("K"));
    let data = std::fs::read_to_string(path).expect("input");
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    match kind {
        "css" => {
            let p = parse_that::parsers::css::css_parser();
            for _ in 0..k {
                let mut st = ParserState::new(black_box(&data));
                let r = p.call(&mut st).expect("css parse");
                h = fnv(h, &(r.len() as u64).to_le_bytes());
                black_box(r);
            }
            let mut st = ParserState::new(&data);
            h = fnv(h, format!("{:?}", p.call(&mut st)).as_bytes());
        }
        "json" => {
            let p = parse_that::json::json_parser();
            for _ in 0..k {
                let mut st = ParserState::new(black_box(&data));
                let r = p.call(&mut st).expect("json parse");
                h = fnv(h, &[st.offset as u8]);
                black_box(r);
            }
            let mut st = ParserState::new(&data);
            h = fnv(h, format!("{:?}", p.call(&mut st)).as_bytes());
        }
        _ => panic!("kind css|json"),
    }
    println!("checksum={h:016x}");
}
